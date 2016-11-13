#
# Debian Packaging
#
PACKAGE := scpf
THIS_INCFILE := $(lastword $(MAKEFILE_LIST))

# REBAR_PROFILE *must* be defined!
$(call assert_nonempty_var,REBAR_PROFILE,q)
$(info $(THIS_INCFILE) is using REBAR_PROFILE=$(REBAR_PROFILE))

RELDIR = $(CURDIR)/_build/$(REBAR_PROFILE)/rel
$(info RELDIR=$(RELDIR))
$(call assert_dir_exists $(RELDIR))

# make package RELEASE=devel|testing|qa|production
RELEASE ?= devel

# SIGN details
#
# These defaults can be overridden as follows:
#
# make package SIGN_KEY_ID=16B9D175 SIGN_FULLNAME='Silent Circle Debian Package Builder' SIGN_EMAIL='build@silentcircle.com'
SIGN_KEY_ID ?= 16B9D175
SIGN_FULLNAME ?= Silent Circle Debian Package Builder
SIGN_EMAIL ?= build@silentcircle.com

DISTDIR = ./distdir
PKG_VERSION := $(shell dpkg-parsechangelog --count 0 | awk '/^Version:/ { print $$2 }')
ERTS_VSN := $(shell ./erts-version)

# Temp files
# mktemp has redundant template spec due to OS X
BUILDDIR := $(shell mktemp -d /tmp/$(PACKAGE)_build.XXXXXXXXXX)
STAGING_DIR := $(shell mktemp -d /tmp/$(PACKAGE)_staging.XXXXXXXXXX)

GIT := $(shell which git)

ifeq ($(GIT),)
$(error git is required for this build but is not on the path)
endif

ifneq ($(wildcard ./.git),)
DCH_COMMENT := $(shell $(GIT) log --oneline -1)
EXTRA_VERSION := $(shell $(GIT) describe --long | sed -re 's/^.*-([0-9]+)-([^-]+)$$/\1.\2/')
else
DCH_COMMENT := No .git dir, probably automated build
EXTRA_VERSION := 0
endif

DATE := $(shell date +'%Y-%m-%d')
DATETIME := $(shell date --utc +'%Y%m%d%H%M%S')
OSNAME := $(shell lsb_release --short --id)
ARCH := $(shell dpkg-architecture -qDEB_BUILD_ARCH)
VERSIONSTRING = $(PACKAGE) ($(PKG_VERSION) $(DATE)) $(OSNAME) $(ARCH)
DCH_VERSION := $(PKG_VERSION)+0~$(DATETIME).$(EXTRA_VERSION)

# Erlang release version
REL_VSN = $(shell cut -f2 -d' ' $(RELDIR)/$(PACKAGE)/releases/start_erl.data)
REL_TARBALL = $(RELDIR)/$(PACKAGE)/$(PACKAGE)-$(REL_VSN).tar.gz

PKG_LIB_DIR := $(DESTDIR)/usr/lib/$(PACKAGE)
PKG_ETC_DIR := $(DESTDIR)/etc/$(PACKAGE)

# install expects a release tarball to have been built
install:
	echo "INSTALL_DIR=$(DESTDIR)" > INSTALL_DIR
	mkdir -p $(PKG_LIB_DIR)
	mkdir -p $(PKG_ETC_DIR)
	mkdir -p $(PKG_ETC_DIR)/certs
	chmod 0755 $(PKG_ETC_DIR)/certs
	cd $(STAGING_DIR) && tar xfz $(REL_TARBALL)
	cp -R $(STAGING_DIR)/lib $(PKG_LIB_DIR)
	cp -R $(STAGING_DIR)/bin $(PKG_LIB_DIR)
	cp -R $(STAGING_DIR)/releases $(PKG_LIB_DIR)
	cp -R $(STAGING_DIR)/$(ERTS_VSN) $(PKG_LIB_DIR)/
	chmod 0755 $(PKG_LIB_DIR)/$(ERTS_VSN)/bin/*
	chmod 0755 $(PKG_LIB_DIR)/bin/mnesia_init
	chmod 0755 $(PKG_LIB_DIR)/bin/nodetool
	chmod 0755 $(PKG_LIB_DIR)/bin/$(PACKAGE)*
	install -m644 MANIFEST $(PKG_LIB_DIR)/MANIFEST.txt
	install -m644 INSTALL_DIR $(PKG_LIB_DIR)/releases/$(REL_VSN)/INSTALL_DIR
	install -m644 $(STAGING_DIR)/bin/start_clean.boot $(PKG_LIB_DIR)/releases/$(REL_VSN)/start_clean.boot
	install -m640 $(STAGING_DIR)/releases/$(REL_VSN)/sys.config $(PKG_ETC_DIR)/sys.config
	install -m640 $(STAGING_DIR)/releases/$(REL_VSN)/vm.args $(PKG_ETC_DIR)/vm.args
	rm -rf $(STAGING_DIR)

# Build unsigned debian package
dev_package: pkgclean
	mkdir -p $(BUILDDIR)/$(PACKAGE)
	cp -Rp . $(BUILDDIR)/$(PACKAGE)/
	export DEBFULLNAME="$(SIGN_FULLNAME)"; \
	export DEBEMAIL="$(SIGN_EMAIL)"; \
	dch --noquery -c $(BUILDDIR)/$(PACKAGE)/debian/changelog \
		--force-distribution \
		--distribution experimental \
		-b -v "$(DCH_VERSION)" "Developer build"
	cd $(BUILDDIR)/$(PACKAGE) && \
		dpkg-buildpackage -d -us -uc
	mkdir -p $(DISTDIR)
	cp -Rp $(BUILDDIR)/* $(DISTDIR)/

# Build signed debian package
# Note that the 'git clone .' will omit any uncommitted
# files.
package: pkgclean
	mkdir -p $(DISTDIR)
	$(GIT) clone . $(DISTDIR)/$(PACKAGE)
	export DEBFULLNAME="$(SIGN_FULLNAME)"; \
	export DEBEMAIL="$(SIGN_EMAIL)"; \
	dch --noquery -c $(DISTDIR)/$(PACKAGE)/debian/changelog \
		--force-distribution \
		--distribution $(RELEASE) \
		-b -v "$(DCH_VERSION)" "$(DCH_COMMENT)"
	cd $(DISTDIR)/$(PACKAGE) && \
		debuild --no-lintian \
		-e REVISION="$(PKG_VERSION)" \
		-e RELEASE="$(RELEASE)" \
		-e VERSIONSTRING="$(VERSIONSTRING)" \
		-e REBAR_PROFILE="$(REBAR_PROFILE)" \
		-b \
		-k$(SIGN_KEY_ID)

pkgclean:
	@rm -rf $(DISTDIR)

include util.mk

# vim: set filetype=make syntax=make noet ts=4 sts=4 sw=4 si:
