#
# Debian Packaging
#

PACKAGE ?= $(shell dpkg-parsechangelog --count 0 | awk '/^Source:/ { print $$2 }')

# make package RELEASE=devel|testing|qa|production
RELEASE ?= experimental

# SIGN details
#
# These defaults can be overridden as follows:
#
# make package SIGN_KEY_ID=FFFFFFFF SIGN_FULLNAME='Name of my Debian Package Builder' SIGN_EMAIL='build@example.com'
#SIGN_KEY_ID = FFFFFFFF
#SIGN_FULLNAME = Name of my Debian Package Builder
#SIGN_EMAIL = build@example.com

PACKAGE_RULE := $(if $(SIGN_KEY_ID),package-signed,package-unsigned)

DISTDIR = ./distdir
# mktemp has redundant template spec due to OS X
BUILDDIR := $(shell mktemp -d /tmp/$(PACKAGE)_build.XXXXXXXXXX)
CODENAME := $(shell lsb_release --short --codename)
PKG_VERSION := $(shell dpkg-parsechangelog --count 0 | awk '/^Version:/ { print $$2 }')
DATE := $(shell date +'%Y-%m-%d')
OSNAME := $(shell lsb_release --short --id)
ARCH := $(shell dpkg-architecture -qDEB_BUILD_ARCH)
VERSIONSTRING = $(PACKAGE) ($(PKG_VERSION) $(DATE)) $(OSNAME) $(ARCH)
DATETIME := $(shell date --utc +'%Y%m%d%H%M%S')
GIT := $(shell which git)
DCH_COMMENT := $(if $(GIT),$(shell git log --oneline -1),Probably automated build)
EXTRA_VERSION := $(if $(GIT),$(shell git describe --long | sed -re 's/^.*-([0-9]+)-([^-]+)$$/\1.\2/'),0)
ERTS_VSN := $(shell ./erts-version)
DCH_VERSION := $(PKG_VERSION)+0~$(DATETIME).$(EXTRA_VERSION)
PKG_LIB_DIR := $(DESTDIR)/usr/lib/$(PACKAGE)
PKG_ETC_DIR := $(DESTDIR)/etc/$(PACKAGE)

# install expects the release to have been built
install:
	mkdir -p $(PKG_LIB_DIR)
	mkdir -p $(PKG_ETC_DIR)
	mkdir -p $(PKG_ETC_DIR)/certs
	cp -R rel/scpf/lib $(PKG_LIB_DIR)
	cp -R rel/scpf/releases $(PKG_LIB_DIR)
	cp -R rel/scpf/$(ERTS_VSN) $(PKG_LIB_DIR)/
	find $(DESTDIR) -type d -name ".git" -exec rm -rf {} \; || true
	chmod 0755 $(PKG_LIB_DIR)/$(ERTS_VSN)/bin/*
	chmod 0755 $(PKG_ETC_DIR)/certs
	install -p -D -m 0755 rel/scpf/bin/mnesia_init \
		$(PKG_LIB_DIR)/bin/mnesia_init
	install -p -D -m 0755 rel/scpf/bin/backup_database.escript \
		$(PKG_LIB_DIR)/bin/backup_database.escript
	install -p -D -m 0755 rel/scpf/bin/scpf $(PKG_LIB_DIR)/bin/scpf
	install -m644 MANIFEST $(PKG_LIB_DIR)/MANIFEST.txt
	install -m644 rel/$(PACKAGE)/etc/app.config $(PKG_ETC_DIR)/app.config
	install -m644 rel/$(PACKAGE)/etc/vm.args $(PKG_ETC_DIR)/vm.args

dev_package: pkgclean
	mkdir -p $(BUILDDIR)/$(PACKAGE)
	cp -Rp . $(BUILDDIR)/$(PACKAGE)/
	dch --noquery -c $(BUILDDIR)/$(PACKAGE)/debian/changelog \
		--force-distribution \
		--distribution experimental \
		-b -v "$(DCH_VERSION)" "Developer build"
	cd $(BUILDDIR)/$(PACKAGE) && \
		dpkg-buildpackage -us -uc
	mkdir -p $(DISTDIR)
	cp -Rp $(BUILDDIR)/* $(DISTDIR)/

package: $(PACKAGE_RULE)

ifeq ($(PACKAGE_RULE),package-signed)
package-signed: pkgclean
	mkdir -p $(DISTDIR)
	git clone . $(DISTDIR)/$(PACKAGE)
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
		-b \
		-k$(SIGN_KEY_ID)

endif

ifeq ($(PACKAGE_RULE),package-unsigned)
package-unsigned: pkgclean
	mkdir -p $(DISTDIR)
	git clone . $(DISTDIR)/$(PACKAGE)
	dch --noquery -c $(DISTDIR)/$(PACKAGE)/debian/changelog \
		--force-distribution \
		--distribution $(RELEASE) \
		-b -v "$(DCH_VERSION)" "$(DCH_COMMENT)"
	cd $(DISTDIR)/$(PACKAGE) && \
		dpkg-buildpackage -us -uc
endif

pkgclean:
	@rm -rf $(DISTDIR)

# vim: set filetype=make syntax=make noet ts=4 sts=4 sw=4 si:
