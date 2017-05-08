#----------------------------------------------------------------------------
# This Makefile works with rebar3 and the profiles that rebar3 supports. This
# makefile will run with the 'default' profile unless REBAR_PROFILE is
# provided, e.g. in bash,
#
# make rel REBAR_PROFILE=prod
#----------------------------------------------------------------------------
.PHONY: all clean compile ct dev_rel dev_package \
		dialyzer distclean doc docclean install help info \
		profiles prod_rel rel relclean run vsn

PACKAGE := scpf
PKG_LIB_DIR := $(DESTDIR)/usr/lib/$(PACKAGE)
PKG_ETC_DIR := $(DESTDIR)/etc/$(PACKAGE)

PROD_REL_DIR := ./_build/prod/rel/$(PACKAGE)
REL_VSN = $(shell cut -f2 -d' ' $(PROD_REL_DIR)/releases/start_erl.data)

REBAR_PROFILE ?= default
EDOWN_TARGET ?= github
EDOWN_TOP_LEVEL_README_URL ?= http://github.com/SilentCircle/scpf
TEST_SPEC_NAME := scpf.test.spec

THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

$(info $(THIS_MAKEFILE) is using REBAR_PROFILE=$(REBAR_PROFILE))

REBAR3_URL = https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR),)
REBAR = $(CURDIR)/rebar3
endif

all: vsn compile

# PKG_VERSION is based on the value in debian/changelog.
# Example: "2.0.3"
# This is what should be used for documentation versions.
#
# DCH_VERSION is the version that adds a timestamp and `git describe`
# value to the PKG_VERSION.
# Example: "2.0.3~rc3+0~20170420142017.108+jessie"
#
# This should be used to version scpf in the packaging system. That is,
# it should be the output of `dpkg -l scpf` once it is installed. The
# reason this numbering scheme is used is that it is Debian-compliant,
# and compares correctly when used with `dpkg --compare-versions`, so
# upgrades will be done correctly even if the base version numbers don't
# change.
include pkg.mk

ifeq ($(wildcard APP_VERSION),APP_VERSION)
APP_VERSION = $(shell cat APP_VERSION)
else
APP_VERSION = $(DCH_VERSION)
endif

MARKDOWN_PGM := pandoc

vsn:
	@if [ ! -f APP_VERSION ]; then echo $(APP_VERSION) > APP_VERSION; fi

help:
	@echo
	@echo 'Usage: make <target> [REBAR_PROFILE=<profile>]'
	@echo
	@echo 'This Makefile uses rebar3 profiles.'
	@echo 'Omitting a profile uses the "default" profile.'
	@echo 'Run "make profiles" to see which profiles are available.'

profiles:
	@erl -noinput \
		 -eval \
		 '{ok,C}=file:consult("rebar.config"),Ps=[P||{P,_}<-proplists:get_value(profiles,C)],io:format("Profiles: ~p~n",[Ps]),init:stop().'

info:
	@echo PKG_VERSION=$(PKG_VERSION)
	@echo APP_VERSION=$(APP_VERSION)
	@echo MARKDOWN_PGM=$(call get_prog,MARKDOWN_PGM)
	@echo DCH_COMMENT=$(DCH_COMMENT)
	@echo EXTRA_VERSION=$(EXTRA_VERSION)

compile: $(REBAR)
	@$(REBAR) do clean, compile

# The idea here is to generate a set of fake certs, copy them to the
# simulators, run the simulators in the background, and start SCPF
# to point to the simulators so that it can run without needing real
# certs or an internet connection.
run: $(REBAR)
	$(REBAR) as shell do shell --name scpf@127.0.0.1 --setcookie scpf

dev_rel: $(REBAR) manpage
	@echo Building version $(APP_VERSION)
	@$(REBAR) as dev do clean, release

prod_rel: $(REBAR) manpage
	@echo Building version $(APP_VERSION)
	@$(REBAR) as prod do clean, release

rel: $(REBAR) manpage
	@echo Building version $(APP_VERSION)
	@$(REBAR) do clean, release

tar: $(REBAR) manpage
	@$(REBAR) do clean, tar

ct: $(REBAR)
	echo > $(TEST_SPEC_NAME)
	$(REBAR) do clean, ct --spec $(TEST_SPEC_NAME) --name ct1_scpf --setcookie scpf

dialyzer: $(REBAR)
	@$(REBAR) dialyzer

doc: $(REBAR) compile manpage
	@sed -r -f markedoc.sed	doc/README-src.md > doc/overview.edoc
	$(REBAR) edoc EDOWN_TARGET=$(EDOWN_TARGET) EDOWN_TOP_LEVEL_README_URL=$(EDOWN_TOP_LEVEL_README_URL)

manpage: doc/man/scpf.1

doc/man/scpf.1: doc/man/README doc/man/scpf.1.template
    # Capitalize the headings before passing to pandoc
	awk '/^# / { print toupper($$0); next } { print }' $< | \
		pandoc -t man -s \
		--data-dir=$(CURDIR) \
		--template doc/man/scpf.1.template \
		--variable version="scpf $(PKG_VERSION)" \
		--variable date="$(shell date -u)" \
		-o $@

clean: $(REBAR) docclean
	@$(REBAR) clean
	@rm -f *.crashdump

relclean: clean pkgclean
	@rm -rf _build/$(REBAR_PROFILE)/rel

distclean: clean pkgclean
	@rm -rf _build log logs ebin .test .rebar certs
	@rm -f test/*.beam
	@rm -rf Mnesia.*/
	@rm -rf tools/
	@rm -f *.log

docclean:
	@rm -rf doc/man/scpf.1

install: prod_rel
	@if [ ! -d $(PROD_REL_DIR) ]; then \
		echo "Production release directory missing, aborting"; \
		exit 1; \
	fi
	echo "INSTALL_DIR=$(DESTDIR)" > INSTALL_DIR
	mkdir -p $(PKG_LIB_DIR)
	mkdir -p $(PKG_ETC_DIR)
	mkdir -p $(PKG_ETC_DIR)/certs
	chmod 0755 $(PKG_ETC_DIR)/certs
	cp -R $(PROD_REL_DIR)/lib $(PKG_LIB_DIR)
	cp -R $(PROD_REL_DIR)/bin $(PKG_LIB_DIR)
	cp -R $(PROD_REL_DIR)/releases $(PKG_LIB_DIR)
	cp -R $(PROD_REL_DIR)/$(ERTS_VSN) $(PKG_LIB_DIR)/
	chmod 0755 $(PKG_LIB_DIR)/$(ERTS_VSN)/bin/*
	chmod 0755 $(PKG_LIB_DIR)/bin/mnesia_init
	chmod 0755 $(PKG_LIB_DIR)/bin/nodetool
	chmod 0755 $(PKG_LIB_DIR)/bin/$(PACKAGE)*
	install -m644 MANIFEST $(PKG_LIB_DIR)/MANIFEST.txt
	install -m644 INSTALL_DIR $(PKG_LIB_DIR)/releases/$(REL_VSN)/INSTALL_DIR
	install -m644 $(PROD_REL_DIR)/bin/start_clean.boot $(PKG_LIB_DIR)/releases/$(REL_VSN)/start_clean.boot
	install -m640 $(PROD_REL_DIR)/releases/$(REL_VSN)/sys.config $(PKG_ETC_DIR)/sys.config
	install -m640 $(PROD_REL_DIR)/releases/$(REL_VSN)/vm.args $(PKG_ETC_DIR)/vm.args

# Build unsigned debian package
dev_package: pkgclean
	BUILDDIR=$$(mktemp -d /tmp/$(PACKAGE)_build.XXXXXXXXXX) && \
	(mkdir -p $$BUILDDIR/$(PACKAGE); \
	cp -Rp . $$BUILDDIR/$(PACKAGE)/; \
	export DEBFULLNAME="$$(git config --get user.name)"; \
	export DEBEMAIL="$$(git config --get user.email)"; \
	dch --noquery -c $$BUILDDIR/$(PACKAGE)/debian/changelog \
		--force-distribution \
		--distribution experimental \
		-b -v "$(DCH_VERSION)" "Developer build"; \
	cd $$BUILDDIR/$(PACKAGE) && \
		dpkg-buildpackage -d -us -uc && cd -; \
	mkdir -p $(DISTDIR); \
	cp -Rp $$BUILDDIR/* $(DISTDIR)/)

pkgclean:
	@dh_clean
	@rm -rf $(DISTDIR)

$(REBAR):
	curl -s -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x $(REBAR)

# vim: set filetype=make syntax=make noet ts=4 sts=4 sw=4 si:
