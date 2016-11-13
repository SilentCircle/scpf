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
PKG_VERSION := $(shell dpkg-parsechangelog --count 0 | awk '/^Version:/ { print $$2 }')
DCH_VERSION := $(PKG_VERSION)+0~$(DATETIME).$(EXTRA_VERSION)

DISTDIR = $(CURDIR)/_debian_build

