REPO = scpf
APP_VERSION := $(shell cat APP_VERSION)

.PHONY: all build_plt check_dirty check_plt check_tags clean \
		cleanplt compile ct deps dev_package dialyzer distclean doc \
		docclean install manifest package pkgclean realclean \
		relclean scpf update-deps update_locked_config

# The release branch should have a file named USE_REBAR_LOCKED
use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif

# Local rebar is needed because fakeroot build on Debian does not have the
# current user's PATH
REBAR_PGM := $(shell which rebar || echo ./rebar)
REBAR = $(REBAR_PGM) -C $(rebar_config)
GIT_MOD_CMD = diff --quiet
GIT_UC_CMD = diff --cached --quiet

MARKDOWN := $(shell which markdown)
PANDOC := $(shell which pandoc)
MARKDOWN_PGM := $(if $(PANDOC),$(PANDOC),$(MARKDOWN))

all: scpf

scpf: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

check_dirty:
	@git $(GIT_MOD_CMD) || dirty=y ; \
	git $(GIT_UC_CMD) || dirty=y ; \
	[ -z "$$dirty" ] || pwd
	@find deps -type d -name ".git" | \
		while read gd; do \
			unset dirty; \
			wt="$$(dirname $$gd)"; \
			git --git-dir="$$gd" --work-tree="$$wt" $(GIT_MOD_CMD) || dirty=y ; \
			git --git-dir="$$gd" --work-tree="$$wt" $(GIT_UC_CMD) || dirty=y; \
			[ -z "$$dirty" ] || echo "$$wt"; \
		done

# Generates a list of git tag commands required to tag
# repos under the current and deps directories with the contents of the
# APP_VERSION file. It skips directories that don't contain
# APP_VERSION. It doesn't actually do the tag or the push,
# but something like this would help:
#
# make check_tags | sh
#
# and then push them up individually.
check_tags:
	@{ echo "./.git"; find deps -maxdepth 2 -type d -name '.git'; } | \
		while read gd; do \
			wt="$$(dirname $$gd)"; \
			if [ -r "$$wt/APP_VERSION" ]; then \
				newtag=v$$(cat "$$wt/APP_VERSION"); \
				tags=$$(git --git-dir="$$gd" --work-tree="$$wt" tag -l); \
				unset skip_tags; \
				for t in $$tags; do \
					if [ "$$t" = "$$newtag" ]; then \
						skip_tags=1; \
						break; \
					fi; \
				done; \
				if [ -z "$$skip_tags" ]; then \
					echo "git --git-dir='$$gd' --work-tree='$$wt' tag -am'$$newtag' '$$newtag'"; \
				else \
					echo "# [$$wt] '$$newtag' is already present - skipping"; \
				fi; \
			fi; \
		done

retag:
manifest:
	@find deps -type d -name ".git" | \
	while read gd; do \
		wd="$$(dirname $$gd)"; \
		app="$$(basename $$wd)"; \
		echo "$$app	$$(git --git-dir="$$gd" --work-tree="$$wd" describe --long --always)"; \
	done | sort

ct:
	@echo Sorry, tests are broken until the APNS cert issue is fixed.

rel: deps
	sed -e 's/{{APP_VERSION}}/$(APP_VERSION)/g' rel/reltool.config.src > rel/reltool.config
	$(REBAR) -f compile generate

update_locked_config: compile
	@$(REBAR) lock-deps

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   	xmerl webtool snmp public_key mnesia eunit common_test syntax_tools compiler
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

check_plt: scpf
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

build_plt: scpf
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: scpf
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) \
		`find deps -type d | egrep '(apns_erl|gcm_erl|sc_).*/ebin$$'`

cleanplt:
	@echo
	@echo "Are you sure?  It takes a while to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

clean: docclean
	$(REBAR) clean skip_apps=gcm_sim_top

relclean:
	rm -rf rel/scpf

distclean: clean relclean pkgclean
	$(REBAR) delete-deps

realclean: distclean docclean
	@rm -rf deps logs .test

docclean:
	@rm -rf doc/api

doc: scpf
	@mkdir -p doc/api
	echo '@doc' > doc/api/overview.edoc
	$(MARKDOWN_PGM) doc/README.md >> doc/api/overview.edoc
	escript gendocs.escript
	rm -f doc/api/overview.edoc

ifneq ("$(PROGRAMFILES)$(ProgramFiles)","")
OS := Windows
else
OS := $(shell lsb_release --short --id)
endif

ifeq ("$(OS)","Debian")
include Debian.inc
endif

