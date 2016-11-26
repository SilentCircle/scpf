.PHONY: check_dirty check_tags manifest

include util.mk

GIT_PROG=git
GIT=$(call get_prog,GIT_PROG)
GIT_MOD_CMD = diff --quiet
GIT_UC_CMD = diff --cached --quiet

manifest:
	@if [ -d _build/$(REBAR_PROFILE)/lib ]; then \
		{ echo $(pwd)/.git; find -L _build/$(REBAR_PROFILE)/lib -type d -name ".git"; } | \
		while read gd; do \
			if [ -d $$gd ]; then \
			  wd="$$(dirname $$gd)"; \
			  app="$$(basename $$wd)"; \
			  echo "$$app	$$($(GIT) --git-dir="$$gd" --work-tree="$$wd" describe --long --always)"; \
			fi; \
		done | sort; \
	fi

check_dirty:
	@if [ -d _build/$(REBAR_PROFILE)/lib ]; then \
		{ echo $(pwd)/.git; find -L _build/$(REBAR_PROFILE)/lib -type d -name ".git"; } | \
		while read gd; do \
			if [ -d $$gd ]; then \
			  unset dirty; \
			  wt="$$(dirname $$gd)"; \
			  $(GIT) --git-dir="$$gd" --work-tree="$$wt" $(GIT_MOD_CMD) || dirty=y ; \
			  $(GIT) --git-dir="$$gd" --work-tree="$$wt" $(GIT_UC_CMD) || dirty=y; \
			  [ -z "$$dirty" ] || echo "$$wt"; \
			fi; \
		done; \
	fi

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
	@{ if [ -d "./.git" ]; then echo "./.git"; fi; \
		if [ -d _build/$(REBAR_PROFILE)/lib ]; then \
			find -L _build/$(REBAR_PROFILE)/lib -maxdepth 2 -type d -name '.git'; \
		fi; \
	 } | \
		while read gd; do \
			wt="$$(dirname $$gd)"; \
			if [ -r "$$wt/APP_VERSION" ]; then \
				newtag=v$$(cat "$$wt/APP_VERSION"); \
				tags=$$($(GIT) --git-dir="$$gd" --work-tree="$$wt" tag -l); \
				unset skip_tags; \
				for t in $$tags; do \
					if [ "$$t" = "$$newtag" ]; then \
						skip_tags=1; \
						break; \
					fi; \
				done; \
				if [ -z "$$skip_tags" ]; then \
					echo "$(GIT) --git-dir='$$gd' --work-tree='$$wt' tag -am'$$newtag' '$$newtag'"; \
				else \
					echo "# [$$wt] '$$newtag' is already present - skipping"; \
				fi; \
			fi; \
		done


