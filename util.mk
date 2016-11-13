# $1 = Makefile variable name
# $2 = anything - suppress return value, suggest using $(call assert_nonempty_var,X,q)
assert_nonempty_var = $(if $(value $(1)),$(if $(2),,$(value $(1))),$(error Expected nonempty variable `$(1)`))

# $1 = executable name $2 = variable name to display on error
assert_on_path = $(if $(shell which $(1)),$(shell which $(1)),$(error Not on PATH: `$(1)` [$(2)]))

assert_dir_exists = $(if $(shell if test -d $(1); then echo yes; fi),,$(error Directory $(1) does not exist))

get_prog = $(call assert_on_path,$(call assert_nonempty_var,$(1)),$(1))
