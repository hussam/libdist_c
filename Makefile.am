.PHONY: deps rel

REL := node

# First argument is the path to the library (in the release folder), and the
# second argument is the absolute path to the dependency to be linked to (ln source)
ln_lib_to_dep = rm -rf $(1) && ln -sf $(2) $(1)

all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

##
## Release Targets
##

rel:
	./rebar generate

localrel: rel
	./rel/make_local_rel.sh

stagerel: localrel
	$(foreach dir,deps apps,\
		$(foreach dep,$(wildcard $(dir)/*),\
			$(call ln_lib_to_dep, $(wildcard rel/$(REL)/lib/$(shell basename $(dep))*), $(abspath $(dep)));))

relclean:
	rm -rf rel/$(REL)

##
## Developer Targets
##

devrel:
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/gen_dev/ overlay_vars=dev_vars.config)
	./rel/make_devs.sh

stagedevrel: devrel
	$(foreach dev,$(wildcard dev/dev*),\
		$(foreach dir,deps apps,\
			$(foreach dep,$(wildcard $(dir)/*),\
				$(call ln_lib_to_dep, $(wildcard $(dev)/lib/$(shell basename $(dep))*), $(abspath $(dep)));)))

devclean:
	rm -rf dev/dev*

clean: relclean devclean
	./rebar clean
