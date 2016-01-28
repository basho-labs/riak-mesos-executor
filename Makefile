REPO         ?= riak_mesos_executor

PKG_REVISION    ?= $(shell git describe --tags)
PKG_VERSION	    ?= $(shell git describe --tags | tr - .)
PKG_ID           = riak_mesos_executor-$(PKG_VERSION)
PKG_BUILD        = 1

BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

.PHONY: deps test test-deps

all: compile
compile: deps
	$(REBAR) compile
recompile:
	$(REBAR) compile skip_deps=true
clean:
	$(REBAR) clean
clean-deps:
	$(REBAR) -r clean
deps/rebar_lock_deps_plugin/ebin/rebar_lock_deps_plugin.beam:
	$(REBAR) get-deps compile
rebar.config.lock: deps/rebar_lock_deps_plugin/ebin/rebar_lock_deps_plugin.beam
	$(REBAR) lock-deps
deps: rebar.config.lock
	$(REBAR) -C rebar.config.lock get-deps
#upgrade-deps:
# TODO log-changed-deps seems to have a bug
#	$(REBAR) log-changed-deps
force-upgrade-deps:
	# EXPERIMENTAL AND INVASIVE
	rm -rf deps
	$(REBAR) get-deps compile lock-deps
cleantest:
	rm -rf .eunit/*
	rm -rf ct_log/*
test: test-deps
	$(REBAR) skip_deps=true ct
rel: relclean deps compile
	$(REBAR) compile
	$(REBAR) skip_deps=true generate $(OVERLAY_VARS)
relclean:
	rm -rf rel/riak_mesos_executor
distclean: clean
	@./rebar delete-deps
	@rm -rf $(PKG_ID).tar.gz
stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/riak_mesos_executor/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/riak_mesos_executor/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/riak_mesos_executor/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/riak_mesos_executor/lib;)
recycle: relclean clean-deps rel

test-deps:
	$(MAKE) -C test-deps sampler.tar.gz
	-mkdir -p test/rnp_SUITE_data
	-mkdir -p test/rnp_sup_bridge_SUITE_data
	-cp test-deps/sampler.tar.gz test/rnp_SUITE_data/
	-cp test-deps/sampler.tar.gz test/rnp_sup_bridge_SUITE_data/
##
## Packaging targets
##
.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE
package: rel
	-tar -C rel -czf riak_mesos_executor.tar.gz riak_mesos_executor/
