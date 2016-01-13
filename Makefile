REPO         ?= riak_mesos_executor

PKG_REVISION    ?= $(shell git describe --tags)
PKG_VERSION	    ?= $(shell git describe --tags | tr - .)
PKG_ID           = riak_mesos_executor-$(PKG_VERSION)
PKG_BUILD        = 1

BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

.PHONY: deps

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
test: cleantest
	$(REBAR)  skip_deps=true eunit
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

##
## Packaging targets
##
.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	mkdir -p package/$(PKG_ID)/priv
	git --git-dir=.git describe --tags >package/$(PKG_ID)/priv/vsn.git
	for dep in package/$(PKG_ID)/deps/*; do \
             echo "Processing dep: $${dep}"; \
             mkdir -p $${dep}/priv; \
             git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git; \
        done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package
