REPO            ?= riak_mesos_executor
PKG_VERSION	    ?= $(shell git describe --tags --abbrev=0 | tr - .)
MAJOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f1)
MINOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f2)
ARCH            ?= amd64
OSNAME          ?= ubuntu
OSVERSION       ?= trusty
S3_BASE         ?= riak-tools
S3_PREFIX       ?= http://$(S3_BASE).s3.amazonaws.com/
DEPLOY_BASE     ?= $(REPO)/$(MAJOR).$(MINOR)/$(PKG_VERSION)/$(OSNAME)/$(OSVERSION)/
PKGNAME         ?= $(REPO)-$(PKG_VERSION)-$(ARCH).tar.gz

BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

ifneq (,$(shell whereis sha256sum | awk '{print $2}';))
SHASUM = sha256sum
else
SHASUM = shasum -a 256
endif

.PHONY: all compile recompile clean clean-deps deps force-upgrade-deps cleantest test rel relclean distclean stage recycle package tarball patches

all: compile
compile: deps
	$(REBAR) compile
recompile:
	$(REBAR) compile skip_deps=true
clean: cleantest relclean
	$(REBAR) clean
	-rm -rf packages
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

cepmd:
	-curl -O http://riak-tools.s3.amazonaws.com/riak-mesos/cepmd_linux_amd64
	-mv cepmd_linux_amd64 rel/$(REPO)

patches:
	$(MAKE) -C patches clean all tarball

##
## Packaging targets
##
tarball: rel patches
	echo "Creating packages/"$(PKGNAME)
	mkdir -p packages
	tar -C rel -czf $(PKGNAME) $(REPO)/
	mv $(PKGNAME) packages/
	cd packages && $(SHASUM) $(PKGNAME) > $(PKGNAME).sha
	cd packages && echo "$(S3_PREFIX)$(DEPLOY_BASE)$(PKGNAME)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt
sync:
	echo "Uploading to "$(DEPLOY_BASE)
	cd packages && \
		s3cmd put --acl-public $(PKGNAME) s3://$(S3_BASE)/$(DEPLOY_BASE) && \
		s3cmd put --acl-public $(PKGNAME).sha s3://$(S3_BASE)/$(DEPLOY_BASE)
