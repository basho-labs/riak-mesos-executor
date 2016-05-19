REPO            ?= riak-mesos-executor
RELDIR          ?= riak_mesos_executor
PKG_VERSION	    ?= $(shell git describe --tags --abbrev=0 | tr - .)
MAJOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f1)
MINOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f2)
ARCH            ?= amd64
OS_FAMILY          ?= ubuntu
OS_VERSION       ?= trusty
PKGNAME         ?= $(RELDIR)-$(PKG_VERSION)-$(OS_FAMILY)-$(OS_VERSION)-$(ARCH).tar.gz
OAUTH_TOKEN     ?= $(shell cat oauth.txt)
RELEASE_ID      ?= $(shell curl --silent https://api.github.com/repos/basho-labs/$(REPO)/releases/tags/$(PKG_VERSION)?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print json.load(sys.stdin)["id"]')
DEPLOY_BASE     ?= "https://uploads.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN)&name=$(PKGNAME)"
DOWNLOAD_BASE   ?= https://github.com/basho-labs/$(REPO)/releases/download/$(PKG_VERSION)/$(PKGNAME)

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
	-mv cepmd_linux_amd64 rel/$(RELDIR)

patches:
	$(MAKE) -C patches clean all tarball

##
## Packaging targets
##
tarball: rel patches
	echo "Creating packages/"$(PKGNAME)
	mkdir -p packages
	echo "$(GIT_REF)" > rel/version
	echo "$(GIT_TAG_VERSION)" >> rel/version
	tar -C rel -czf $(PKGNAME) $(RELDIR)/
	rm rel/version
	mv $(PKGNAME) packages/
	cd packages && $(SHASUM) $(PKGNAME) > $(PKGNAME).sha
	cd packages && echo "$(DOWNLOAD_BASE)" > remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt

sync:
	echo "Uploading to "$(DOWNLOAD_BASE)
	cd packages && \
		curl -XPOST -v -H 'Content-Type: application/gzip' $(DEPLOY_BASE) --data-binary @$(PKGNAME) && \
		curl -XPOST -v -H 'Content-Type: application/octet-stream' $(DEPLOY_BASE).sha --data-binary @$(PKGNAME).sha

ASSET_ID        ?= $(shell curl --silent https://api.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print "".join([str(asset["id"]) if asset["name"] == "$(PKGNAME)" else "" for asset in json.load(sys.stdin)])')
ASSET_SHA_ID    ?= $(shell curl --silent https://api.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print "".join([str(asset["id"]) if asset["name"] == "$(PKGNAME).sha" else "" for asset in json.load(sys.stdin)])')
DELETE_DEPLOY_BASE     ?= "https://api.github.com/repos/basho-labs/$(REPO)/releases/assets/$(ASSET_ID)?access_token=$(OAUTH_TOKEN)"
DELETE_SHA_DEPLOY_BASE ?= "https://api.github.com/repos/basho-labs/$(REPO)/releases/assets/$(ASSET_SHA_ID)?access_token=$(OAUTH_TOKEN)"

sync-delete:
	echo "Deleting "$(DOWNLOAD_BASE)
	- $(shell curl -XDELETE -v $(DELETE_DEPLOY_BASE))
	- $(shell curl -XDELETE -v $(DELETE_SHA_DEPLOY_BASE))
