REPO            ?= riak-mesos-executor
RELDIR          ?= riak_mesos_executor
PATCHNAME       ?= riak_erlpmd_patches
GIT_REF         ?= $(shell git describe --all)
GIT_TAG_VERSION ?= $(shell git describe --tags)
PKG_VERSION	    ?= $(shell git describe --tags --abbrev=0 | tr - .)
GIT_TAG         ?= $(shell git describe --tags --abbrev=0)
MAJOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f1)
MINOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f2)
ARCH            ?= amd64
OS_FAMILY          ?= ubuntu
OS_VERSION       ?= trusty
PKGNAME         ?= $(RELDIR)-$(PKG_VERSION)-$(OS_FAMILY)-$(OS_VERSION)-$(ARCH).tar.gz
PATCH_PKGNAME   ?= $(PATCHNAME)-$(PKG_VERSION)-$(OS_FAMILY)-$(OS_VERSION)-$(ARCH).tar.gz
OAUTH_TOKEN     ?= $(shell cat oauth.txt)
RELEASE_ID      ?= $(shell curl --silent https://api.github.com/repos/basho-labs/$(REPO)/releases/tags/$(GIT_TAG)?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print json.load(sys.stdin)["id"]')
	# TODO expand this to also include patches .tgz
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

.PHONY: all compile recompile clean clean-deps deps cleantest test test-deps rel relclean distclean stage recycle package tarball patches

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
	$(REBAR) -C rebar.config.lock get-deps
	$(REBAR) compile
rebar.config.lock: deps/rebar_lock_deps_plugin/ebin/rebar_lock_deps_plugin.beam
	$(REBAR) lock-deps
deps: rebar.config.lock
	$(REBAR) -C rebar.config.lock get-deps
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

patches:
	$(MAKE) -C patches clean all prepare

##
## Packaging targets
##
tarball: rel patches
	echo "Creating patches/"$(PATCH_PKGNAME)
	tar -C patches -czf $(PATCH_PKGNAME) root/
	mv $(PATCH_PKGNAME) packages/
	echo "Creating packages/"$(PKGNAME)
	mkdir -p packages
	echo "$(GIT_REF)" > rel/version
	echo "$(GIT_TAG_VERSION)" >> rel/version
	tar -C rel -czf $(PKGNAME) version $(RELDIR)/
	rm rel/version
	mv $(PKGNAME) packages/
	cd packages && $(SHASUM) $(PKGNAME) > $(PKGNAME).sha
	cd packages && $(SHASUM) $(PATCH_PKGNAME) > $(PATCH_PKGNAME).sha
	cd packages && echo "$(DOWNLOAD_BASE)" > remote.txt
	# TODO Create PATCHES_DOWNLOAD_BASE var
	#cd packages && echo "$(DOWNLOAD_BASE)" > patches_remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PATCH_PKGNAME)" > patches_local.txt

sync-test:
	echo $(PKG_VERSION)
	echo $(RELEASE_ID)

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
