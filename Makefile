REPO            ?= riak-mesos-executor
RELDIR          ?= riak_mesos_executor
PATCHNAME       ?= riak_erlpmd_patches
GIT_TAG_ISH     ?= $(shell git describe --tags)
PKG_VERSION     ?= $(GIT_TAG_ISH)
MAJOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f1)
MINOR           ?= $(shell echo $(PKG_VERSION) | cut -d'.' -f2)
OS_FAMILY       ?= ubuntu
OS_VERSION      ?= 14.04
mesos           ?= 0.28.1
PKGNAME         ?= $(RELDIR)-$(PKG_VERSION)-mesos-$(mesos)-$(OS_FAMILY)-$(OS_VERSION).tar.gz
PATCH_PKGNAME   ?= $(PATCHNAME)-$(PKG_VERSION)-mesos-$(mesos)-$(OS_FAMILY)-$(OS_VERSION).tar.gz
OAUTH_TOKEN     ?= $(shell cat oauth.txt)
GIT_TAG         ?= $(shell git describe --tags --abbrev=0)
RELEASE_ID      ?= $(shell curl -sS https://api.github.com/repos/basho-labs/$(REPO)/releases/tags/$(GIT_TAG)?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print json.load(sys.stdin)["id"]')
DEPLOY_BASE     ?= "https://uploads.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN)&name=$(PKGNAME)"
PATCH_DEPLOY_BASE ?= "https://uploads.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN)&name=$(PATCH_PKGNAME)"
DOWNLOAD_BASE   ?= https://github.com/basho-labs/$(REPO)/releases/download/$(GIT_TAG)/$(PKGNAME)
PATCH_DOWNLOAD_BASE   ?= https://github.com/basho-labs/$(REPO)/releases/download/$(GIT_TAG)/$(PATCH_PKGNAME)

ifeq ($(GIT_TAG_ISH),$(GIT_TAG))
# If these 2 are identical, there have been no commits since the last tag
BUILDING_EXACT_TAG = yes
else
BUILDING_EXACT_TAG = no
endif

BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

ifneq (,$(shell whereis sha256sum | awk '{print $2}';))
SHASUM = sha256sum
else
SHASUM = shasum -a 256
endif

.PHONY: all compile recompile clean clean-deps deps cleantest test test-deps relx rel relclean distclean stage recycle package tarball patches

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
rebar.config.lock:
	$(REBAR) get-deps compile
	$(REBAR) lock-deps
clean-lock:
	-rm rebar.config.lock
lock: clean-lock distclean rebar.config.lock
deps: rebar.config.lock
	$(REBAR) -C rebar.config.lock get-deps
cleantest:
	-rm -rf .eunit/*
	-rm -rf ct_log/*
test: test-deps
	$(REBAR) skip_deps=true ct
rel: relclean compile relx
relx:
	./relx release
relclean:
	-rm -rf _rel/riak_mesos_executor
distclean: clean
	$(REBAR) delete-deps
stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf _rel/riak_mesos_executor/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) _rel/riak_mesos_executor/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf _rel/riak_mesos_executor/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) _rel/riak_mesos_executor/lib;)
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
tarball: rel retarball
retarball: relx patches
	echo "Creating patches/"$(PATCH_PKGNAME)
	tar -C patches -czf $(PATCH_PKGNAME) root/
	mkdir -p packages/
	mv $(PATCH_PKGNAME) packages/
	echo "Creating packages/"$(PKGNAME)
	tar -C _rel -czf $(PKGNAME) $(RELDIR)/
	mv $(PKGNAME) packages/
	cd packages && $(SHASUM) $(PKGNAME) > $(PKGNAME).sha
	cd packages && $(SHASUM) $(PATCH_PKGNAME) > $(PATCH_PKGNAME).sha
	cd packages && echo "$(DOWNLOAD_BASE)" > remote.txt
	cd packages && echo "$(PATCH_DOWNLOAD_BASE)" > patches_remote.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PKGNAME)" > local.txt
	cd packages && echo "$(BASE_DIR)/packages/$(PATCH_PKGNAME)" > patches_local.txt

prball: GIT_SHA = $(shell git log -1 --format='%h')
prball: PR_COMMIT_COUNT = $(shell git log --oneline master.. | wc -l)
prball: PKG_VERSION = PR-$(PULL_REQ)-$(PR_COMMIT_COUNT)-$(GIT_SHA)
prball: PKGNAME = $(RELDIR)-$(PKG_VERSION)-mesos-$(mesos)-$(OS_FAMILY)-$(OS_VERSION).tar.gz
prball: tarball

sync-test:
ifeq (yes,$(BUILDING_EXACT_TAG))
	@echo $(RELEASE_ID)
else
	@echo "Refusing to upload: not an exact tag: "$(GIT_TAG_ISH)
endif

sync:
ifeq (yes,$(BUILDING_EXACT_TAG))
	@echo "Uploading to "$(DOWNLOAD_BASE)
	@cd packages && \
		curl -XPOST -sS -H 'Content-Type: application/gzip' $(DEPLOY_BASE) --data-binary @$(PKGNAME) && \
		curl -XPOST -sS -H 'Content-Type: application/gzip' $(PATCH_DEPLOY_BASE) --data-binary @$(PATCH_PKGNAME) && \
		curl -XPOST -sS -H 'Content-Type: application/octet-stream' $(DEPLOY_BASE).sha --data-binary @$(PKGNAME).sha && \
		curl -XPOST -sS -H 'Content-Type: application/octet-stream' $(PATCH_DEPLOY_BASE).sha --data-binary @$(PATCH_PKGNAME).sha
else
	@echo "Refusing to upload: not an exact tag: "$(GIT_TAG_ISH)
endif

ASSET_ID        ?= $(shell curl -sS https://api.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print "".join([str(asset["id"]) if asset["name"] == "$(PKGNAME)" else "" for asset in json.load(sys.stdin)])')
PATCH_ASSET_ID        ?= $(shell curl -sS https://api.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print "".join([str(asset["id"]) if asset["name"] == "$(PATCH_PKGNAME)" else "" for asset in json.load(sys.stdin)])')
ASSET_SHA_ID    ?= $(shell curl -sS https://api.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print "".join([str(asset["id"]) if asset["name"] == "$(PKGNAME).sha" else "" for asset in json.load(sys.stdin)])')
PATCH_ASSET_SHA_ID    ?= $(shell curl -sS https://api.github.com/repos/basho-labs/$(REPO)/releases/$(RELEASE_ID)/assets?access_token=$(OAUTH_TOKEN) | python -c 'import sys, json; print "".join([str(asset["id"]) if asset["name"] == "$(PATCH_PKGNAME).sha" else "" for asset in json.load(sys.stdin)])')
DELETE_DEPLOY_BASE     ?= "https://api.github.com/repos/basho-labs/$(REPO)/releases/assets/$(ASSET_ID)?access_token=$(OAUTH_TOKEN)"
PATCH_DELETE_DEPLOY_BASE     ?= "https://api.github.com/repos/basho-labs/$(REPO)/releases/assets/$(PATCH_ASSET_ID)?access_token=$(OAUTH_TOKEN)"
DELETE_SHA_DEPLOY_BASE ?= "https://api.github.com/repos/basho-labs/$(REPO)/releases/assets/$(ASSET_SHA_ID)?access_token=$(OAUTH_TOKEN)"
PATCH_DELETE_SHA_DEPLOY_BASE ?= "https://api.github.com/repos/basho-labs/$(REPO)/releases/assets/$(PATCH_ASSET_SHA_ID)?access_token=$(OAUTH_TOKEN)"

sync-delete:
	echo "Deleting "$(DOWNLOAD_BASE)
	- $(shell curl -XDELETE -sS $(DELETE_DEPLOY_BASE))
	- $(shell curl -XDELETE -sS $(PATCH_DELETE_DEPLOY_BASE))
	- $(shell curl -XDELETE -sS $(DELETE_SHA_DEPLOY_BASE))
	- $(shell curl -XDELETE -sS $(PATCH_DELETE_SHA_DEPLOY_BASE))
