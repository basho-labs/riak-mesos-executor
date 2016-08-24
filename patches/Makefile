# TODO This should be checking that it exists etc
ERLC=erlc
ESRC=src
EBIN=ebin

RIAK_BASE ?= root

ERL_COMPILE_FLAGS += -I include -Werror

.NOTPARALLEL:

all:
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(ESRC)/erl_epmd.erl

clean:
	-rm -f $(EBIN)/*.beam
	-rm -rf sandbox/*

prepare:
	-mkdir -p sandbox/$(RIAK_BASE)/riak/lib/basho-patches
	-mv $(EBIN)/* sandbox/$(RIAK_BASE)/riak/lib/basho-patches/

