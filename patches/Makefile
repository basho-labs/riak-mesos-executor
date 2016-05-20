# TODO This should be checking that it exists etc
ERLC=erlc
ESRC=src
EBIN=ebin

ERL_COMPILE_FLAGS += -I include -Werror

all:
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(ESRC)/erl_epmd.erl

clean:
	rm -f $(EBIN)/*.beam

tarball:
	-mkdir -p root/riak/lib/basho-patches
	-mv $(EBIN)/* root/riak/lib/basho-patches/
	-tar zcf riak-patches.tar.gz root/

