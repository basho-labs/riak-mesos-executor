# TODO This should be checking that it exists etc
ERLC=erlc
ESRC=src
EBIN=ebin

ERL_COMPILE_FLAGS += -I include -Werror

TARGET_FILES = $(EBIN)/erl_epmd.beam

all: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)

$(EBIN)/erl_epmd.beam: $(ESRC)/erl_epmd.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

tarball:
	-mkdir -p root/riak/lib/basho-patches
	-mv $(EBIN)/* root/riak/lib/basho-patches/
	-tar zcf riak-patches.tar.gz root/

