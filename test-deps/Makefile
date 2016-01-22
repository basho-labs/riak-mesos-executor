.PHONY: all clean

all: sampler.tar.gz

clean: clean-sampler

clean-sampler:
	-rm sampler.tar.gz

sampler.tar.gz: erlang-app-skeleton erlang-app-skeleton/rel/sampler
	-tar -C erlang-app-skeleton/rel/ -czf sampler.tar.gz sampler || rm -rf sampler.tar.gz

erlang-app-skeleton/rel/sampler: erlang-app-skeleton
	$(MAKE) -C erlang-app-skeleton rel

erlang-app-skeleton:
	git clone https://github.com/drewkerrigan/erlang-app-skeleton
