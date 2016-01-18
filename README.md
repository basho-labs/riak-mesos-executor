# riak_mesos_executor
Top-level application forming the erlang-based Mesos Executor for running Riak.

### Building

```
make rel
```

Alternatively `make stage` then `make recompile` when changes are made to `*.erl`.

### Testing

```
make && make test
```

#### Run the riak_mesos_executor application

NB: This is not possible outside of the `mesos-slave` context: libmesos will cause a segfault without the appropriate runtime environment.

For installing/running in a mesos cluster, see the [riak-mesos-tools](https://github.com/basho-labs/riak-mesos-tools) repository

#### Clique Based CLI

Get the status

```
./rel/riak_mesos_executor/bin/riak-mesos_executor-admin status
```

```
{"alive":true}
```
