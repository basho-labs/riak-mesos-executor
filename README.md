# riak_mesos_executor
Top-level application forming the erlang-based Mesos Executor for running Riak.

### Building

```
make rel
```

Alternatively `make stage` then `make recompile` when changes are made to `*.erl`.

### Testing

#### Run the riak_mesos_executor application

```
./rel/riak_mesos_executor/bin/riak_mesos_executor start
```

#### Clique Based CLI

Get the status

```
./rel/riak_mesos_executor/bin/riak-mesos_executor-admin status
```

```
{"alive":true}
```
