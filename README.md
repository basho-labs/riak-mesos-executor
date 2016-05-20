# riak_mesos_executor
The Executor component of the Riak Mesos Framework.

## Usage

See the [RMF](https://github.com/basho-labs/riak-mesos-erlang) README for details on what, how and why to deploy the RMF.

### Development

To build a deployable artifact, simply run the following:

```
make tarball
```

### Testing

```
make && make test
```

#### Run the riak_mesos_executor application

NB: This is not possible outside of the `mesos-slave` context: libmesos will cause a segfault without the appropriate runtime environment.

For installing/running in a mesos cluster, see the [riak-mesos-erlang](https://github.com/basho-labs/riak-mesos-erlang) repository README.
