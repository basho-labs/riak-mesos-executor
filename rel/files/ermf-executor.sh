#!/bin/bash

main() {
    echo "Running checks for proper environment:"
    echo "Checking that riak_mesos_executor directory exists"
    [ -d "riak_mesos_executor" ] || exit
    echo "Checking for riak_mesos_executor executable"
    [ -x "riak_mesos_executor/bin/riak_mesos_executor" ] || exit
    echo "Checking for required mesos env vars"
    [ ! -z "$MESOS_EXECUTOR_ID" ] || exit
    echo "Checking if HOME is set..."
    if [ -z "$HOME" ]; then
        export HOME=`eval echo "~$WHOAMI"`
    fi

    echo "Starting riak_mesos_executor..."
    ulimit -n 65536 2>&1 > /dev/null
    riak_mesos_executor/bin/riak_mesos_executor -noinput
}

main "$@"
