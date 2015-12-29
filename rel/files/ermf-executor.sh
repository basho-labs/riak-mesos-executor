#!/bin/bash

main() {
    echo "Running checks for proper environment:"
    echo "Checking that riak_mesos_executor directory exists"
    [ -d "riak_mesos_executor" ] || exit
    echo "Checking for riak_mesos_executor executable"
    [ -x "riak_mesos_executor/bin/riak_mesos_executor" ] || exit
    echo "Checking if HOME is set..."
    if [ -z "$HOME" ]; then
        echo "Setting HOME to $PWD"...
        export HOME="$PWD"
    fi

    echo "Starting riak_mesos_executor..."
    riak_mesos_executor/bin/riak_mesos_executor console -noinput
}

main "$@"
