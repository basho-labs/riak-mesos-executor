#!/bin/bash

readonly EXECUTORDIR="${PWD}/riak_mesos_executor"
readonly EXECUTOR="${EXECUTORDIR}/bin/riak_mesos_executor"

main() {
    echo "Running checks for proper environment:"
    echo "Checking that riak_mesos_executor directory exists"
    [ -d "${EXECUTORDIR}" ] || exit
    echo "Checking for riak_mesos_executor executable"
    [ -x "${EXECUTOR}" ] || exit
    echo "Checking if HOME is set..."
    if [ -z "${HOME}" ]; then
        echo "Setting HOME to $PWD"...
        export HOME="${PWD}"
    fi

    echo "Starting riak_mesos_executor..."
    "${EXECUTOR}" console -noinput
}

main "$@"
