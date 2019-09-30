#!/usr/bin/env bash
set -e

THIS="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BIN_DIR="$THIS/../libexec"
export PATH=${BIN_DIR}:$PATH
# By default, all execution should work here.
cd /tmp

check(){
    run "$@"
    echo "> Exit code: \$status=$status"
    local IFS=$'\n'
    echo "> stdout+stderr size: ${#lines[@]}"
    for line in ${lines[@]}; do
        echo "> stdout+stderr: '$line'"
    done
}

testtee() {
    # Prints given command and executes it.
    # It's useful as when a test is failed,
    # then you can see a command in the log.
    #
    # usage: testtee [ags]...
    echo "$(basename ${BASH_SOURCE[0]}): $@"
    eval "$@"
}
