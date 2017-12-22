#!/usr/bin/env bash
set -e

THIS="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BIN_DIR="$THIS/../../src/main"
export PATH=$BIN_DIR:$PATH

check(){
    run "$@"
    echo "> Exit code: \$status=$status"
    local IFS=$'\n'
    echo "> stdout+stderr size: ${#lines[@]}"
    for line in ${lines[@]}; do
        echo "> stdout+stderr: '$line'"
    done
}
