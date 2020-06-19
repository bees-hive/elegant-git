#!/usr/bin/env bash
set -e
# Runs bats tests
# usage: ./script [command name]

all_tests() {
    exec bats --tap --recursive tests
}

some_tests() {
    exec bats --tap $(find tests -type f -name "*${1}*")
}

usage() {
    cat <<MESSAGE
usage: ${BASH_SOURCE[0]} <command>

Available commands:
    help            prints this message
    all_tests       runs all Bats tests
    some_tests      runs Bats tests matchinh a pattern

MESSAGE
}

main() {
    case ${1} in
        all_tests)  all_tests ;;
        some_tests) shift; some_tests "${@}" ;;
        help)       usage ;;
        *)          "${@}" ;;
    esac
}

main "${@}"
