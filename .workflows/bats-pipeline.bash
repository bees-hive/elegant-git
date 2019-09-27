#!/usr/bin/env bash
set -e
# Runs bats tests
# usage: ./script [command name]

if [[ -n "${1}" ]]; then
    bats --tap $(find tests -type f -name "*${1}*")
else
    bats --tap tests
fi
