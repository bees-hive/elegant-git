#!/usr/bin/env bats

load commons

setup() {
    fake-pass git "fetch --tags"
    fake-pass git "rebase origin/master"
}

@test "exit code is 0 when run 'git-elegant rebase'" {
    run git-elegant rebase
    [ "$status" -eq 0 ]
}
