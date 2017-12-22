#!/usr/bin/env bats

load addons-common
load addons-fake

setup() {
    fake-pass git "fetch --tags"
    fake-pass git "rebase origin/master"
}

teardown() {
    clean-fake
}

@test "'rebase': command is available" {
    check git-elegant rebase
    [ "$status" -eq 0 ]
}
