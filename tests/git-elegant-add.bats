#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake

teardown() {
    clean-fake
}

setup() {
    fake-pass git "ls-files -m" src/test/git-elegant
    fake-pass git "add src/test/git-elegant"
    fake-pass git status
}

@test "'add': successful adding of modified file" {
    check git-elegant add
    [ "$status" -eq 0 ]
}
