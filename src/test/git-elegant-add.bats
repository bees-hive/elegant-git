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

@test "exit code is 0 when run 'git-elegant add'" {
    run git-elegant add
    [ "$status" -eq 0 ]
}
