#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "exit code is 0 when run 'git-elegant pull' without parameters" {
    fake-pass git "fetch --tags"
    fake-pass git pull

    run git-elegant pull
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant pull' with parameter" {
    fake-pass git "checkout master"
    fake-pass git "fetch --tags"
    fake-pass git pull

    run git-elegant pull master
    [ "$status" -eq 0 ]
}
