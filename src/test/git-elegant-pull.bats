#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "'pull': current local branch is pulled successfully" {
    fake-pass git "fetch --tags"
    fake-pass git pull

    run git-elegant pull
    [ "$status" -eq 0 ]
}

@test "'pull': provided local branch is pulled successfully" {
    fake-pass git "checkout master"
    fake-pass git "fetch --tags"
    fake-pass git pull

    run git-elegant pull master
    [ "$status" -eq 0 ]
}
