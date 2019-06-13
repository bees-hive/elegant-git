#!/usr/bin/env bats

load addons-common
load addons-fake
load addons-read


teardown() {
    clean-fake
}

setup() {
    fake-pass git "add --interactive"
    fake-pass git "elegant check --staged"
    fake-pass git commit --amend
}

@test "'amend': command is available" {
    check git-elegant amend
    [ "$status" -eq 0 ]
}
