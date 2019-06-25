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
    fake-pass git commit
}

@test "'save': command is available" {
    check git-elegant save   
    [ "$status" -eq 0 ]
}
