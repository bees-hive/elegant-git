#!/usr/bin/env bats

load addons-common
load addons-fake
load addons-read


teardown() {
    clean-fake
}

setup() {
    fake-pass git "add --interactive"
    fake-pass git "diff --cached --check"
    fake-pass git commit
}

@test "'save-work': command is available" {
    check git-elegant save-work
    [ "$status" -eq 0 ]
}
