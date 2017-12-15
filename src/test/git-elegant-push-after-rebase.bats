#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-fake

setup() {
    fake-pass git "elegant rebase"
    fake-pass git "elegant push"
}

teardown() {
    clean-fake
}

@test "exit code is 0 when run 'git-elegant push-after-rebase'" {
  run git-elegant push-after-rebase
  [ "$status" -eq 0 ]
}
