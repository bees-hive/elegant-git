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

@test "'push-after-rebase': command is available" {
  check git-elegant push-after-rebase
  [ "$status" -eq 0 ]
}
