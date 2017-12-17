#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake

setup() {
    fake-pass git init
    fake-pass git "elegant configure --local"
}

teardown() {
    clean-fake
}

@test "'init': command is available" {
  run git-elegant init
  [ "$status" -eq 0 ]
}
