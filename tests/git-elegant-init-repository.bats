#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake

setup() {
    fake-pass git init
    fake-pass git "elegant acquire-repository"
}

teardown() {
    clean-fake
}

@test "'init-repository': command is available" {
  check git-elegant init-repository
  [ "$status" -eq 0 ]
}
