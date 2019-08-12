#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-fake

setup() {
    fake-pass git "branch -lvv" "first [gone]"
    fake-pass git "elegant obtain-work master"
    fake-pass git "branch -d first"
}

teardown() {
    clean-fake
}

@test "'clear-local': command is available" {
  check git-elegant clear-local
  [ "$status" -eq 0 ]
}
