#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake

teardown() {
    clean-fake
}

setup() {
    fake-pass git branch *master
    fake-pass git "push -u origin master:master"
}

@test "'push': command is available" {
  run git-elegant push
  [ "$status" -eq 0 ]
}
