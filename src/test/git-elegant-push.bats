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

@test "exit code is 0 when run 'git-elegant push'" {
  run git-elegant push
  [ "$status" -eq 0 ]
}
