#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake

setup() {
    fake-pass git init
    fake-pass git "elegant configure --local"
}

@test "exit code is 0 when run 'git-elegant init'" {
  run git-elegant init
  [ "$status" -eq 0 ]
}
