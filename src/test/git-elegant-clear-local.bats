#!/usr/bin/env bats -ex

load commons
load fake-read

setup() {
    fake-pass git "branch -lvv" "first [gone]"
    fake-pass git "checkout master"
    fake-pass git "fetch --tags"
    fake-pass git pull
    fake-pass git "branch -d first"
}

@test "exit code is 0 when run 'git-elegant clear-local'" {
  run git-elegant clear-local
  [ "$status" -eq 0 ]
}
