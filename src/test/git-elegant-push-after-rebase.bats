#!/usr/bin/env bats -ex

load commons
load fake-read

setup() {
    fake-pass git "elegant rebase"
    fake-pass git "elegant push"
}

@test "exit code is 0 when run 'git-elegant push-after-rebase'" {
  run git-elegant push-after-rebase
  [ "$status" -eq 0 ]
}
