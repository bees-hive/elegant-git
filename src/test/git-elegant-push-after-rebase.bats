#!/usr/bin/env bats -ex

load commons
load fake-read

setup() {
    fake-pass git branch *master
    fake-pass git "push -u origin master:master"
    fake-pass git "fetch --tags"
    fake-pass git "rebase origin/master"
}

@test "exit code is 0 when run 'git-elegant push-after-rebase'" {
  run git-elegant push-after-rebase
  [ "$status" -eq 0 ]
}
