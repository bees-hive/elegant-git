#!/usr/bin/env bats -ex

load commons
load fake-read

setup() {
    fake-pass git "checkout master"
    fake-pass git "fetch --tags"
    fake-pass git pull
    fake-pass git "checkout -b test-feature"
}

@test "exit code is 0 when run 'git-elegant feature'" {
  run git-elegant feature test-feature
  [ "$status" -eq 0 ]
}
