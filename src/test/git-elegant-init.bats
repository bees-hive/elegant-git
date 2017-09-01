#!/usr/bin/env bats

load commons
load fake-read

setup() {
    fake-pass git init
    fake-pass git "elegant configure --local"
}

@test "exit code is 0 when run 'git-elegant init'" {
  run git-elegant init
  [ "$status" -eq 0 ]
}
