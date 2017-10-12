#!/usr/bin/env bats

load commons
load fake-read

setup() {
    fake-pass git init

    fake-pass git "config --global user.name" "UserName"
    fake-pass git "config --global user.email" "UserEmail"

    fake-pass git "config --local core.commentChar"
    fake-pass git "config --local user.name UserName"
    fake-pass git "config --local user.email UserEmail"
}

@test "exit code is 0 when run 'git-elegant init'" {
  run git-elegant init
  [ "$status" -eq 0 ]
}
