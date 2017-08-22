#!/usr/bin/env bats

load commons
load fake-read

setup() {
    fake-pass git init
    fake-pass git "config --global user.name" aaa
    fake-pass git "config --global user.email" aaa@aaa.com
    fake-pass git "config --local user.name aaa"
    fake-pass git "config --local user.email aaa@aaa.com"
}

@test "exit code is 0 when run 'git-elegant init'" {
  run git-elegant init
  [ "$status" -eq 0 ]
}
