#!/usr/bin/env bats -ex

load commons
load fake-read

setup() {
    fake-pass git "checkout master"
    fake-pass git "fetch --tags"
    fake-pass git pull
}

@test "print message when run 'git-elegant pull'" {
  run git-elegant pull
  [[ "${lines[0]}" =~ "Specify branch name"  ]]
}

@test "exit code is 0 when run 'git-elegant pull'" {
  run git-elegant pull master
  [ "$status" -eq 0 ]
}