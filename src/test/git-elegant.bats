#!/usr/bin/env bats -ex

load commons
load fake-read

@test "print available commands when run 'git-elegant'" {
  run git-elegant
  [[ "${lines[0]}" =~ "feature" ]]
  [[ "${lines[1]}" =~ "pull" ]]
  [[ "${lines[2]}" =~ "push" ]]
  [[ "${lines[3]}" =~ "push-after-rebase" ]]
  [[ "${lines[4]}" =~ "rebase" ]]
  [[ "${lines[5]}" =~ "init" ]]
  [[ "${lines[6]}" =~ "clone" ]]
  [[ "${lines[7]}" =~ "add" ]]
  [[ "${lines[8]}" =~ "clear-local" ]]
}


@test "exit code is 10 when run 'git-elegant'" {
  run git-elegant
  echo $status
  [ "$status" -eq 10 ]
}


@test "exit code is 0 when run 'git-elegant commands'" {
  run git-elegant commands
  [ "$status" -eq 0 ]
}


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
