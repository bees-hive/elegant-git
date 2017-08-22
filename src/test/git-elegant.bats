#!/usr/bin/env bats

load commons

@test "print available commands when run 'git-elegant commands'" {
  run git-elegant commands
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
