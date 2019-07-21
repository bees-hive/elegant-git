#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "'commands': print all available commands" {
  check git-elegant commands
  [ "${lines[0]}" = "start-work" ]
  [ "${lines[1]}" = "accept-work" ]
  [ "${lines[2]}" = "pull" ]
  [ "${lines[3]}" = "push" ]
  [ "${lines[4]}" = "push-after-rebase" ]
  [ "${lines[5]}" = "rebase" ]
  [ "${lines[6]}" = "init" ]
  [ "${lines[7]}" = "acquire-repository" ]
  [ "${lines[8]}" = "add" ]
  [ "${lines[9]}" = "clear-local" ]
  [ "${lines[10]}" = "configure-repository" ]
  [ "${lines[11]}" = "check" ]
  [ "${lines[12]}" = "save"  ]
  [ ${#lines[@]} -eq 13 ]
}

@test "'commands': default exit code is 0" {
  check git-elegant commands
  [ "$status" -eq 0 ]
}
