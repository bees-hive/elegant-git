#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "'commands': print all available commands" {
  check git-elegant commands
  [ "${lines[0]}" = "feature" ]
  [ "${lines[1]}" = "pull" ]
  [ "${lines[2]}" = "push" ]
  [ "${lines[3]}" = "push-after-rebase" ]
  [ "${lines[4]}" = "rebase" ]
  [ "${lines[5]}" = "init" ]
  [ "${lines[6]}" = "clone" ]
  [ "${lines[7]}" = "add" ]
  [ "${lines[8]}" = "clear-local" ]
  [ "${lines[9]}" = "configure" ]
  [ "${lines[10]}" = "check" ]
  [ ${#lines[@]} -eq 11 ]
}

@test "'commands': default exit code is 0" {
  check git-elegant commands
  [ "$status" -eq 0 ]
}
