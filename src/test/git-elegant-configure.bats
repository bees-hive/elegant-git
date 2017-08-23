#!/usr/bin/env bats

load commons
load fake-read

setup() {
    fake-pass git "config --global core.commentChar" "|"
    fake-pass git "config --local core.commentChar" "|"
}

@test "'configure': exit code is 11 when run without arguments" {
  run git-elegant configure
  [ "$status" -eq 11 ]
}


@test "'configure': exit code is 0 when run with '--global' argument" {
  run git-elegant configure --global
  [ "$status" -eq 0 ]
}

@test "'configure': exit code is 0 when run with '--local' argument" {
  run git-elegant configure --local
  [ "$status" -eq 0 ]
}

@test "'configure': sequence of the configuration is correct when run with '--global' argument" {
  run git-elegant configure --global
  [[ "${lines[0]}" =~ "commit message won't start with [|]:" ]]
  [ ${#lines[@]} == 1 ]
}

@test "'configure': sequence of the configuration is correct when run with '--local' argument" {
  run git-elegant configure --local
  [[ "${lines[0]}" =~ "commit message won't start with [|]:" ]]
  [ ${#lines[@]} == 1 ]
}