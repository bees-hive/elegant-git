#!/usr/bin/env bats

load addons-common
load addons-read

setup() {
    fake-pass git "elegant commands"

    fake-pass git "config --global user.name" "UserName"
    fake-pass git "config --global user.email" "UserEmail"

    fake-pass git "config --global core.commentChar"
    fake-pass git "config --local core.commentChar"

    fake-pass git "config --global user.name UserName"
    fake-pass git "config --global user.email UserEmail"
    fake-pass git "config --local user.name UserName"
    fake-pass git "config --local user.email UserEmail"

    fake-pass git "config --global apply.whitespace fix"
    fake-pass git "config --local apply.whitespace fix"
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
  [ "${lines[0]}" = "commit message won't start with [|]: " ]
  [ "${lines[1]}" = "your user name [UserName]: " ]
  [ "${lines[2]}" = "your user email [UserEmail]: " ]
  [ "${lines[3]}" = "whitespace issues on patching [fix]: " ]
  [ "${lines[4]}" = "add git aliases for all 'elegant git' commands [yes]: " ]
  [ ${#lines[@]} -eq 5 ]
}

@test "'configure': sequence of the configuration is correct when run with '--local' argument" {
  run git-elegant configure --local
  [ "${lines[0]}" = "commit message won't start with [|]: " ]
  [ "${lines[1]}" = "your user name [UserName]: " ]
  [ "${lines[2]}" = "your user email [UserEmail]: " ]
  [ "${lines[3]}" = "whitespace issues on patching [fix]: " ]
  [ ${#lines[@]} -eq 4 ]
}
