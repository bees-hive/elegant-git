#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-cd
load addons-fake

setup() {
    fake-pass "git clone-repository"
    fake-pass "git clone https://github.com/extsoft/elegant-git.git"
    fake-pass "git elegant acquire-repository"
}

teardown() {
    fake-clean
}

@test "'clone-repository': raise an error if cloneable URL isn't set" {
  check git-elegant clone-repository
  [[ "${lines[0]}" =~ "Cloneable URL is not set"  ]]
}

@test "'clone-repository': clone the repo" {
  check git-elegant clone-repository https://github.com/extsoft/elegant-git.git
  [ "$status" -eq 0 ]
}
