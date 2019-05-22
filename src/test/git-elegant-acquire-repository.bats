#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-cd
load addons-fake

setup() {
    fake-pass git acquire-repository
    fake-pass git "clone https://github.com/extsoft/elegant-git.git"
    fake-pass git "elegant configure --local"
}

teardown() {
    clean-fake
}

@test "'acquire-repository': raise an error if cloneable URL isn't set" {
  check git-elegant acquire-repository
  [[ "${lines[0]}" =~ "Cloneable URL is not set"  ]]
}

@test "'acquire-repository': clone the repo" {
  check git-elegant acquire-repository https://github.com/extsoft/elegant-git.git
  [ "$status" -eq 0 ]
}
