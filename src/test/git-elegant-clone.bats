#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-cd
load addons-fake

setup() {
    fake-pass git clone
    fake-pass git "clone https://github.com/extsoft/elegant-git.git"
    fake-pass git "elegant configure --local"
}

@test "print message when run 'git-elegant clone'" {
  run git-elegant clone
  [[ "${lines[0]}" =~ "Clonable URL is not set"  ]]
}

@test "exit code is 0 when run 'git-elegant clone'" {
  run git-elegant clone https://github.com/extsoft/elegant-git.git
  [ "$status" -eq 0 ]
}
