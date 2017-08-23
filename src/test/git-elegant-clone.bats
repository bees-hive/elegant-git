#!/usr/bin/env bats -ex

load commons
load fake-read
load fake-cd

setup() {
    fake-pass git clone
    fake-pass git "clone https://github.com/extsoft/elegant-git.git"
    fake-pass git "config --global user.name" aaa
    fake-pass git "config --global user.email" aaa@aaa.com
    fake-pass git "config --local user.name aaa"
    fake-pass git "config --local user.email aaa@aaa.com"
}

@test "print message when run 'git-elegant clone'" {
  run git-elegant clone
  [[ "${lines[0]}" =~ "Specify URL to clone"  ]]
}

@test "exit code is 0 when run 'git-elegant clone'" {
  run git-elegant clone https://github.com/extsoft/elegant-git.git
  [ "$status" -eq 0 ]
}
