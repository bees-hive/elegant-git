#!/usr/bin/env bats -ex

load commons
load fake-read
load fake-cd

setup() {
    fake-pass git clone
    fake-pass git "clone https://github.com/extsoft/elegant-git.git"

    fake-pass git "config --global user.name" "UserName"
    fake-pass git "config --global user.email" "UserEmail"

    fake-pass git "config --local core.commentChar"
    fake-pass git "config --local user.name UserName"
    fake-pass git "config --local user.email UserEmail"
}

@test "print message when run 'git-elegant clone'" {
  run git-elegant clone
  [[ "${lines[0]}" =~ "Clonable URL is not set"  ]]
}

@test "exit code is 0 when run 'git-elegant clone'" {
  run git-elegant clone https://github.com/extsoft/elegant-git.git
  [ "$status" -eq 0 ]
}
