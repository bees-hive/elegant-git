#!/usr/bin/env bats -ex

load addons-common
load addons-fake

setup() {
    fake-pass git "elegant pull master"
    fake-pass git "checkout -b test-feature"
    fake-pass git "stash save elegant-git"
}

@test "exit code is 0 when run 'git-elegant feature test-feature'" {
  run git-elegant feature test-feature
  [ "$status" -eq 0 ]
}

@test "exit code is 255 when run 'git-elegant feature'" {
  run git-elegant feature
  [ "$status" -eq 255 ]
}

@test "print message when run 'git-elegant feature'" {
  run git-elegant feature
  [[ "${lines[0]}" =~ "Feature name is not set" ]]
}

@test "exit code is 0 when run 'git-elegant feature' with changes" {
  fake-pass git "stash save elegant-git" "Saved working directory"
  fake-pass git "stash apply stash^{/elegant-git}"
  fake-pass git "stash drop stash@{0}"
  run git-elegant feature test-feature
  [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant feature' without changes" {
  fake-pass git "stash save elegant-git" "No local changes to save"
  run git-elegant feature test-feature
  [ "$status" -eq 0 ]
}

@test "exit code is 100 when run 'git-elegant feature' with error at 'git stash drop'" {
  fake-pass git "stash save elegant-git" "Saved working directory"
  fake-pass git "stash apply stash^{/elegant-git}"
  fake-fail git "stash drop stash@{0}"
  run git-elegant feature test-feature
  [ "$status" -eq 100 ]
}
