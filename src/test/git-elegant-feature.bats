#!/usr/bin/env bats -ex

load addons-common
load addons-fake

setup() {
    fake-pass git "elegant pull master"
    fake-pass git "checkout -b test-feature"
    fake-pass git "stash save elegant-git"
}

teardown() {
    clean-fake
}

@test "'feature': branch with given name is created successfully" {
  run git-elegant feature test-feature
  [ "$status" -eq 0 ]
}

@test "'feature': exit code is 255 when branch name isn't set" {
  run git-elegant feature
  [ "$status" -eq 255 ]
}

@test "'feature': print error message when branch name isn't set" {
  run git-elegant feature
  [[ "${lines[0]}" =~ "Feature name is not set" ]]
}

@test "'feature': use stash for available changes" {
  fake-pass git "stash save elegant-git" "Saved working directory"
  fake-pass git "stash apply stash^{/elegant-git}"
  fake-pass git "stash drop stash@{0}"
  run git-elegant feature test-feature
  [ "$status" -eq 0 ]
}

@test "'feature': ignore stash if there are no changes" {
  fake-pass git "stash save elegant-git" "No local changes to save"
  run git-elegant feature test-feature
  [ "$status" -eq 0 ]
}

@test "'feature': exit code is 100 when stash wasn't applied" {
  fake-pass git "stash save elegant-git" "Saved working directory"
  fake-pass git "stash apply stash^{/elegant-git}"
  fake-fail git "stash drop stash@{0}"
  run git-elegant feature test-feature
  [ "$status" -eq 100 ]
}
