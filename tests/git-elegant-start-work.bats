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

@test "'start-work': branch with given name is created successfully" {
  check git-elegant start-work test-feature
  [ "$status" -eq 0 ]
}

@test "'start-work': exit code is 255 when branch name isn't set" {
  check git-elegant start-work
  [ "$status" -eq 255 ]
}

@test "'start-work': print error message when branch name isn't set" {
  check git-elegant start-work
  [[ "${lines[0]}" =~ "Please give a name for the new branch." ]]
}

@test "'start-work': use stash for available changes" {
  fake-pass git "stash save elegant-git" "Saved working directory"
  fake-pass git "stash apply stash^{/elegant-git}"
  fake-pass git "stash drop stash@{0}"
  check git-elegant start-work test-feature
  [ "$status" -eq 0 ]
}

@test "'start-work': ignore stash if there are no changes" {
  fake-pass git "stash save elegant-git" "No local changes to save"
  check git-elegant start-work test-feature
  [ "$status" -eq 0 ]
}

@test "'start-work': exit code is 100 when stash wasn't applied" {
  fake-pass git "stash save elegant-git" "Saved working directory"
  fake-pass git "stash apply stash^{/elegant-git}"
  fake-fail git "stash drop stash@{0}"
  check git-elegant start-work test-feature
  [ "$status" -eq 100 ]
}
