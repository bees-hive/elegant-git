#!/usr/bin/env bats -ex

load addons-common
load addons-fake
load addons-repo

setup() {
    repo-new
    fake-pass "git pull"
}

teardown() {
    fake-clean
    repo-clean
}

@test "'start-work': branch with given name is created successfully" {
    check git-elegant start-work test-feature
    [[ "$status" -eq 0 ]]
}

@test "'start-work': exit code is 45 when branch name isn't set" {
    check git-elegant start-work
    [[ "$status" -eq 45 ]]
}

@test "'start-work': print error message when branch name isn't set" {
    check git-elegant start-work
    [[ "${lines[0]}" =~ "Please give a name for the new branch." ]]
}

@test "'start-work': use stash pipe if there are uncommitted changes" {
    repo-non-staged-change "A new line..."
    check git-elegant start-work test-feature
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "stash push" ]]
    [[ "${lines[@]}" =~ "stash pop" ]]
}

@test "'start-work': ignore stash pipe if there are uncommitted changes" {
    fake-pass "git stash save elegant-git" "No local changes to save"
    check git-elegant start-work test-feature
    [ "$status" -eq 0 ]
}

@test "'start-work': exit code is 100 when stash pipe wasn't applied" {
    repo-non-staged-change "A new line..."
    fake-fail "git stash pop stash@{0}"
    check git-elegant start-work test-feature
    [[ "$status" -eq 100 ]]
}
