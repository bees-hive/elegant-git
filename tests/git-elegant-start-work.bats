#!/usr/bin/env bats -ex

load addons-common
load addons-fake
load addons-git

setup() {
    init-repo
    fake-pass "git pull"
}

teardown() {
    fake-clean
    clean-git
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
    gitrepo "echo stash >> ${FILE_TO_MODIFY}"
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
    gitrepo "echo stash >> ${FILE_TO_MODIFY}"
    fake-fail "git stash pop stash@{0}"
    check git-elegant start-work test-feature
    [[ "$status" -eq 100 ]]
}
