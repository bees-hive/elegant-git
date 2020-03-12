#!/usr/bin/env bats -ex

load addons-common
load addons-fake
load addons-repo

setup() {
    repo-new
}

teardown() {
    fake-clean
    repo-clean
}

@test "'start-work': starts work with a given name" {
    check git-elegant start-work test-feature
    [[ ${status} -eq 0 ]]
    [[ $(git rev-parse --abbrev-ref @) == "test-feature" ]]
}

@test "'start-work': updates upsteram branch before start a new work" {
    fake-fail "git pull"
    fake-pass "git rev-parse --abbrev-ref master@{upstream}"
    check git-elegant start-work test-feature
    [[ ${status} -eq 100 ]]
}

@test "'start-work': raises 45 error if work name is not set" {
    check git-elegant start-work
    [[ ${status} -eq 45 ]]
    [[ ${lines[0]} =~ "Please give a name for the new branch." ]]
}

@test "'start-work': starts work from a given branch instead of a default one" {
    repo "git checkout -b custom"
    fake-pass "git rev-parse --abbrev-ref custom@{upstream}"
    fake-pass "git pull"
    check git-elegant start-work
    [[ ${status} -eq 45 ]]
    [[ ${lines[0]} =~ "Please give a name for the new branch." ]]
}

@test "'start-work': moves existing changes to a new branch" {
    repo-non-staged-change "A new line..."
    check git-elegant start-work test-feature
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "stash push" ]]
    [[ ${lines[@]} =~ "stash pop" ]]
}

@test "'start-work': doesn't run stash pipeline if there are no tracked changes" {
    check git-elegant start-work test-feature
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[@]} =~ "stash push" ]]
    [[ ! ${lines[@]} =~ "stash pop" ]]
}

@test "'start-work': doesn't apply a stash when it wasn't found for a given message" {
    fake-fail "git diff-index --quiet HEAD"
    check git-elegant start-work test-feature
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "stash push" ]]
    [[ ! ${lines[@]} =~ "stash pop" ]]
}

@test "'start-work': appies a stash when the failed command reruns" {
    repo-non-staged-change "A new line..."
    fake-fail "git checkout -b fail"
    git-elegant start-work fail || true
    check git-elegant start-work pass
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git stash pop" ]]
    [[ ! ${lines[@]} =~ "git stash push" ]]
}
