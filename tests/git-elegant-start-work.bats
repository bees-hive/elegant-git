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

@test "'start-work': works when the remote repository is unavailable" {
    fake-fail "git pull"
    check git-elegant start-work test-feature
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "As the branch can't be pulled, the current local version is used." ]]
}

@test "'start-work': exit code is 45 when branch name isn't set" {
    check git-elegant start-work
    [[ "$status" -eq 45 ]]
}

@test "'start-work': print error message when branch name isn't set" {
    check git-elegant start-work
    [[ "${lines[0]}" =~ "Please give a name for the new branch." ]]
}

@test "'start-work': tracked changes move to a new branch when they are available" {
    repo-non-staged-change "A new line..."
    check git-elegant start-work test-feature
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "stash push" ]]
    [[ "${lines[@]}" =~ "stash pop" ]]
}

@test "'start-work': stash commands don't run when there are no tracked changes" {
    check git-elegant start-work test-feature
    [[ "$status" -eq 0 ]]
    [[ ! "${lines[@]}" =~ "stash push" ]]
    [[ ! "${lines[@]}" =~ "stash pop" ]]
}

@test "'start-work': stash is not applied when it is not found for a given message" {
    fake-fail "git diff-index --quiet HEAD"
    check git-elegant start-work test-feature
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "stash push" ]]
    [[ ! "${lines[@]}" =~ "stash pop" ]]
}

@test "'start-work': stash is applied when the failed command reruns" {
    repo-non-staged-change "A new line..."
    fake-fail "git checkout -b fail"
    git-elegant start-work fail || true
    check git-elegant start-work pass
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git stash pop" ]]
    [[ ! ${lines[@]} =~ "git stash push" ]]
}
