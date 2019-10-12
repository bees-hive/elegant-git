#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-repo

setup() {
    repo-new
    fake-pass "git fetch"
    fake-pass "git rebase origin/master"
}

teardown() {
    fake-clean
    repo-clean
}

@test "'deliver-work': by default, a name of remote branch is equal to local branch" {
    repo "git checkout -b feature1"
    fake-pass "git push --set-upstream --force origin feature1:feature1"
    check git-elegant deliver-work
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "git push --set-upstream --force origin feature1:feature1" ]]
}

@test "'deliver-work': if branch name passed, a name of remote branch is different to local branch" {
    repo "git checkout -b feature1"
    fake-pass "git push --set-upstream --force origin feature1:feature2"
    check git-elegant deliver-work feature2
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "git push --set-upstream --force origin feature1:feature2" ]]
}

@test "'deliver-work': exit code is 42 when current local branch is master" {
    check git-elegant deliver-work
    [[ "${status}" -eq 42 ]]
    [[ "${lines[1]}" = "== No pushes to 'master' branch. Please read more on https://elegant-git.bees-hive.org ==" ]]
}

@test "'deliver-work': use stash pipe if there are uncommitted changes" {
    repo "git checkout -b feature1"
    repo-non-staged-change "A new line..."
    fake-pass "git push --set-upstream --force origin feature1:feature1"
    check git-elegant deliver-work
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "git stash push" ]]
    [[ "${lines[@]}" =~ "git stash pop" ]]
}

@test "'deliver-work': use existing upstream branch if it is available" {
    repo "git checkout -b some/remote"
    repo "git checkout -b feature1"
    repo "git branch --set-upstream-to some/remote"
    fake-pass "git push --set-upstream --force some feature1:remote"
    check git-elegant deliver-work
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "git push --set-upstream --force some feature1:remote" ]]
}
