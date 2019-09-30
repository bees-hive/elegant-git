#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-git

setup() {
    init-repo
}

teardown() {
    fake-clean
    clean-git
}

@test "'deliver-work': by default, a name of remote branch is equal to local branch" {
    fake-pass "git rev-parse --abbrev-ref HEAD" feature1
    fake-pass "git fetch"
    fake-pass "git rebase origin/master"
    fake-pass "git push --set-upstream --force origin feature1:feature1"
    check git-elegant deliver-work
    [ "$status" -eq 0 ]
    [[ "${lines[@]}" =~ "git push --set-upstream --force origin feature1:feature1" ]]
}

@test "'deliver-work': if branch name passed, a name of remote branch is different to local branch" {
    fake-pass "git rev-parse --abbrev-ref HEAD" feature1
    fake-pass "git fetch"
    fake-pass "git rebase origin/master"
    fake-pass "git push --set-upstream --force origin feature1:feature2"
    check git-elegant deliver-work feature2
    [ "$status" -eq 0 ]
    [[ "${lines[@]}" =~ "git push --set-upstream --force origin feature1:feature2" ]]
}

@test "'deliver-work': exit code is 42 when current local branch is master" {
    fake-pass "git rev-parse --abbrev-ref HEAD" master
    fake-pass "git fetch"
    fake-pass "git rebase origin/master"
    fake-pass "git push --set-upstream --force origin master:master"
    check git-elegant deliver-work
    [ "$status" -eq 42 ]
    [ "${lines[1]}" = "== No pushes to 'master' branch. Please read more on https://elegant-git.bees-hive.org ==" ]
}

@test "'deliver-work': use stash pipe if there are uncommitted changes" {
    fake-pass "git pull"
    gitrepo "echo stash >> ${FILE_TO_MODIFY}"
    check git-elegant start-work test-feature
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "git stash push" ]]
    [[ "${lines[@]}" =~ "git stash pop" ]]
}
