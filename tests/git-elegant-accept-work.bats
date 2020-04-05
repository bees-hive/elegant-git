#!/usr/bin/env bats

load addons-common
load addons-fake
load addons-repo

setup() {
    repo-new
    repo "git checkout -b __eg; git checkout -"
    fake-pass "git elegant obtain-work test-feature __eg"
    fake-pass "git rebase origin/master"
    fake-pass "git push origin master:master"
}

teardown() {
    repo-clean
    fake-clean
}

@test "'accept-work': accepts changes from given remote branch successfully" {
    fake-pass "git remote" "origin"
    fake-pass "git for-each-ref --format='%(upstream:short)' refs/heads/__eg}" "origin/test-feature"
    fake-fail "git push origin --delete test-feature"
    check git-elegant accept-work test-feature
    [[ ${status} -eq 100 ]]
}

@test "'accept-work': stops with exit code 45 when a branch name isn't set" {
    fake-pass "git remote" "origin"
    check git-elegant accept-work
    [[ ${status} -eq 45 ]]
}

@test "'accept-work': saves WIP prior to doing work and restores it after work is done" {
    repo "git checkout -b other"
    repo-non-staged-change "A new line..."
    fake-pass "git remote" "origin"
    fake-pass "git for-each-ref --format='%(upstream:short)' refs/heads/__eg}" "origin/test-feature"
    fake-pass "git push origin --delete test-feature"
    check git-elegant accept-work test-feature
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git stash push" ]]
    [[ ${lines[@]} =~ "git stash pop" ]]
    [[ ${lines[@]} =~ "git checkout other" ]]
}

@test "'accept-work': does not remove a remote branch that is pulled from a fork" {
    fake-pass "git for-each-ref --format='%(upstream:short)' refs/heads/__eg}" "origin-a/test-feature"
    fake-pass "git remote" "origin"
    fake-fail "git push origin --delete test-feature"
    check git-elegant accept-work test-feature
    [[ ${status} -eq 0 ]]
}

@test "'accept-work': accepts changes from given local branch successfully" {
    repo "git checkout -b local-work; git checkout -"
    check git-elegant accept-work local-work
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git fetch --all" ]]
    [[ ${lines[@]} =~ "git checkout -B __eg local-work" ]]
    [[ ${lines[@]} =~ "git rebase master" ]]
    [[ ${lines[@]} =~ "git checkout master" ]]
    [[ ${lines[@]} =~ "git merge --ff-only __eg" ]]
    [[ ${lines[@]} =~ "git branch --delete --force __eg" ]]
    [[ ! ${lines[@]} =~ "git push" ]]
}

@test "'accept-work': continues current rebase if it was started by 'accept-work' command" {
    repo "git checkout __eg"
    fake-pass "git rev-parse --git-path rebase-merge" ".git"
    repo "echo refs/heads/__eg > .git/head-name"
    check git-elegant accept-work rebase
    [[ ${status} -ne 0 ]]
    [[ ${lines[@]} =~ "No rebase in progress?" ]]
}

@test "'accept-work': stops with exit code 43 if current rebase was not started by 'accept-work' command" {
    repo "git checkout __eg"
    fake-pass "git rev-parse --git-path rebase-merge" ".git"
    repo "echo refs/heads/rebase > .git/head-name"
    check git-elegant accept-work rebase
    [[ ${status} -eq 43 ]]
    [[ ${lines[@]} =~ "rebase" ]]
}
