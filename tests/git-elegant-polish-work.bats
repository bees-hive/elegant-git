#!/usr/bin/env bats

load addons-common
load addons-fake
load addons-repo

setup() {
    repo-new
}

teardown() {
    repo-clean
    fake-clean
}

@test "'polish-work': exit code is 42 when the command is run against default protected branch" {
    check git-elegant polish-work
    [[ ${status} -eq 42 ]]
    [[ ${lines[@]} =~ "The protected 'main' branch history can't be rewritten." ]]
    [[ ${lines[@]} =~ "Please read more on https://elegant-git.bees-hive.org." ]]
}

@test "'polish-work': exit code is 42 when the command is run against custom protected branch" {
    repo "git config --local elegant-git.protected-branches \"main some\""
    repo "git checkout -b some"
    check git-elegant polish-work
    [[ ${status} -eq 42 ]]
    [[ ${lines[@]} =~ "The protected 'some' branch history can't be rewritten." ]]
    [[ ${lines[@]} =~ "Please read more on https://elegant-git.bees-hive.org." ]]
}

@test "'polish-work': a rebase process doesn't start when there are no new commits" {
    repo "git checkout -b no-new-commits"
    check git-elegant polish-work
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "There are no new commits comparing to 'main' branch." ]]
}

@test "'polish-work': a rebase process works when there are new commits" {
    repo "git checkout -b new-commits"
    repo-commit-file "1"
    repo-commit-file "2"
    repo-commit-file "3"
    fake-fail "git rebase --interactive @~3" "git rebase was executed"
    check git-elegant polish-work
    [[ ${status} -eq 100 ]]
    [[ ${lines[@]} =~ "git rebase was executed" ]]
}

@test "'polish-work': current rebase is continued when the rebase process is active" {
    repo "git checkout -b new-commits"
    fake-pass "git rev-parse --git-path rebase-merge" ".git"
    check git-elegant polish-work
    [[ ${status} -ne 0 ]]
    [[ ${lines[@]} =~ "No rebase in progress?" ]]
}

@test "'polish-work': all uncommitted changes are applied back when rebase is completed" {
    repo "git checkout -b new-commits"
    repo-commit-file "1"
    repo-non-staged-change "A new line..."
    fake-pass "git rebase --interactive @~1"
    fake-fail "git stash pop stash@{0}"
    check git-elegant polish-work
    [[ ${status} -eq 100 ]]
}
