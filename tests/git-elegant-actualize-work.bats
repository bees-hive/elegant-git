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

@test "'actualize-work': makes a rebase of the default local branch if there is no remote repository" {
    fake-pass "git rebase master"
    check git-elegant actualize-work
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[*]} =~ "git fetch" ]]
    [[ ${lines[*]} =~ "git rebase master" ]]
}

@test "'actualize-work': makes a rebase of the default remote branch if there is a remote repository" {
    fake-pass "git remote" "origin"
    fake-pass "git rebase origin/master"
    fake-fail "git rebase master"
    check git-elegant actualize-work
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git fetch" ]]
    [[ ${lines[*]} =~ "git rebase origin/master" ]]
}

@test "'actualize-work': uses local revision of the default remote branch if the fetch is failed" {
    fake-pass "git remote" "origin"
    fake-fail "git fetch"
    fake-pass "git rebase origin/master"
    fake-fail "git rebase master"
    check git-elegant actualize-work
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git fetch" ]]
    [[ ${lines[*]} =~ "Unable to fetch. The last local revision will be used." ]]
    [[ ${lines[*]} =~ "git rebase origin/master" ]]
}

@test "'actualize-work': makes a rebase of the given local branch without remote-tracking branch" {
    fake-pass "git rebase branch"
    check git-elegant actualize-work branch
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git rebase branch" ]]
    [[ ! ${lines[*]} =~ "git fetch" ]]
}

@test "'actualize-work': makes a rebase of the given local branch with remote-tracking branch" {
    fake-pass "git rev-parse --abbrev-ref rt@{upstream}"
    fake-pass "git fetch"
    fake-pass "git rev-parse --abbrev-ref rt@{upstream}" "origin/rt"
    fake-pass "git rebase origin/rt rt"
    fake-pass "git rebase rt"
    check git-elegant actualize-work rt
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git fetch" ]]
    [[ ${lines[*]} =~ "git rebase origin/rt rt" ]]
    [[ ${lines[*]} =~ "git rebase rt" ]]
}

@test "'actualize-work': makes a rebase of the given remote-tracking branch" {
    fake-pass "git for-each-ref refs/remotes/only/remote" "true"
    fake-pass "git fetch"
    fake-pass "git rebase only/remote"
    check git-elegant actualize-work only/remote
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git fetch" ]]
    [[ ${lines[*]} =~ "git rebase only/remote" ]]
}

@test "'actualize-work': uses local revision of thegiven remote-tracking branch if the fetch is failed" {
    fake-pass "git rev-parse --abbrev-ref rt@{upstream}"
    fake-fail "git fetch"
    fake-pass "git rev-parse --abbrev-ref rt@{upstream}" "origin/rt"
    fake-pass "git rebase origin/rt rt"
    fake-pass "git rebase rt"
    check git-elegant actualize-work rt
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git fetch" ]]
    [[ ${lines[*]} =~ "Unable to fetch. The last local revision will be used." ]]
    [[ ${lines[*]} =~ "git rebase origin/rt rt" ]]
    [[ ${lines[*]} =~ "git rebase rt" ]]
}

@test "'actualize-work': uses stash pipe if uncommited changes are present" {
    repo-non-staged-change "A new line..."
    check git-elegant actualize-work
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git stash push --message git-elegant actualize-work auto-stash:" ]]
    [[ ! ${lines[*]} =~ "git fetch" ]]
    [[ ${lines[*]} =~ "git rebase master" ]]
    [[ ${lines[@]} =~ "git stash pop" ]]
}

@test "'actualize-work': continues an existing rebase process there is an active rebase process" {
    repo "git checkout -b feature1"
    repo "git commit --allow-empty --message First"
    repo "git commit --allow-empty --message Second"
    repo "git rebase --exec 'grep bla-bla *.txt' @~1 || true "
    check git-elegant actualize-work
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git rebase --continue" ]]
    [[ ${lines[@]} =~ "git rebase master" ]]
}
