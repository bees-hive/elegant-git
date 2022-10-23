#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-fake
load addons-repo

setup() {
    repo-new
}

teardown() {
    fake-clean
    repo-clean
}

@test "'prune-repository': a branch is removed when it doesn't have an upstream and new commits" {
    repo "git checkout -b equal-to-main"
    repo "git checkout main"
    check git-elegant prune-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git branch --delete --force equal-to-main" ]]
}

@test "'prune-repository': updates current main branch when there is a remote upstream" {
    repo "git checkout -b equal-to-main"
    fake-pass "git rev-parse --abbrev-ref main@{upstream}"
    fake-pass "git rebase"
    check git-elegant prune-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git fetch --all" ]]
    [[ ${lines[@]} =~ "git rebase" ]]
    [[ ${lines[@]} =~ "git branch --delete --force equal-to-main" ]]
}

@test "'prune-repository': works when the remote repository is unavailable" {
    repo "git checkout -b equal-to-main"
    fake-pass "git rev-parse --abbrev-ref main@{upstream}"
    fake-fail "git fetch --all" "Manual fetch fail"
    fake-pass "git rebase"
    check git-elegant prune-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "Manual fetch fail" ]]
    [[ ${lines[@]} =~ "As the remotes can't be fetched, the current local version is used." ]]
    [[ ${lines[@]} =~ "git branch --delete --force equal-to-main" ]]
}

@test "'prune-repository': a branch is alive when it doesn't have an upstream and has a new commit" {
    repo "git checkout -b commit"
    repo-commit-file "commit"
    check git-elegant prune-repository
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[@]} =~ "git branch --delete --force commit" ]]
}

@test "'prune-repository': a branch is removed when it has a gone upstream" {
    repo "git checkout -b upstream"
    repo "git checkout main"
    fake-pass "git config --get branch.upstream.merge" "upstream"
    fake-fail "git rev-parse --abbrev-ref upstream@{upstream}"
    check git-elegant prune-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git branch --delete --force upstream" ]]
}

@test "'prune-repository': a branch is alive when it has an active upstream" {
    repo "git checkout -b upstream"
    fake-pass "git config --get branch.upstream.merge" "upstream"
    fake-pass "git rev-parse --abbrev-ref upstream@{upstream}"
    check git-elegant prune-repository
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[@]} =~ "git branch --delete --force upstream" ]]
}

@test "'prune-repository': keeps protected branches" {
    repo "git config --local elegant-git.protected-branches \"main some\""
    repo "git checkout -b some"
    repo "git checkout -b equal-to-main"
    repo "git checkout main"
    check git-elegant prune-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git branch --delete --force equal-to-main" ]]
    [[ ! ${lines[@]} =~ "git branch --delete --force main" ]]
    [[ ! ${lines[@]} =~ "git branch --delete --force some" ]]
}
