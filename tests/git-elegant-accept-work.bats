#!/usr/bin/env bats

load addons-common
load addons-fake
load addons-repo

setup() {
    repo-new
    repo "git checkout -b __eg; git checkout -"
    fake-pass "git elegant obtain-work test-feature __eg"
    fake-pass "git rebase origin/master"
    fake-pass "git fetch --all"
    fake-pass "git push origin master:master"
}

teardown() {
    repo-clean
    fake-clean
}

@test "'accept-work': a work is accepted successfully for given remote branch" {
    fake-pass "git for-each-ref --format='%(upstream:short)' refs/heads/__eg}" "origin/test-feature"
    fake-fail "git push origin --delete test-feature"
    check git-elegant accept-work test-feature
    [[ ${status} -eq 100 ]]
}

@test "'accept-work': exit code is 45 when remote branch name isn't set" {
    check git-elegant accept-work
    [[ ${status} -eq 45 ]]
}

@test "'accept-work': save WIP prior accepting and restore after it" {
    repo "git checkout -b other"
    repo-non-staged-change "A new line..."
    fake-pass "git for-each-ref --format='%(upstream:short)' refs/heads/__eg}" "origin/test-feature"
    fake-pass "git push origin --delete test-feature"
    check git-elegant accept-work test-feature
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git stash push" ]]
    [[ ${lines[@]} =~ "git stash pop" ]]
    [[ ${lines[@]} =~ "git checkout other" ]]
}

@test "'accept-work': a remote branch is not removed when it is pulled from fork" {
    fake-pass "git for-each-ref --format='%(upstream:short)' refs/heads/__eg}" "origin-a/test-feature"
    fake-fail "git push origin --delete test-feature"
    check git-elegant accept-work test-feature
    [[ ${status} -eq 0 ]]
}

@test "'accept-work': continue current rebase when it is initiated by 'accept-work' command" {
    repo "git checkout __eg"
    fake-pass "git rev-parse --git-path rebase-merge" ".git"
    repo "echo refs/heads/__eg > .git/head-name"
    check git-elegant accept-work rebase
    [[ ${status} -ne 0 ]]
    [[ ${lines[@]} =~ "No rebase in progress?" ]]
}

@test "'accept-work': raise error 43 when current rebase is not initiated by 'accept-work' command" {
    repo "git checkout __eg"
    fake-pass "git rev-parse --git-path rebase-merge" ".git"
    repo "echo refs/heads/rebase > .git/head-name"
    check git-elegant accept-work rebase
    [[ ${status} -eq 43 ]]
    [[ ${lines[@]} =~ "rebase" ]]
}
