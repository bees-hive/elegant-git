#!/usr/bin/env bats

load addons-common
load addons-fake
load addons-git

setup() {
    init-repo
    fake-pass "git elegant obtain-work test-feature __eg"
    fake-pass "git rebase origin/master"
    fake-pass "git fetch --all"
    fake-pass "git merge --ff-only __eg"
    fake-pass "git push origin master:master"
    fake-pass "git branch --delete --force __eg"
    fake-pass "git for-each-ref --format='%(upstream:short)' refs/heads/_eg}" "origin/test-feature"
    fake-pass "git push origin --delete test-feature"
}

teardown() {
    clean-git
    fake-clean
}

@test "'accept-work': a work is accepted successfully for given remote branch" {
    check git-elegant accept-work test-feature
    [[ "${status}" -eq 0 ]]
}

@test "'accept-work': exit code is 45 when remote branch name isn't set" {
    check git-elegant accept-work
    [[ "${status}" -eq 45 ]]
}

@test "'accept-work': save WIP prior accepting and restore after it" {
    gitrepo "git checkout -b other"
    gitrepo "echo stash >> ${FILE_TO_MODIFY}"
    check git-elegant accept-work test-feature
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "git stash push" ]]
    [[ "${lines[@]}" =~ "git stash pop" ]]
    [[ "${lines[@]}" =~ "git checkout other" ]]
}
