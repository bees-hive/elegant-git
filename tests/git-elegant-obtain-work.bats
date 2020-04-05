#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    fake-clean
}

@test "'obtain-work': raise 45 error if branch name pattern in not set" {
    check git-elegant obtain-work
    [[ ${status} -eq 45 ]]
    [[ ${lines[@]} =~ "Please provide a branch name or its part." ]]
}

@test "'obtain-work': use found remote branch when given pattern matches only one remote branch" {
    fake-pass "git fetch --all"
    fake-pass "git for-each-ref --format='%(refname:short)' refs/remotes" "origin/rremote\norigin/master"
    fake-pass "git checkout -B rremote origin/rremote"

    check git-elegant obtain-work rr
    [[ ${status} -eq 0 ]]
}

@test "'obtain-work': use given local branch name when it is provided" {
    fake-pass "git fetch --all"
    fake-pass "git for-each-ref --format='%(refname:short)' refs/remotes" "origin/rremote\norigin/master"
    fake-pass "git checkout -B myname origin/rremote"

    check git-elegant obtain-work rr myname
    [[ ${status} -eq 0 ]]
}

@test "'obtain-work': raise 43 error when given pattern matches several remote branches" {
    fake-pass "git fetch --all"
    fake-pass "git for-each-ref --format='%(refname:short)' refs/remotes" "origin/rremote\norigin/master\nother-upstream/barr"

    check git-elegant obtain-work rr
    [[ ${status} -eq 43 ]]
    [[ ${lines[@]} =~ "Please re-run the command with concrete branch name from the list above!" ]]
}

@test "'obtain-work': raise 43 error when given pattern matches zero remote branches" {
    fake-pass "git fetch --all"
    fake-pass "git for-each-ref --format='%(refname:short)' refs/remotes" "origin/rremote\norigin/master\norigin/barr"

    check git-elegant obtain-work aa
    [[ ${status} -eq 43 ]]
    [[ ${lines[@]} =~ "There is no branch that matches the 'aa' pattern." ]]
}
