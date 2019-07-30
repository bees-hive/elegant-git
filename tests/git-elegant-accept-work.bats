#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "'accept-work': a work is accepted successfully for given remote branch" {
    fake-pass git "fetch --all --tags"
    fake-pass git "checkout --force -B __eg origin/work"
    fake-pass git "status"
    fake-pass git "rebase origin/master"
    fake-pass git "checkout master"
    fake-pass git "merge --ff-only __eg"
    fake-pass git "push origin master:master"
    fake-pass git "branch --delete --force __eg"
    fake-pass git "push origin --delete work"
    check git-elegant accept-work origin/work
    [[ "$status" -eq 0 ]]
}

@test "'accept-work': exit code is 45 when remote branch name isn't set" {
    check git-elegant accept-work
    [[ "$status" -eq 45 ]]
}

@test "'accept-work': print error message when remote branch name isn't set" {
    check git-elegant accept-work
    [[ "${lines[0]}" =~ "Please give a name of remote branch (like 'origin/123')." ]]
}
