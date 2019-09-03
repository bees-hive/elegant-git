#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "'accept-work': a work is accepted successfully for given remote branch" {
    fake-pass git "elegant obtain-work some __eg"
    fake-pass git "status"
    fake-pass git "rebase origin/master"
    fake-pass git "checkout master"
    fake-pass git "merge --ff-only __eg"
    fake-pass git "push origin master:master"
    fake-pass git "branch --delete --force __eg"
    fake-pass git "for-each-ref --format='%(upstream:short)' refs/heads/_eg}" "origin/some-work"
    fake-pass git "push origin --delete some-work"

    check git-elegant accept-work some
    [[ "$status" -eq 0 ]]
}

@test "'accept-work': exit code is 45 when remote branch name isn't set" {
    check git-elegant accept-work
    [[ "$status" -eq 45 ]]
}
