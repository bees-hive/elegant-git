#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake

teardown() {
    clean-fake
}

@test "'deliver-work': by default, a name of remote branch is equal to local branch" {
    fake-pass git branch *feature1
    fake-pass git "fetch"
    fake-pass git "rebase origin/master"
    fake-pass git "push --set-upstream --force origin feature1:feature1"
    check git-elegant deliver-work
    [ "$status" -eq 0 ]
    [ "${lines[7]}" = "== git push --set-upstream --force origin feature1:feature1 ==" ]
}

@test "'deliver-work': exit code is 42 when current local branch is master" {
    fake-pass git branch *master
    fake-pass git "fetch"
    fake-pass git "rebase origin/master"
    fake-pass git "push --set-upstream --force origin master:master"
    check git-elegant deliver-work
    [ "$status" -eq 42 ]
    [ "${lines[1]}" = "== No pushes to 'master' branch. Please read more on https://elegant-git.bees-hive.org ==" ]
}
