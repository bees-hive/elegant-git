#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake

teardown() {
    clean-fake
}

@test "'push': by default name of remote branch is equal to local branch" {
    fake-pass git branch *feature1
    fake-pass git "push --set-upstream --force origin feature1:feature1"
    check git-elegant push
    [ "$status" -eq 0 ]
    [ "${lines[1]}" = "== git push --set-upstream --force origin feature1:feature1 ==" ]
}

@test "'push': raise error #81 if current local branch is master" {
    fake-pass git branch *master
    fake-pass git "push --set-upstream --force origin master:master"
    check git-elegant push
    [ "$status" -eq 81 ]
    [ "${lines[1]}" = "== No pushes to 'master' branch. Read more on http://elegant-git.extsoft.pro ==" ]
}
