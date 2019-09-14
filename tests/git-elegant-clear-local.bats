#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-fake
load addons-git

setup() {
    init-repo
    gitrepo git branch --force first
    fake-pass git "branch -lvv" "first [gone]"
}

teardown() {
    clean-fake
    clean-git
}

@test "'clear-local': command is available" {
    check git-elegant clear-local
    [[ "${status}" -eq 0 ]]
}
