#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-fake
load addons-git

setup() {
    init-repo
    gitrepo git branch --force first
    fake-pass "git branch -lvv" "first [gone]"
}

teardown() {
    fake-clean
    clean-git
}

@test "'clear-local': command is available" {
    check git-elegant clear-local
    [[ "${status}" -eq 0 ]]
}

@test "'clear-local': save WIP prior cleaning and restore after it" {
    gitrepo "git checkout -b other"
    gitrepo "echo stash >> ${FILE_TO_MODIFY}"
    check git-elegant clear-local
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "git stash push" ]]
    [[ "${lines[@]}" =~ "git stash pop" ]]
    [[ "${lines[@]}" =~ "git checkout other" ]]
}
