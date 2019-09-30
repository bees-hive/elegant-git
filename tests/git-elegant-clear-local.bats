#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-fake
load addons-repo

setup() {
    repo-new
    repo git branch --force first
    fake-pass "git branch -lvv" "first [gone]"
}

teardown() {
    fake-clean
    repo-clean
}

@test "'clear-local': command is available" {
    check git-elegant clear-local
    [[ "${status}" -eq 0 ]]
}

@test "'clear-local': save WIP prior cleaning and restore after it" {
    repo "git checkout -b other"
    repo-non-staged-change "A new line..."
    check git-elegant clear-local
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "git stash push" ]]
    [[ "${lines[@]}" =~ "git stash pop" ]]
    [[ "${lines[@]}" =~ "git checkout other" ]]
}
