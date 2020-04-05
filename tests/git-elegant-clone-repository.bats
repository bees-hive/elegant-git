#!/usr/bin/env bats -ex

load addons-common
load addons-read
load addons-cd
load addons-fake

setup() {
    fake-pass "git clone-repository"
    fake-pass "git clone https://github.com/extsoft/elegant-git.git"
    fake-pass "git elegant acquire-repository"
    fake-pass "git rev-parse --show-cdup"
}

teardown() {
    fake-clean
}

@test "'clone-repository': stops with exit code 45 if cloneable URL is not set" {
    check git-elegant clone-repository
    [[ ${status} -eq 45 ]]
    [[ ${lines[0]} =~ "Cloneable URL is not set" ]]
}

@test "'clone-repository': clone the repo" {
    check git-elegant clone-repository https://github.com/extsoft/elegant-git.git
    [[ ${status} -eq 0 ]]
}
