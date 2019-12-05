#!/usr/bin/env bats

load addons-common
load addons-fake
current=$(pwd)
repository=${current}/new-repository

setup() {
    fake-pass "git elegant acquire-repository"
    perform-verbose "mkdir -p ${repository}"
    perform-verbose "cd ${repository}"
    perform-verbose "git config --global user.email you@example.com"
    perform-verbose "git config --global user.name YourName"
    perform-verbose "git config --global core.pager cat"
}

teardown() {
    perform-verbose "cd ${current}"
    perform-verbose "rm -rf ${repository}"
    perform-verbose "git config --global --unset user.email"
    perform-verbose "git config --global --unset user.name"
    perform-verbose "git config --global --unset core.pager"
    fake-clean
}

@test "'init-repository': creates a new repository with initial commit" {
    check git-elegant init-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git init" ]]
    [[ ${lines[@]} =~ "git commit --allow-empty" ]]
    [[ ${lines[@]} =~ "git show" ]]
    [[ ${lines[@]} =~ "Add initial empty commit" ]]
}
