#!/usr/bin/env bats

load addons-common
load addons-fake
load addons-read
load addons-repo

setup() {
    repo-new
}

teardown() {
    fake-clean
    repo-clean
}

@test "'amend-work': command works as expected for non-master branch" {
    fake-pass "git add --interactive"
    fake-pass "git diff --cached --check"
    fake-pass "git commit --amend"
    repo git checkout -b test
    check git-elegant amend-work
    [[ ${status} -eq 0 ]]
}


@test "'amend-work': exit code is 42 when the command is run against default protected branch" {
    check git-elegant amend-work
    [[ ${status} -eq 42 ]]
    [[ ${lines[@]} =~ "No direct commits to the protected 'master' branch." ]]
    [[ ${lines[@]} =~ "Please read more on https://elegant-git.bees-hive.org." ]]
    [[ ${lines[@]} =~ "Run 'git elegant start-work' prior to retrying this command." ]]
}

@test "'amend-work': exit code is 42 when the command is run against custom protected branch" {
    repo "git config --local elegant-git.protected-branches \"master some\""
    repo "git checkout -b some"
    check git-elegant amend-work
    [[ ${status} -eq 42 ]]
    [[ ${lines[@]} =~ "No direct commits to the protected 'some' branch." ]]
    [[ ${lines[@]} =~ "Please read more on https://elegant-git.bees-hive.org." ]]
    [[ ${lines[@]} =~ "Run 'git elegant start-work' prior to retrying this command." ]]
}
