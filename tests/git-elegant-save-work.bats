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

@test "'save-work': command works as expected for non-master branch" {
    fake-pass "git add --interactive"
    fake-pass "git diff --cached --check"
    fake-pass "git commit"
    repo git checkout -b test
    check git-elegant save-work
    [[ "${status}" -eq 0 ]]
}

@test "'save-work': exit code is 42 when current local branch is master" {
    check git-elegant save-work
    [[ "${status}" -eq 42 ]]
    [[ "${lines[@]}" =~ "== No commits to 'master' branch. Please read more on https://elegant-git.bees-hive.org ==" ]]
    [[ "${lines[@]}" =~ "== Try 'git elegant start-work' prior to retrying this command. ==" ]]
}
