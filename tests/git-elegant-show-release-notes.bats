#!/usr/bin/env bats -ex

load addons-common
load addons-fake
load addons-repo

first=first.txt
second=second.txt
third=third.txt

setup() {
    repo-new
    repo-commit-file ${first}
    repo "git tag -a -m ${first} 1"
    repo-commit-file ${second}
    repo "git tag -a -m ${second} 2"
    repo-commit-file ${third}
    repo "git tag && git log --oneline"
}

teardown() {
    fake-clean
    repo-clean
}

@test "'show-release-notes': the defaults are used when there are no arguments specified" {
    fake-pass "git remote get-url origin" "https://fake-repo.git"
    check git-elegant show-release-notes
    [[ "${status}" -eq 0 ]]
    [[ ! "${lines[@]}" =~ ${first} ]]
    [[ ! "${lines[@]}" =~ ${second} ]]
    [[ "${lines[@]}" =~ ${third} ]]
    [[ "${#lines[@]}" -eq 2 ]]
}

@test "'show-release-notes': a 'from-reference' is used when it is specified" {
    fake-pass "git remote get-url origin" "https://fake-repo.git"
    check git-elegant show-release-notes simple 1
    [[ "${status}" -eq 0 ]]
    [[ ! "${lines[@]}" =~ ${first} ]]
    [[ "${lines[@]}" =~ ${second} ]]
    [[ "${lines[@]}" =~ ${third} ]]
    [[ "${#lines[@]}" -eq 3 ]]
}

@test "'show-release-notes': a 'to-reference' is used when it is specified" {
    fake-pass "git remote get-url origin" "https://fake-repo.git"
    check git-elegant show-release-notes simple 1 HEAD
    [[ "${status}" -eq 0 ]]
    [[ ! "${lines[@]}" =~ ${first} ]]
    [[ "${lines[@]}" =~ ${second} ]]
    [[ "${lines[@]}" =~ ${third} ]]
}

@test "'show-release-notes': a GitHub layout is used if 'git@git@github.com' is in remote URL" {
    fake-pass "git remote get-url origin" "git@github.com:bees-hive/elegant-git.git"
    check git-elegant show-release-notes smart 1 @
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "<li> <a href=\"https://github.com/bees-hive/elegant-git" ]]
}

@test "'show-release-notes': a GitHub layout is used if 'https://github.com' is in remote URL" {
    fake-pass "git remote get-url origin" "https://github.com/bees-hive/elegant-git.git"
    check git-elegant show-release-notes smart 1 @
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "<li> <a href=\"https://github.com/bees-hive/elegant-git" ]]
}

@test "'show-release-notes': raise error 43 if a given layout is unknown" {
    check git-elegant show-release-notes some
    [[ "${status}" -eq 43 ]]
    [[ "${lines[@]}" =~ "'some' layout is not supported." ]]
}
