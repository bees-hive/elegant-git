#!/usr/bin/env bats -ex

load addons-common
load addons-fake
load addons-repo
load addons-read

first=first.txt
second=second.txt
third=third.txt
new_tag=3.0.0

setup() {
    repo-new
    repo-commit-file ${first}
    repo "git tag -a -m ${first} 1"
    repo-commit-file ${second}
    repo "git tag -a -m ${second} 2"
    repo-commit-file ${third}
    repo "git tag -a -m ${third} ${new_tag}"
    repo "git tag && git log --oneline"
    fake-pass "git pull --tags"
    fake-pass "git push --tags"
    fake-pass "git tag --annotate --file tag-message --edit ${new_tag}"
    fake-pass "git remote get-url origin" "https://fake-repo.git"
    read-answer ${new_tag}
}

teardown() {
    fake-clean
    repo-clean
    read-clean
}

@test "'release-work': release work when a new tag is provided as argument" {
    check git-elegant release-work ${new_tag}
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "Release notes" ]]
}

@test "'release-work': release work when a new tag is provided via question" {
    check git-elegant release-work
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "Release notes" ]]
}

@test "'release-work': working branch is restored when the command runs in non-master branch" {
    repo "git checkout -b new"
    check git-elegant release-work
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git checkout new" ]]
}

@test "'release-work': working branch is restored when the failed command reruns" {
    repo "git checkout -b new"
    fake-fail "git tag --annotate --file tag-message --edit 4"
    git-elegant release-work 4 || true
    fake-pass "git tag --annotate --file tag-message --edit 5"
    check git-elegant release-work 5
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "git checkout new" ]]
}
