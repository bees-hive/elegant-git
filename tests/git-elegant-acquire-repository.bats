#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-git

fake-preconditions() {
    fake-pass git "config user.name" "UserName"
    fake-pass git "config user.email" "UserEmail"

}
setup() {
    init-repo
}

teardown() {
    clean-fake
    clean-git
}

@test "'acquire-repository': all configurations work as expected" {
    check git-elegant acquire-repository
    [[ ${#lines[@]} -eq 35 ]]
    [[ "${status}" -eq 0 ]]
}

@test "'acquire-repository': interactive configuration works as expected" {
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "What is your user name? {Elegant Git}: " ]]
    [[ "${lines[@]}" =~ "What is your user email? {elegant-git@example.com}: " ]]
}

@test "'acquire-repository': mandatory configuration works as expected" {
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "== git config --local core.commentChar | ==" ]]
    [[ "${lines[@]}" =~ "== git config --local apply.whitespace fix ==" ]]
}

@test "'acquire-repository': aliases configuration works as expected" {
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "== git config --local alias.start-work elegant start-work ==" ]]
    [[ "${lines[@]}" =~ "== git config --local alias.save-work elegant save-work ==" ]]
    [[ "${lines[@]}" =~ "== git config --local alias.deliver-work elegant deliver-work ==" ]]
    [[ "${lines[@]}" =~ "== git config --local alias.accept-work elegant accept-work ==" ]]
}

@test "'acquire-repository': removing existing git aliases works as expected when aliases available" {
    testtee git config --local "alias.aaa" "\"elegant aaa\""
    testtee git config --local "alias.bbb" "\"elegant bbb\""
    check git-elegant acquire-repository
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "== 2 git aliases were removed that contained 'elegant git' reference. ==" ]]
}

@test "'acquire-repository': removing existing git aliases works as expected when aliases are absent" {
    check git-elegant acquire-repository
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "== There are no git aliases which contain 'elegant git' reference. ==" ]]
}
