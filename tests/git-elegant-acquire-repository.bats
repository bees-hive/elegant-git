#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-repo

setup() {
    repo-new
}

teardown() {
    fake-clean
    repo-clean
}

@test "'acquire-repository': all configurations work as expected" {
    check git-elegant acquire-repository
    [[ "${status}" -eq 0 ]]
}

@test "'acquire-repository': basics are configured as expected" {
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "What is your user name? {Elegant Git}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local user.name Elegant Git" ]]
    [[ "${lines[@]}" =~ "What is your user email? {elegant-git@example.com}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local user.email elegant-git@example.com" ]]
    [[ "${lines[@]}" =~ "Please specify a command to start the editor. {edi}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.editor edi" ]]
}

@test "'acquire-repository': standards are configured as expected on Windows" {
    fake-pass "uname -s" Windows
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "==>> git config --local core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --local apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.pruneTags false" ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.autocrlf true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local rebase.autoStash false" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --local credential.helper osxkeychain" ]]
    [[ ! "${lines[@]}" =~ "==>> git config --local core.autocrlf input" ]]
}

@test "'acquire-repository': standards are configured as expected on Linux" {
    fake-pass "uname -s" Linux
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "==>> git config --local core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --local apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.pruneTags false" ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.autocrlf input" ]]
    [[ "${lines[@]}" =~ "==>> git config --local pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local rebase.autoStash false" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --local credential.helper osxkeychain" ]]
    [[ ! "${lines[@]}" =~ "==>> git config --local core.autocrlf true" ]]
}

@test "'acquire-repository': standards are configured as expected on Darwin" {
    fake-pass "uname -s" Darwin
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "==>> git config --local core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --local apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.pruneTags false" ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.autocrlf input" ]]
    [[ "${lines[@]}" =~ "==>> git config --local pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local rebase.autoStash false" ]]
    [[ "${lines[@]}" =~ "==>> git config --local credential.helper osxkeychain" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --local core.autocrlf true" ]]
}

@test "'acquire-repository': new aliases are configured as expected" {
    check git-elegant acquire-repository
    for next in $(git-elegant commands); do
        echo "Test aliasing of '${next}' command"
        [[ "${lines[@]}" =~ "==>> git config --local alias.${next} elegant ${next}" ]]
        echo "Tested successfully!"
    done
}

@test "'acquire-repository': old aliases remove correctly if they are present" {
    repo git config --local "alias.aaa" "\"elegant aaa\""
    repo git config --local "alias.bbb" "\"elegant bbb\""
    check git-elegant acquire-repository
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "2 Elegant Git aliases were removed." ]]
}
