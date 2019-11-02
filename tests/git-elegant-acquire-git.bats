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

@test "'acquire-git': all configurations work as expected" {
    check git-elegant acquire-git
    [[ "${status}" -eq 0 ]]
}

@test "'acquire-git': basics are configured as expected" {
    check git-elegant acquire-git
    [[ "${lines[@]}" =~ "What is your user name? {Elegant Git}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --global user.name Elegant Git" ]]
    [[ "${lines[@]}" =~ "What is your user email? {elegant-git@example.com}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --global user.email elegant-git@example.com" ]]
    [[ "${lines[@]}" =~ "Please specify a command to start the editor. {edi}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --global core.editor edi" ]]
}

@test "'acquire-git': standards are configured as expected on Windows" {
    fake-pass "uname -s" Windows
    check git-elegant acquire-git
    [[ "${lines[@]}" =~ "==>> git config --global core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --global apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --global fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --global fetch.pruneTags false" ]]
    [[ "${lines[@]}" =~ "==>> git config --global core.autocrlf true" ]]
    [[ "${lines[@]}" =~ "==>> git config --global pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --global rebase.autoStash false" ]]
    [[ "${lines[@]}" =~ "==>> git config --global elegant.acquired true" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --global credential.helper osxkeychain" ]]
    [[ ! "${lines[@]}" =~ "==>> git config --global core.autocrlf input" ]]
}

@test "'acquire-git': standards are configured as expected on Linux" {
    fake-pass "uname -s" Linux
    check git-elegant acquire-git
    [[ "${lines[@]}" =~ "==>> git config --global core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --global apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --global fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --global fetch.pruneTags false" ]]
    [[ "${lines[@]}" =~ "==>> git config --global core.autocrlf input" ]]
    [[ "${lines[@]}" =~ "==>> git config --global pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --global rebase.autoStash false" ]]
    [[ "${lines[@]}" =~ "==>> git config --global elegant.acquired true" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --global credential.helper osxkeychain" ]]
    [[ ! "${lines[@]}" =~ "==>> git config --global core.autocrlf true" ]]
}

@test "'acquire-git': standards are configured as expected on Darwin" {
    fake-pass "uname -s" Darwin
    check git-elegant acquire-git
    [[ "${lines[@]}" =~ "==>> git config --global core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --global apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --global fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --global fetch.pruneTags false" ]]
    [[ "${lines[@]}" =~ "==>> git config --global core.autocrlf input" ]]
    [[ "${lines[@]}" =~ "==>> git config --global pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --global rebase.autoStash false" ]]
    [[ "${lines[@]}" =~ "==>> git config --global credential.helper osxkeychain" ]]
    [[ "${lines[@]}" =~ "==>> git config --global elegant.acquired true" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --global core.autocrlf true" ]]
}

@test "'acquire-git': new aliases are configured as expected" {
    check git-elegant acquire-git
    for next in $(git-elegant commands); do
        echo "Test aliasing of '${next}' command"
        [[ "${lines[@]}" =~ "==>> git config --global alias.${next} elegant ${next}" ]]
        echo "Tested successfully!"
    done
}

@test "'acquire-git': old aliases remove correctly if they are present" {
    repo git config --global "alias.aaa" "\"elegant aaa\""
    repo git config --global "alias.bbb" "\"elegant bbb\""
    check git-elegant acquire-git
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "2 Elegant Git aliases were removed." ]]
}
