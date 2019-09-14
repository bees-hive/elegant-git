#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-git

setup() {
    init-repo
}

teardown() {
    clean-fake
    clean-git
}

@test "'acquire-repository': all configurations work as expected" {
    check git-elegant acquire-repository
    [[ "${status}" -eq 0 ]]
}

@test "'acquire-repository': interactive configuration works as expected" {
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "What is your user name? {Elegant Git}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local user.name Elegant Git" ]]
    [[ "${lines[@]}" =~ "What is your user email? {elegant-git@example.com}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local user.email elegant-git@example.com" ]]
    [[ "${lines[@]}" =~ "Please specify a command to start the editor. {edi}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.editor edi" ]]
}

@test "'acquire-repository': mandatory configuration works as expected on Windows" {
    fake-pass uname -s Windows
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "==>> git config --local core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --local apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.pruneTags true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.autocrlf true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local rebase.autoStash false" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --local credential.helper osxkeychain" ]]
    [[ ! "${lines[@]}" =~ "==>> git config --local core.autocrlf input" ]]
}

@test "'acquire-repository': mandatory configuration works as expected on Linux" {
    fake-pass uname -s Linux
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "==>> git config --local core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --local apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.pruneTags true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.autocrlf input" ]]
    [[ "${lines[@]}" =~ "==>> git config --local pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local rebase.autoStash false" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --local credential.helper osxkeychain" ]]
    [[ ! "${lines[@]}" =~ "==>> git config --local core.autocrlf true" ]]
}

@test "'acquire-repository': mandatory configuration works as expected on Darwin" {
    fake-pass uname -s Darwin
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "==>> git config --local core.commentChar |" ]]
    [[ "${lines[@]}" =~ "==>> git config --local apply.whitespace fix" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.prune true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local fetch.pruneTags true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.autocrlf input" ]]
    [[ "${lines[@]}" =~ "==>> git config --local pull.rebase true" ]]
    [[ "${lines[@]}" =~ "==>> git config --local rebase.autoStash false" ]]
    [[ "${lines[@]}" =~ "==>> git config --local credential.helper osxkeychain" ]]
    # negative checks are used instead of checking commands size
    [[ ! "${lines[@]}" =~ "==>> git config --local core.autocrlf true" ]]
}

@test "'acquire-repository': aliases configuration works as expected" {
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "==>> git config --local alias.start-work elegant start-work" ]]
    [[ "${lines[@]}" =~ "==>> git config --local alias.save-work elegant save-work" ]]
    [[ "${lines[@]}" =~ "==>> git config --local alias.deliver-work elegant deliver-work" ]]
    [[ "${lines[@]}" =~ "==>> git config --local alias.accept-work elegant accept-work" ]]
}

@test "'acquire-repository': local aliases are removed as expected" {
    gitrepo git config --local "alias.aaa" "\"elegant aaa\""
    gitrepo git config --local "alias.bbb" "\"elegant bbb\""
    check git-elegant acquire-repository
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "2 git aliases were removed that contained 'elegant git' reference." ]]
}

@test "'acquire-repository': global aliases aren't removed" {
    gitrepo git config --global "alias.glb" "\"elegant glb\""
    check git-elegant acquire-repository
    gitrepo git config --global --unset "alias.glb"
    [[ "$status" -eq 0 ]]
    [[ "${lines[@]}" =~ "Non-local alias! Remove it if needed using 'git config --global --unset alias.glb'" ]]
    [[ "${lines[@]}" =~ "0 git aliases were removed that contained 'elegant git' reference." ]]
}

@test "'acquire-repository': removing existing git aliases works as expected when aliases are absent" {
    check git-elegant acquire-repository
    [[ "${status}" -eq 0 ]]
    [[ "${lines[@]}" =~ "There are no git aliases which contain 'elegant git' reference." ]]
}
