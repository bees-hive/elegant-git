#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-repo
load addons-read

setup() {
    repo-new
}

teardown() {
    fake-clean
    repo-clean
    read-clean
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
    [[ "${lines[@]}" =~ "Please specify a command to start the editor. {vi}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local core.editor vi" ]]
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
    for next in $(git-elegant show-commands); do
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

@test "'acquire-repository': 'elegant.acquired' affects configuration correctly" {
    fake-pass "uname -s" Linux
    repo git config --local "alias.aaa" "\"elegant aaa\""
    repo git config --global "alias.bbb" "\"elegant bbb\""
    repo git config --global "elegant.acquired" "true"
    check git-elegant acquire-repository
    [[ "${lines[@]}" =~ "What is your user name? {Elegant Git}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local user.name Elegant Git" ]]
    [[ "${lines[@]}" =~ "What is your user email? {elegant-git@example.com}: " ]]
    [[ "${lines[@]}" =~ "==>> git config --local user.email elegant-git@example.com" ]]
    [[ ! "${lines[@]}" =~ "Please specify a command to start the editor. {vi}: " ]]
    [[ ! "${lines[@]}" =~ "==>> git config --local core.editor vi" ]]
    [[ ! "${lines[@]}" =~ "==>> git config --local core.commentChar |" ]]
    [[ "${lines[@]}" =~ "1 Elegant Git aliases were removed." ]]
    [[ ! "${lines[@]}" =~ "==>> git config --local alias.acquire-repository elegant acquire-repository" ]]
}

@test "'acquire-repository': configures a signature if GPG key is provided" {
    read-answer "The User"
    read-answer "the@email"
    read-answer "someeditor"
    read-answer "thekey"
    fake-pass "gpg --list-secret-keys --keyid-format long the@email" "some dummy keys"
    check git-elegant acquire-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "some dummy keys" ]]
    [[ ${lines[@]} =~ "==>> git config --local user.signingkey thekey" ]]
    [[ ${lines[@]} =~ "==>> git config --local gpg.program /tmp/elegant-git-fakes/gpg" ]]
    [[ ${lines[@]} =~ "==>> git config --local commit.gpgsign true" ]]
    [[ ${lines[@]} =~ "==>> git config --local tag.forceSignAnnotated true" ]]
    [[ ${lines[@]} =~ "==>> git config --local tag.gpgSign true" ]]
}

@test "'acquire-repository': does not configure a signature if GPG key is not provided" {
    read-answer "The User"
    read-answer "the@email"
    read-answer "someeditor"
    read-answer ""
    fake-pass "gpg --list-secret-keys --keyid-format long the@email" "some keys"
    check git-elegant acquire-repository
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[@]} =~ "==>> git config --local user.signingkey thekey" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local gpg.program /tmp/elegant-git-fakes/gpg" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local commit.gpgsign true" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local tag.forceSignAnnotated true" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local tag.gpgSign true" ]]
    [[ ${lines[@]} =~ "The signature is not configured as the empty key is provided." ]]
}

@test "'acquire-repository': does not configure a signature if 'gpg' program is absent" {
    read-answer "The User"
    read-answer "the@email"
    read-answer "someeditor"
    read-answer ""
    check git-elegant acquire-repository
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[@]} =~ "Configuring signature..." ]]
    [[ ! ${lines[@]} =~ "==>> git config --local user.signingkey thekey" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local gpg.program /tmp/elegant-git-fakes/gpg" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local commit.gpgsign true" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local tag.forceSignAnnotated true" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local tag.gpgSign true" ]]
    [[ ! ${lines[@]} =~ "The signature is not configured as the empty key is provided." ]]
}

@test "'acquire-repository': does not configure a signature if there is no key for the email" {
    read-answer "The User"
    read-answer "the@email"
    read-answer "someeditor"
    read-answer ""
    fake-fail "gpg --list-secret-keys --keyid-format long the@email" "gpg: error reading key: No secret key"
    check git-elegant acquire-repository
    [[ ${status} -eq 0 ]]
    [[ ${lines[@]} =~ "Configuring signature..." ]]
    [[ ${lines[@]} =~ "There is no gpg key for the given email." ]]
    [[ ${lines[@]} =~ "A signature is not configured." ]]
    [[ ! ${lines[@]} =~ "==>> git config --local user.signingkey" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local gpg.program /tmp/elegant-git-fakes/gpg" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local commit.gpgsign true" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local tag.forceSignAnnotated true" ]]
    [[ ! ${lines[@]} =~ "==>> git config --local tag.gpgSign true" ]]
    [[ ! ${lines[@]} =~ "The signature is not configured as the empty key is provided." ]]
}
