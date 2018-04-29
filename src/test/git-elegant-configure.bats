#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-git

fake-preconditions() {
    fake-pass git "elegant commands"

    fake-pass git "config user.name" "UserName"
    fake-pass git "config user.email" "UserEmail"

    fake-pass git "config --global core.commentChar"
    fake-pass git "config --local core.commentChar"

    fake-pass git "config --global user.name UserName"
    fake-pass git "config --global user.email UserEmail"
    fake-pass git "config --local user.name UserName"
    fake-pass git "config --local user.email UserEmail"

    fake-pass git "config --global apply.whitespace fix"
    fake-pass git "config --local apply.whitespace fix"
}

teardown() {
    clean-fake
    clean-git
}

@test "'configure': exit code is 11 when no arguments provided" {
    fake-preconditions
    check git-elegant configure
    [ "$status" -eq 11 ]
}


@test "'configure': '--global' option is available" {
    fake-preconditions
    check git-elegant configure --global
    [ "$status" -eq 0 ]
}

@test "'configure': '--local' option is available" {
    fake-preconditions
    check git-elegant configure --local
    [ "$status" -eq 0 ]
}

@test "'configure': sequence of the global git configuration is correct" {
    fake-preconditions
    check git-elegant configure --global
    [ "${lines[0]}" = "your user name [UserName]: " ]
    [ "${lines[4]}" = "your user email [UserEmail]: " ]
    [ "${lines[8]}" = "commit message won't start with [|]: " ]
    [ "${lines[12]}" = "whitespace issues on patching [fix]: " ]
    [ "${lines[16]}" = "add git aliases for all 'elegant git' commands [yes]: " ]
    [ ${#lines[@]} -eq 17 ]
}

@test "'configure': sequence of the local git configuration is correct" {
    init-repo
    check git-elegant configure --local
    [ "${lines[0]}" = "your user name [Elegant Git]: " ]
    [ "${lines[4]}" = "your user email [elegant-git@example.com]: " ]
    [ "${lines[8]}" = "commit message won't start with [|]: " ]
    [ "${lines[12]}" = "whitespace issues on patching [fix]: " ]
    [ ${#lines[@]} -eq 16 ]
}
