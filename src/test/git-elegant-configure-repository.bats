#!/usr/bin/env bats

load addons-common
load addons-read
load addons-fake
load addons-git

fake-preconditions() {
    fake-pass git "elegant commands"

    fake-pass git "config user.name" "UserName"
    fake-pass git "config user.email" "UserEmail"

    fake-pass git "config --local core.commentChar"


    fake-pass git "config --local user.name UserName"
    fake-pass git "config --local user.email UserEmail"

    fake-pass git "config --local apply.whitespace fix"
}

teardown() {
    clean-fake
    clean-git
}


@test "'configure-repository': sequence of the local git configuration is correct" {
    init-repo
    check git-elegant configure-repository
    [ "${lines[0]}" = "your user name [Elegant Git]: " ]
    [ "${lines[4]}" = "your user email [elegant-git@example.com]: " ]
    [ "${lines[8]}" = "commit message won't start with [|]: " ]
    [ "${lines[12]}" = "whitespace issues on patching [fix]: " ]
    [ ${#lines[@]} -eq 16 ]
}
