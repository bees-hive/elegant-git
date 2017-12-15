#!/usr/bin/env bats

load addons-common
load addons-git
load addons-fake

teardown() {
    clean-fake
    clean-git
}

preconditions() {
    fake-pass git "diff --check"
    fake-pass git "diff --cached --check"
    fake-pass git "elegant check --all"
}

@test "exit code is 0 when run 'git-elegant check -a'" {
    preconditions
    run git-elegant check -a
    [ "$status" -eq 0 ]
}

@test "'check -s': no trailing spaces in the staged changes" {
    init-repo && add-st-change "no space"
    check git-elegant check -s
    [ "$status" -eq 0 ]
    [ "${#lines[@]}" -eq 0 ]
}

@test "'check -s': trailing spaces in the staged changes" {
    init-repo && add-st-change "the space "
    check git-elegant check -s
    [ "$status" -eq 2 ]
    [ "${lines[1]}" = "+the space " ]

}

@test "exit code is 0 when run 'git-elegant check -u'" {
    preconditions
    run git-elegant check -u
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check --all'" {
    preconditions
    run git-elegant check --all
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check --staged'" {
    preconditions
    run git-elegant check --staged
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check --unstaged'" {
    preconditions
    run git-elegant check --unstaged
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check'" {
    preconditions
    run git-elegant check
    [ "$status" -eq 0 ]
}
