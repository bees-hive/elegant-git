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

@test "'check': '-a' option is available" {
    preconditions
    check git-elegant check -a
    [ "$status" -eq 0 ]
}

@test "'check': '--all' option is available" {
    preconditions
    check git-elegant check --all
    [ "$status" -eq 0 ]
}

@test "'check': '-u' option is available" {
    preconditions
    check git-elegant check -u
    [ "$status" -eq 0 ]
}

@test "'check': '--unstaged' option is available" {
    preconditions
    check git-elegant check --unstaged
    [ "$status" -eq 0 ]
}

@test "'check': '-s' option is available" {
    preconditions
    check git-elegant check -s
    [ "$status" -eq 0 ]
}

@test "'check': '--staged' option is available" {
    preconditions
    check git-elegant check --staged
    [ "$status" -eq 0 ]
}

@test "'check': no trailing spaces in the staged changes" {
    init-repo && add-st-change "no space"
    check git-elegant check -s
    [ "$status" -eq 0 ]
    [ "${#lines[@]}" -eq 3 ]
}

@test "'check': trailing spaces in the staged changes" {
    init-repo && add-st-change "the space "
    check git-elegant check -s
    [ "$status" -eq 2 ]
    [ "${lines[4]}" = "+the space " ]

}