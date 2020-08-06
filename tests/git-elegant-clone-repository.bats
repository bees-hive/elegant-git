#!/usr/bin/env bats

load addons-common
load addons-cd
load addons-fake

setup() {
    fake-pass "git elegant acquire-repository"
}

teardown() {
    fake-clean
}

@test "'clone-repository': stops with exit code 45 if cloneable URL is not set" {
    check git-elegant clone-repository
    [[ ${status} -eq 45 ]]
    [[ ${lines[0]} =~ "There are no arguments!" ]]
}

@test "'clone-repository': clone given repository and store to the default directory" {
    fake-pass "git clone https://github.com/extsoft/elegant-git.git"
    check git-elegant clone-repository https://github.com/extsoft/elegant-git.git
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git clone https://github.com/extsoft/elegant-git.git" ]]
    [[ ${lines[*]} =~ "The repository was cloned into 'elegant-git' directory." ]]
}

@test "'clone-repository': clone given repository and store to the given directory" {
    fake-pass "git clone https://github.com/extsoft/elegant-git.git my-elegnat-git"
    check git-elegant clone-repository https://github.com/extsoft/elegant-git.git my-elegnat-git
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git clone https://github.com/extsoft/elegant-git.git my-elegnat-git" ]]
    [[ ${lines[*]} =~ "The repository was cloned into 'my-elegnat-git' directory." ]]
}

@test "'clone-repository': clone given repository with specific options" {
    fake-pass "git clone --branch work1 https://github.com/extsoft/elegant-git.git"
    check git-elegant clone-repository --branch work1 https://github.com/extsoft/elegant-git.git
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git clone --branch work1 https://github.com/extsoft/elegant-git.git" ]]
    [[ ${lines[*]} =~ "The repository was cloned into 'elegant-git' directory." ]]
}

@test "'clone-repository': clone given repository with specific options and store to the given directory" {
    fake-pass "git clone --branch work1 https://github.com/extsoft/elegant-git.git work1"
    check git-elegant clone-repository --branch work1 https://github.com/extsoft/elegant-git.git work1
    [[ ${status} -eq 0 ]]
    [[ ${lines[*]} =~ "git clone --branch work1 https://github.com/extsoft/elegant-git.git work1" ]]
    [[ ${lines[*]} =~ "The repository was cloned into 'work1' directory." ]]
}
