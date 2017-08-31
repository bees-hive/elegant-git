#!/usr/bin/env bats

load commons

setup() {
    fake-pass git "diff --check"
    fake-pass git "diff --cached --check"
}

@test "exit code is 0 when run 'git-elegant check -a'" {
    run git-elegant check -a
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check -s'" {
    run git-elegant check -s
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check -u'" {
    run git-elegant check -u
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check --all'" {
    run git-elegant check --all
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check --staged'" {
    run git-elegant check --staged
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check --unstaged'" {
    run git-elegant check --unstaged
    [ "$status" -eq 0 ]
}

@test "exit code is 0 when run 'git-elegant check'" {
    run git-elegant check
    [ "$status" -eq 0 ]
}
