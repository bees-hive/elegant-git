#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "'obtain-work': raise 45 error if branch name pattern in not set" {
    check git-elegant obtain-work
    [[ "$status" -eq 45 ]]
    [[ "${lines[@]}" =~ "Please provide a branch name or its part." ]]
}

@test "'obtain-work': use found local branch when given pattern matches only one local branch" {
    fake-pass git "for-each-ref --format='%(refname:strip=2)' refs/heads/*" "master\nfoo"
    fake-pass git "checkout foo"
    fake-pass git pull

    check git-elegant obtain-work fo
    [[ "$status" -eq 0 ]]
}

@test "'obtain-work': raise 43 error when given pattern matches several local branches" {
    fake-pass git "for-each-ref --format='%(refname:strip=2)' refs/heads/*" "master\nfoo\nfo2\nfo3"

    check git-elegant obtain-work fo
    [[ "$status" -eq 43 ]]
    [[ "${lines[@]}" =~ "Please re-run the command with concrete branch name from the list above!" ]]
}

@test "'obtain-work': use found remote branch when given pattern matches only one remote branch" {
    fake-pass git "for-each-ref --format='%(refname:strip=2)' refs/heads/*"
    fake-pass git "fetch --all"
    fake-pass git "for-each-ref --format='%(refname:strip=3)' refs/remotes/**" "rremote\nmaster"
    fake-pass git "checkout rremote"

    check git-elegant obtain-work rr
    [[ "$status" -eq 0 ]]
}

@test "'obtain-work': raise 43 error when given pattern matches several remote branches" {
    fake-pass git "for-each-ref --format='%(refname:strip=2)' refs/heads/*"
    fake-pass git "fetch --all"
    fake-pass git "for-each-ref --format='%(refname:strip=3)' refs/remotes/**" "rremote\nmaster\nbarr"

    check git-elegant obtain-work rr
    [[ "$status" -eq 43 ]]
    [[ "${lines[@]}" =~ "Please re-run the command with concrete branch name from the list above!" ]]
}

@test "'obtain-work': raise 43 error when given pattern matches zero remote branches" {
    fake-pass git "for-each-ref --format='%(refname:strip=2)' refs/heads/*"
    fake-pass git "fetch --all"
    fake-pass git "for-each-ref --format='%(refname:strip=3)' refs/remotes/**" "rremote\nmaster\nbarr"

    check git-elegant obtain-work aa
    [[ "$status" -eq 43 ]]
    [[ "${lines[@]}" =~ "There are no either remove or local branches which match 'aa' pattern." ]]
}
