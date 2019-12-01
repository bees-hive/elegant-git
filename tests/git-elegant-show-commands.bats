#!/usr/bin/env bats

load addons-common
load addons-fake

setup() {
    fake-pass "git rev-parse --show-cdup"
}

teardown() {
    fake-clean
}

@test "'show-commands': print all available Elegant Git commands" {
    COMMANDS=(
        "acquire-repository"
        "acquire-git"
        "clone-repository"
        "init-repository"
        "start-work"
        "save-work"
        "deliver-work"
        "accept-work"
        "obtain-work"
        "prune-repository"
        "show-commands"
        "amend-work"
        "show-release-notes"
        "release-work"
        "polish-work"
    )
    check git-elegant show-commands
    [ ${#lines[@]} -eq ${#COMMANDS[@]} ]
    for command in ${COMMANDS[@]}; do
        [[ "${lines[@]}" =~ "${command}" ]]
    done
}

@test "'show-commands': default exit code is 0" {
    check git-elegant show-commands
    [ "$status" -eq 0 ]
}
