#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    fake-clean
}

@test "'commands': print all available commands" {
    COMMANDS=(
        "acquire-repository"
        "clone-repository"
        "init-repository"
        "start-work"
        "save-work"
        "deliver-work"
        "accept-work"
        "obtain-work"
        "clear-local"
        "commands"
        "amend-work"
    )
    check git-elegant commands
    [ ${#lines[@]} -eq ${#COMMANDS[@]} ]
    for command in ${COMMANDS[@]}; do
        [[ "${lines[@]}" =~ "${command}" ]]
    done
}

@test "'commands': default exit code is 0" {
    check git-elegant commands
    [ "$status" -eq 0 ]
}
