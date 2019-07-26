#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
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
        "pull"
        "clear-local"
        "commands"
    )
    check git-elegant commands
    for command in {0..12}; do
        [ "${lines[$command]}" = "${COMMANDS[$command]}" ]
    done
    [ ${#lines[@]} -eq 10 ]
}

@test "'commands': default exit code is 0" {
    check git-elegant commands
    [ "$status" -eq 0 ]
}
