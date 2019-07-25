#!/usr/bin/env bats

load addons-common
load addons-fake

teardown() {
    clean-fake
}

@test "'commands': print all available commands" {
    COMMANDS=(
        "start-work"
        "save-work"
        "deliver-work"
        "accept-work"
        "pull"
        "rebase"
        "init"
        "acquire-repository"
        "add"
        "clear-local"
        "configure-repository"
        "check"
    )
    check git-elegant commands
    for command in {0..12}; do
        [ "${lines[$command]}" = "${COMMANDS[$command]}" ]
    done
    [ ${#lines[@]} -eq 12 ]
}

@test "'commands': default exit code is 0" {
    check git-elegant commands
    [ "$status" -eq 0 ]
}
