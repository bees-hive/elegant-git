#!/usr/bin/env bats

load addons-common

@test "'git elegant': a help is displayed if a command is not provided" {
    check git-elegant
    [[ "$status" -eq 0 ]]
    [ ${#lines[@]} -gt 3 ]
}

@test "'git elegant': a unknown command is handled in user-friendly way" {
    check git-elegant sdfsdfsd
    [[ "$status" -eq 46 ]]
    [[ "${lines[@]}" =~ "Unknown command: git elegant sdfsdfsd" ]]
}
