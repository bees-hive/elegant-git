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

@test "'git elegant': a help is provided for each available command" {
    for COMMAND in $(git-elegant commands) ; do
        echo "Check help for '${COMMAND}'"
        check git-elegant ${COMMAND} -h
        [[ "$status" -eq 0 ]]
        [ ${#lines[@]} -gt 3 ]
    done
}

@test "'git elegant': each command is included in main help message" {
    check git-elegant --help
    for COMMAND in $(git-elegant commands) ; do
        echo "Check presence of '${COMMAND}'"
        [[ "${lines[@]}" =~ "${COMMAND}" ]]
    done
}

@test "'git elegant': a version is displayed correctly" {
    check git-elegant --version
    [[ "${lines[@]}" =~ "/eg/tests/../libexec/../version" ]]
}
