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
    for COMMAND in $(git-elegant show-commands) ; do
        echo "Check help for '${COMMAND}'"
        check git-elegant ${COMMAND} -h
        [[ "$status" -eq 0 ]]
        [ ${#lines[@]} -gt 3 ]
    done
}

@test "'git elegant': each command is included in main help message" {
    check git-elegant --help
    for COMMAND in $(git-elegant show-commands) ; do
        echo "Check presence of '${COMMAND}'"
        [[ "${lines[@]}" =~ "${COMMAND}" ]]
    done
}

@test "'git elegant': a version is displayed correctly" {
    check git-elegant --version
    [[ "${lines[@]}" =~ "/eg/tests/../libexec/../version" ]]
}

@test "'git elegant': workflows are loaded correctly" {
    perform-verbose mkdir -p .workflows .git/.workflows
    echo "echo ahead git" | tee -i .git/.workflows/show-commands-ahead
    echo "echo ahead no" | tee -i .workflows/show-commands-ahead
    echo "echo after git" | tee -i .git/.workflows/show-commands-after
    echo "echo after no" | tee -i .workflows/show-commands-after
    perform-verbose chmod +x .git/.workflows/* .workflows/*
    perform-verbose ls -lah .git/.workflows/* .workflows/*
    check git-elegant show-commands
    [[ "$status" -eq 0 ]]
    [[ "${lines[0]}" =~ ".git/.workflows/show-commands-ahead" ]]
    [[ "${lines[1]}" == "ahead git" ]]
    [[ "${lines[2]}" =~ ".workflows/show-commands-ahead" ]]
    [[ "${lines[3]}" == "ahead no" ]]
    [[ "${lines[-4]}" =~ ".git/.workflows/show-commands-after" ]]
    [[ "${lines[-3]}" == "after git" ]]
    [[ "${lines[-2]}" =~ ".workflows/show-commands-after" ]]
    [[ "${lines[-1]}" == "after no" ]]
}

@test "'git elegant': workflows are ignored if --no-workflows is set before a command" {
    perform-verbose mkdir -p .workflows
    echo "echo ahead no" | tee -i .workflows/show-commands-ahead
    echo "echo after no" | tee -i .workflows/show-commands-after
    perform-verbose chmod +x .workflows/*
    perform-verbose ls -lah .workflows/*
    check git-elegant --no-workflows show-commands
    [[ "$status" -eq 0 ]]
    [[ ! "${lines[@]}" =~ ".workflows/show-commands-ahead" ]]
    [[ ! "${lines[@]}" =~ ".workflows/show-commands-after" ]]
}

@test "'git elegant': workflows are ignored if --no-workflows is set after a command" {
    perform-verbose mkdir -p .workflows
    echo "echo ahead no" | tee -i .workflows/show-commands-ahead
    echo "echo after no" | tee -i .workflows/show-commands-after
    perform-verbose chmod +x .workflows/*
    perform-verbose ls -lah .workflows/*
    check git-elegant show-commands --no-workflows
    [[ "$status" -eq 0 ]]
    [[ ! "${lines[@]}" =~ ".workflows/show-commands-ahead" ]]
    [[ ! "${lines[@]}" =~ ".workflows/show-commands-after" ]]
}

teardown(){
     if [[ -d ".workflows" ]]; then
        perform-verbose rm -rv .workflows
     fi
     if [[ -d ".git/.workflows" ]]; then
        perform-verbose rm -rv .git/.workflows
     fi
}
