#!/usr/bin/env bats

load addons-common
load addons-repo

setup() {
    repo-new
}

teardown(){
    repo-clean
}


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

@test "'git elegant': workflows are loaded correctly when a command is executed from root directory" {
    repo "mkdir -p .workflows .git/.workflows"
    repo "echo echo ahead git > .git/.workflows/show-commands-ahead"
    repo "echo echo ahead no > .workflows/show-commands-ahead"
    repo "echo echo after git > .git/.workflows/show-commands-after"
    repo "echo echo after no > .workflows/show-commands-after"
    repo "chmod +x .git/.workflows/* .workflows/*"
    repo "ls -lah .git/.workflows/* .workflows/*"
    check git-elegant show-commands
    [[ ${status} -eq 0 ]]
    [[ ${lines[0]} =~ ".git/.workflows/show-commands-ahead" ]]
    [[ ${lines[1]} == "ahead git" ]]
    [[ ${lines[2]} =~ ".workflows/show-commands-ahead" ]]
    [[ ${lines[3]} == "ahead no" ]]
    [[ ${lines[-4]} =~ ".git/.workflows/show-commands-after" ]]
    [[ ${lines[-3]} == "after git" ]]
    [[ ${lines[-2]} =~ ".workflows/show-commands-after" ]]
    [[ ${lines[-1]} == "after no" ]]
}

@test "'git elegant': workflows are loaded correctly when a command is executed from non-root directory" {
    repo mkdir -p .workflows .git/.workflows
    repo "echo echo ahead git > .git/.workflows/show-commands-ahead"
    repo "echo echo ahead no > .workflows/show-commands-ahead"
    repo "echo echo after git > .git/.workflows/show-commands-after"
    repo "echo echo after no > .workflows/show-commands-after"
    repo "chmod +x .git/.workflows/* .workflows/*"
    repo "ls -lah .git/.workflows/* .workflows/*"
    repo "mkdir -p some/path"
    repo "cd some/path"
    check git-elegant show-commands
    [[ ${status} -eq 0 ]]
    [[ ${lines[0]} =~ ".git/.workflows/show-commands-ahead" ]]
    [[ ${lines[1]} == "ahead git" ]]
    [[ ${lines[2]} =~ ".workflows/show-commands-ahead" ]]
    [[ ${lines[3]} == "ahead no" ]]
    [[ ${lines[-4]} =~ ".git/.workflows/show-commands-after" ]]
    [[ ${lines[-3]} == "after git" ]]
    [[ ${lines[-2]} =~ ".workflows/show-commands-after" ]]
    [[ ${lines[-1]} == "after no" ]]
}

@test "'git elegant': workflows are ignored if --no-workflows is set before a command" {
    repo "mkdir -p .workflows"
    repo "echo echo ahead no > .workflows/show-commands-ahead"
    repo "echo echo after no > .workflows/show-commands-after"
    repo "chmod +x .workflows/*"
    repo "ls -lah .workflows/*"
    check git-elegant --no-workflows show-commands
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[@]} =~ ".workflows/show-commands-ahead" ]]
    [[ ! ${lines[@]} =~ ".workflows/show-commands-after" ]]
}

@test "'git elegant': workflows are ignored if --no-workflows is set after a command" {
    repo "mkdir -p .workflows"
    repo "echo echo ahead no > .workflows/show-commands-ahead"
    repo "echo echo after no > .workflows/show-commands-after"
    repo "chmod +x .workflows/*"
    repo "ls -lah .workflows/*"
    check git-elegant show-commands --no-workflows
    [[ ${status} -eq 0 ]]
    [[ ! ${lines[@]} =~ ".workflows/show-commands-ahead" ]]
    [[ ! ${lines[@]} =~ ".workflows/show-commands-after" ]]
}
