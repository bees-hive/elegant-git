#!/usr/bin/env bats

load addons-common
load addons-repo

setup() {
    repo-new
    git config --local core.editor cat
}

teardown(){
    repo-clean
}

@test "'make-workflow': creates a new workflow file" {
    check git-elegant make-workflow show-work ahead common
    [[ ${status} -eq 0 ]]
    [[ ${lines[0]} =~ "mkdir -p .workflows" ]]
    [[ ${lines[1]} =~ "touch .workflows/show-work-ahead" ]]
    [[ ${lines[2]} =~ "chmod +x .workflows/show-work-ahead" ]]
    [[ ${lines[3]} =~ "cat .workflows/show-work-ahead" ]]
    [[ ${lines[4]} = "#!/usr/bin/env sh -e" ]]
    [[ ${lines[5]} = "# This script invokes ahead of the 'show-work' execution." ]]
}

@test "'make-workflow': stops with exit code 45 if a command argument is not set" {
    check git-elegant make-workflow
    [[ ${status} -eq 45 ]]
    [[ ${lines[0]} = "Please specify a name of Elegant Git command" ]]
}

@test "'make-workflow': stops with exit code 45 if a type argument is not set" {
    check git-elegant make-workflow show-work
    [[ ${status} -eq 45 ]]
    [[ ${lines[0]} = "Please specify a type of the workflow" ]]
}

@test "'make-workflow': stops with exit code 45 if a location argument is not set" {
    check git-elegant make-workflow show-work ahead
    [[ ${status} -eq 45 ]]
    [[ ${lines[0]} = "Please specify a location of the workflow" ]]
}

@test "'make-workflow': stops with exit code 43 if the workflow aready exists" {
    git-elegant make-workflow show-work ahead common
    check git-elegant make-workflow show-work ahead common
    [[ ${status} -eq 43 ]]
    [[ ${lines[0]} = "The '.workflows/show-work-ahead' file already exists." ]]
    [[ ${lines[1]} = "Please remove it manually and repeat the command if you need a new one." ]]
}
