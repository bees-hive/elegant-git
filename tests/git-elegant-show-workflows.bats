#!/usr/bin/env bats

load addons-common
load addons-repo

setup() {
    repo-new
}

teardown(){
    repo-clean
}

@test "'show-workflows': displays all available workflow files" {
    repo "mkdir -p .workflows .git/.workflows"
    repo "echo echo ahead git > .git/.workflows/start-work-ahead"
    repo "echo echo ahead no > .workflows/start-work-ahead"
    repo "echo echo after git > .git/.workflows/start-work-after"
    repo "echo echo after no > .workflows/start-work-after"
    repo "ls -lah .git/.workflows/* .workflows/*"
    check git-elegant show-workflows
    [[ ${status} -eq 0 ]]
    [[ ${lines[0]} =~ ".git/.workflows/start-work-ahead" ]]
    [[ ${lines[1]} =~ ".git/.workflows/start-work-after" ]]
    [[ ${lines[2]} =~ ".workflows/start-work-ahead" ]]
    [[ ${lines[3]} =~ ".workflows/start-work-after" ]]
}
