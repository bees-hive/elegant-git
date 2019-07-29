#!/usr/bin/env bash

_git_elegant() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${prev}" in
        elegant)
            opts=($(git elegant commands))
            COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cur}) )
            return 0 ;;
        pull|epull)
            local data=$(git branch | awk -F ' +' '! /\(no branch\)/ {print $2}')
            COMPREPLY=( $(compgen -W "${data}" ${cur}) )
            return 0 ;;
        accept-work)
            COMPREPLY=(
                $(compgen -W "$(git fetch --all &>/dev/null; git branch --remotes --list)" -- ${cur})
            )
            return 0 ;;
        *)
            return 0 ;;
    esac
}
