#!/usr/bin/env bash

_git_elegant() {
    COMPREPLY=()
    local cursor="${COMP_WORDS[COMP_CWORD]}"

    # the first word prior to the ${cursor}
    case ""${COMP_WORDS[COMP_CWORD-1]}"" in
        elegant|git-elegant)
            local opts=($(git elegant commands))
            COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
            return 0 ;;
        pull|epull)
            local data=$(git branch | awk -F ' +' '! /\(no branch\)/ {print $2}')
            COMPREPLY=( $(compgen -W "${data}" ${cursor}) )
            return 0 ;;
        accept-work|obtain-work)
            COMPREPLY=(
                $(compgen -W "$(git branch --remotes --list)" -- ${cursor})
            )
            return 0 ;;
        show-release-notes)
            COMPREPLY=( $(compgen -W "simple smart" ${cursor}) )
            return 0 ;;
        *)  ;;
    esac

    # the second word prior to the ${cursor}
    case "${COMP_WORDS[COMP_CWORD-2]}" in
        show-release-notes)
            local opts=($(git for-each-ref --sort "-version:refname" --format "%(refname:short)" refs/**))
            COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
            return 0 ;;
        *)  ;;
    esac

    # the third word prior to the ${cursor}
    case "${COMP_WORDS[COMP_CWORD-3]}" in
        show-release-notes)
            local opts=($(git for-each-ref --sort "-version:refname" --format "%(refname:short)" refs/**))
            COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
            return 0 ;;
        *)  ;;
    esac
}

complete -F _git_elegant git-elegant
