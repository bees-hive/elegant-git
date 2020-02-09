#!/usr/bin/env bash

_git_elegant() {
    COMPREPLY=()
    local cursor="${COMP_WORDS[COMP_CWORD]}"
    local geops="--help --version --no-workflows"
    local gecops="--help --no-workflows"
    local offset=0
    if [[ ${COMP_WORDS[*]} =~ --no-workflows ]]; then
        geops=""
        gecops=""
        if [[ ${COMP_WORDS[COMP_CWORD-1]} =~ --no-workflows ]]; then
            offset=1
        fi
    fi
    # the first word prior to the ${cursor}
    if [[ ${#COMP_WORDS[*]} > $(( 1 + ${offset} )) ]]; then
        case "${COMP_WORDS[COMP_CWORD-$(( 1 + ${offset} ))]}" in
            elegant|git-elegant)
                local opts=(
                    ${geops}
                    $(git elegant show-commands)
                )
                COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
                return 0 ;;
            accept-work|obtain-work)
                local opts=(
                    ${gecops}
                    $(git for-each-ref --format='%(refname:short)' refs/remotes)
                )
                COMPREPLY=(
                    $(compgen -W "${opts[*]}" -- ${cursor})
                )
                return 0 ;;
            show-release-notes)
                COMPREPLY=( $(compgen -W "simple smart ${gecops[*]}" -- ${cursor}) )
                return 0 ;;
            *)
                COMPREPLY=( $(compgen -W "${gecops[*]}" -- ${cursor}) )
                return 0 ;;
        esac
    fi

    # the second word prior to the ${cursor}
    if [[ ${#COMP_WORDS[*]} > $(( 2 + ${offset} )) ]]; then
        case "${COMP_WORDS[COMP_CWORD-$(( 2 + ${offset} ))]}" in
            show-release-notes)
                local opts=($(git for-each-ref --sort "-version:refname" --format "%(refname:short)" refs/**))
                COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
                return 0 ;;
            *)  ;;
        esac
    fi

    # the third word prior to the ${cursor}
    if [[ ${#COMP_WORDS[*]} > $(( 3 + ${offset} )) ]]; then
        case "${COMP_WORDS[COMP_CWORD-$(( 3 + ${offset} ))]}" in
            show-release-notes)
                local opts=($(git for-each-ref --sort "-version:refname" --format "%(refname:short)" refs/**))
                COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
                return 0 ;;
            *)  ;;
        esac
    fi
}

complete -F _git_elegant git-elegant
