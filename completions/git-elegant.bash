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
            elegant|git-elegant|make-workflow)
                local opts=(
                    ${geops}
                    $(git elegant show-commands)
                )
                COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
                ;;
            obtain-work)
                local opts=(
                    ${gecops}
                    $(git for-each-ref --format='%(refname:short)' refs/remotes 2>/dev/null)
                )
                COMPREPLY=(
                    $(compgen -W "${opts[*]}" -- ${cursor})
                )
                ;;
            accept-work|actualize-work)
                local opts=(
                    ${gecops}
                    $(git branch --all --format='%(refname:short)')
                )
                COMPREPLY=(
                    $(compgen -W "${opts[*]}" -- ${cursor})
                )
                ;;
            show-release-notes)
                COMPREPLY=( $(compgen -W "simple smart ${gecops[*]}" -- ${cursor}) )
                ;;
            polish-workflow)
                COMPREPLY=(
                    $(compgen -W "${opts[*]} $(git elegant show-workflows)" -- ${cursor})
                )
                ;;
            *)
                COMPREPLY=( $(compgen -W "${gecops[*]}" -- ${cursor}) )
                ;;
        esac
    fi

    # the second word prior to the ${cursor}
    if [[ ${#COMP_WORDS[*]} > $(( 2 + ${offset} )) ]]; then
        case "${COMP_WORDS[COMP_CWORD-$(( 2 + ${offset} ))]}" in
            show-release-notes|start-work)
                local opts=(
                    $(git for-each-ref --sort "-version:refname" --format "%(refname:short)" refs 2>/dev/null)
                )
                COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
                ;;
            make-workflow)
                COMPREPLY=( $(compgen -W "ahead after" -- ${cursor}) )
                ;;
            *)  ;;
        esac
    fi

    # the third word prior to the ${cursor}
    if [[ ${#COMP_WORDS[*]} > $(( 3 + ${offset} )) ]]; then
        case "${COMP_WORDS[COMP_CWORD-$(( 3 + ${offset} ))]}" in
            show-release-notes)
                local opts=(
                    $(git for-each-ref --sort "-version:refname" --format "%(refname:short)" refs 2>/dev/null)
                )
                COMPREPLY=( $(compgen -W "${opts[*]}" -- ${cursor}) )
                ;;
            make-workflow)
                COMPREPLY=( $(compgen -W "common personal" -- ${cursor}) )
                ;;
            *)  ;;
        esac
    fi
}

complete -F _git_elegant git-elegant
