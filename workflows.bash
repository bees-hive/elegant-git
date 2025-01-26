#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
source ./libexec/plugins/text
symlink() {
    sudo ln -s $(stack path --local-install-root)/bin/elegant-git /usr/local/bin/eg
}

commands=(
  symlink
)

main() {
    local command=${1}
    if [[ -z ${command} ]]; then
        question-text "Please select a command:"
        echo ""
        select choise in ${commands[@]} exit; do
            if test ${choise} = "exit"; then exit; fi
            command=${choise}
            if test ${command} = testing; then
                question-text "Please give the tests pattern: "
                read args
            fi
            break
        done
    else
        shift
    fi
    info-box "Run:" ${command} ${args:-${@}}
    ${command} ${args:-${@}}
    if [[ ${command} =~ "test-" ]]; then
        --report-containerized-jobs
    fi
}

set +o nounset
main ${@}
