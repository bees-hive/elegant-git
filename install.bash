#!/usr/bin/env bash
# Usage:
#   ./install.bash                           installs to "${HOME}/.elegant-git" from remote git
#   ./install.bash /installation/path        installs to "/installation/path" from remote git
#   ./install.bash /installation/path src    installs to "installation/path" from local sources
set -e

REPO_HOME="https://github.com/bees-hive/elegant-git.git"

copy(){
    local FROM=${1}
    local INTO=${2}
    install -d -m 755 ${INTO}/{bin,libexec,completions}
    install -d -m 755 ${INTO}/libexec/plugins
    install -m 755 ${FROM}/bin/git* ${INTO}/bin
    install -m 755 ${FROM}/libexec/git* ${INTO}/libexec
    install -m 755 ${FROM}/libexec/plugins/* ${INTO}/libexec/plugins
    install -m 644 ${FROM}/completions/* ${INTO}/completions
    install -m 644 ${FROM}/LICENSE ${INTO}
    install -m 644 ${FROM}/README.md ${INTO}
    install -m 644 ${FROM}/version ${INTO}
}

update-path() {
    local INSTALL_PATH=${1}
    cat <<TEXT
You need to add Elegant Git to the PATH by adding to
a "~/.bashrc" or "~/.bash_profile" file:

    export PATH=${INSTALL_PATH}/bin:\$PATH

After, please configure the installation by running

    git-elegant acquire-git

TEXT
}

update-completion() {
    local INSTALL_PATH=${1}
    local COMPLETION_FILE="${INSTALL_PATH}/completions/git-elegant.bash"
    cat <<TEXT
You need to add loading of BASH completion file to
a "~/.bashrc" or "~/.bash_profile" file:

    [ -f ${COMPLETION_FILE} ] && . ${COMPLETION_FILE}

TEXT
}

add-version() {
    cd "${1}"
    git describe >> version
    cd -
}

main() {
    if [[ -n ${1} ]]; then
        INSTALL_PATH="${1}"
        shift
    fi
    : ${INSTALL_PATH:="${HOME}/.elegant-git"}
    # mode selection
    if [[ -z ${1} ]]; then
        local CODE="/tmp/elegant-git"
        git clone --quiet --depth 50 ${REPO_HOME} ${CODE}
        add-version ${CODE}
        copy ${CODE} ${INSTALL_PATH}
        rm -r ${CODE}
    else
        local path="${0%/*}"
        add-version "${path}"
        copy "${path}" ${INSTALL_PATH}
    fi
    echo "Elegant Git is installed to '${INSTALL_PATH}/bin/git-elegant'."
    if command -v git-elegant 1>/dev/null 2>&1; then
        git-elegant acquire-git
    else
        update-path ${INSTALL_PATH}
    fi
    if ! complete -p git-elegant 1>/dev/null 2>&1; then
        update-completion ${INSTALL_PATH}
    fi
}

main $@
