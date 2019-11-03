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

next-steps() {
    local INSTALL_PATH=${1}
    local COMPLETION_FILE="${INSTALL_PATH}/completions/git-elegant.bash"
    cat <<TEXT >&1
Please add 3 highlighted lines to your "~/.bashrc" (or "~/.bash_profile"):
--------------------------------------------------------------------------
# elegant git: ${REPO_HOME}
export PATH=${INSTALL_PATH}/bin:\$PATH
[ -f ${COMPLETION_FILE} ] && . ${COMPLETION_FILE}
--------------------------------------------------------------------------
Then, please restart the terminal and enjoy the 'elegant git'!
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
    echo "
The final step after installation is to run
    git elegant acquire-git
to apply the latest Elegant Git's configuration.

If this step is not completed, Elegant Git may behave unexpectedly
since the desired Git configuration (relevant for the current version)
is not applied. Please read more on
    https://elegant-git.bees-hive.org/en/latest/configuration/

"
    command -v git-elegant 1>/dev/null 2>&1 || next-steps ${INSTALL_PATH}
}

main $@
