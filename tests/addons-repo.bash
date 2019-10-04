#!/usr/bin/env bash
set -e

GIT_REPO_DIR="/tmp/elegant-git-repo"
FILE_TO_MODIFY=file

repo-new() {
    if [[ -n "${GIT_REPO_DIR}" ]]; then
        perform-verbose mkdir -p ${GIT_REPO_DIR}
        perform-verbose cd ${GIT_REPO_DIR}
        perform-verbose git init
        perform-verbose git config --local user.email "\"elegant-git@example.com\""
        perform-verbose git config --local user.name "\"Elegant Git\""
        perform-verbose git config --local core.editor "\"edi\""
        perform-verbose touch ${FILE_TO_MODIFY}
        perform-verbose git add .
        perform-verbose git commit -m "\"Add ${FILE_TO_MODIFY}\""
    else
        exit 1
    fi
}

repo-non-staged-change(){
    # modifies committed file
    # usage: repo-non-staged-change <text>...
    perform-verbose "echo -e \"${@}\" >> ${FILE_TO_MODIFY}"
}

repo-staged-change(){
    # modifies committed file and stages changes
    # usage: repo-staged-change <text>...
    repo-non-staged-change "${@}"
    perform-verbose git add ${FILE_TO_MODIFY}
}

repo-commit-file() {
    # Commits a new file to the repository
    # usage: repo-commit-file <file name>
    perform-verbose cd ${GIT_REPO_DIR}
    touch "${1}"
    git add "${1}"
    git commit --message "Add ${1}"
}

repo() {
    # execute given arguments on real git repo
    # usage: repo <command and arguments>
    perform-verbose cd ${GIT_REPO_DIR}
    perform-verbose "${@}"
}

repo-clean() {
    if [[ -d "${GIT_REPO_DIR}" ]]; then
        rm -rf "${GIT_REPO_DIR}"
    fi
}
