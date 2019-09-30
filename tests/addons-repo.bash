#!/usr/bin/env bash
set -e

GIT_REPO_DIR="/tmp/elegant-git-repo"
FILE_TO_MODIFY=file

repo-new() {
    if [[ -n "${GIT_REPO_DIR}" ]]; then
        testtee mkdir -p ${GIT_REPO_DIR}
        testtee cd ${GIT_REPO_DIR}
        testtee git init
        testtee git config --local user.email "\"elegant-git@example.com\""
        testtee git config --local user.name "\"Elegant Git\""
        testtee git config --local core.editor "\"edi\""
        testtee touch ${FILE_TO_MODIFY}
        testtee git add .
        testtee git commit -m "\"Add ${FILE_TO_MODIFY}\""
    else
        exit 1
    fi
}

repo-non-staged-change(){
    # modifies committed file
    # usage: repo-non-staged-change <text>...
    testtee "echo -e \"${@}\" >> ${FILE_TO_MODIFY}"
}

repo-staged-change(){
    # modifies committed file and stages changes
    # usage: repo-staged-change <text>...
    repo-non-staged-change "${@}"
    testtee git add ${FILE_TO_MODIFY}
}

repo() {
    # execute given arguments on real git repo
    # usage: repo <command and arguments>
    testtee cd ${GIT_REPO_DIR}
    testtee "${@}"
}

repo-clean() {
    if [[ -d "${GIT_REPO_DIR}" ]]; then
        rm -rf "${GIT_REPO_DIR}"
    fi
}
