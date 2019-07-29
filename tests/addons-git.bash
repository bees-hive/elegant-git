#!/usr/bin/env bash
set -e

GIT_REPO_DIR="/tmp/elegant-git-repo"
FILE_TO_MODIFY=file

init-repo() {
    if [ -n "$GIT_REPO_DIR" ]; then
        testtee mkdir -p $GIT_REPO_DIR
        testtee cd $GIT_REPO_DIR
        testtee git init
        testtee git config --local user.email "\"elegant-git@example.com\""
        testtee git config --local user.name "\"Elegant Git\""
        testtee git config --local core.editor "\"edi\""
        testtee touch $FILE_TO_MODIFY
        testtee git add .
        testtee git commit -m "\"Add $FILE_TO_MODIFY\""
    else
        exit 1
    fi
}

add-unst-change(){
    testtee "echo -e \"$@\" >> $FILE_TO_MODIFY"
}

add-st-change(){
    add-unst-change "$@"
    testtee git add $FILE_TO_MODIFY
}

gitrepo() {
    # execute given arguments on real git repo
    # usage: gitrepo <command and arguments>
    testtee cd ${GIT_REPO_DIR}
    testtee eval $@
    testtee cd -
}

clean-git() {
    if [ -d "$GIT_REPO_DIR" ]; then
        rm -rf "$GIT_REPO_DIR"
    fi
}
