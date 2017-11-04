#!/usr/bin/env bash
set -e

GIT_REPO_DIR="$BATS_TMPDIR/mock-repo"
FILE_TO_MODIFY=file

_gilog(){
    echo "$(basename ${BASH_SOURCE[0]}): $@"
}

_ex() {
    _gilog "$@"
    eval "$@"
}

init-repo() {
    if [ -n "$GIT_REPO_DIR" ]; then
        _ex mkdir -p $GIT_REPO_DIR
        _ex cd $GIT_REPO_DIR
        _ex git init
        _ex git config --local user.email "elegant-git@example.com"
        _ex git config --local user.name "Elegant Git"
        _ex touch $FILE_TO_MODIFY
        _ex git add .
        _ex git commit -m "Add $FILE_TO_MODIFY"
    else
        exit 1
    fi
}

add-unst-change(){
    _ex "echo -e \"$@\" >> $FILE_TO_MODIFY"
}

add-st-change(){
    add-unst-change "$@"
    _ex git add $FILE_TO_MODIFY
}
