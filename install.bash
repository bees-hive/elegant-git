#!/usr/bin/env bash -e

[ -z "$INSTALL_PATH" ] && INSTALL_PATH="$HOME/.git-elegant"
[ -z "$REPO_HOME" ] && REPO_HOME="https://github.com/extsoft/elegant-git.git"

man() {
    echo "Usage: ./install.bash [remote|dev|uninstall]"
    echo "  - remote - install from GitHub (default)"
    echo "  - dev - install from local copy"
    echo "  - uninstall - remove current installation"
    echo ""
    echo "Configuration:"
    echo "  INSTALL_PATH=$INSTALL_PATH"
    echo "  REPO_HOME=$REPO_HOME"
}

run() {
    echo "$1"
    eval "$1"
}

install() {
    echo "Installing 'elegant git' to $INSTALL_PATH"
    run "rm -rfv $INSTALL_PATH"
    run "mkdir -p $INSTALL_PATH"
    if [ $1 = 'remote' ]; then
        run "git clone --depth 1 $REPO_HOME $INSTALL_PATH"
        cd "$INSTALL_PATH"
        rm -rf .git
    elif [ $1 = 'dev' ]; then
        cp -rv $(dirname "$0")/* $INSTALL_PATH
    else
        echo "Unexpected mode: $1"
    fi

    run "mkdir -p $INSTALL_PATH/bin"
    run "mkdir -p $INSTALL_PATH/completion"
    run "mv $INSTALL_PATH/src/main/git-elegant-completion $INSTALL_PATH/completion"
    run "mv $INSTALL_PATH/src/main/git-elegant* $INSTALL_PATH/bin"

    echo ""
    echo "Add the following to your .bash_profile to allow:"
    echo "# extend 'git' with 'elegant git' commands"
    echo "export PATH=$INSTALL_PATH/bin:\$PATH"
    echo "#'elegant git' completion"
    echo "[ -f $INSTALL_PATH/completion/git-elegant-completion ] && . $INSTALL_PATH/completion/git-elegant-completion"
    echo ""
    echo "Then please restart the terminal and enjoy the 'elegant git'!"
}

uninstall() {
    echo "Uninstalling 'elegant git' from $INSTALL_PATH"
    run "rm -rfv $INSTALL_PATH"
}

case "$1" in
    uninstall)  uninstall                       ;;
    help)       man                             ;;
    dev)        uninstall && install dev        ;;
    *)          uninstall && install remote     ;;
esac
