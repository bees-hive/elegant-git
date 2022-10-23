#!/usr/bin/env bash
# This script is destructive! That's why it will work only if a specific variable is set.
# It's recommended to run it within a docker container only.
# Usage:
#   ./installation-workflows.bash       runs quality pipeline
#   ./installation-workflows.bash    prints tooling version
set -e

install() {
    echo "Installation...."
    git config --global user.name "Elegant Git"
    git config --global user.email elegant.git@email.com
    git config --global core.editor vi
    git config --global elegant-git.acquired true
    git config --global init.defaultBranch main
    ./install.bash /usr/local src
}

smoke-tests() {
    echo "'Unknown command' testing..."
    git elegant unknown-command | grep "Unknown command: git elegant unknown-command"
    echo "Check installation of version's file..."
    git elegant --version || exit 1
}

check-env() {
    if [[ -z ${EG_ENABLE_TESTING} ]]; then
        echo "Testing is disabled! Looks like the environment is not ready..."
        exit 1
    fi
}

usage() {
    cat <<MESSAGE
usage: ${BASH_SOURCE[0]} <command>

Available commands:
    help            prints this message
    install         runs the installation process
    smoke-tests     runs smoke tests of the installation

MESSAGE
}

main() {
    case $1 in
        install)      check-env && install ;;
        smoke-tests)  check-env && smoke-tests ;;
        help)         usage ;;
        *)            echo "Available commands: --version or testing"; exit 1 ;;
    esac
}

main "$@"
