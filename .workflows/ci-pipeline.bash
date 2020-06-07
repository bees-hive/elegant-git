#!/usr/bin/env bash
# This script is destructive! That's why it will work only if a specific variable is set.
# It's recommended to run it within a docker container only.
# Usage:
#   ./ci-pipeline.bash              runs quality pipeline
#   ./ci-pipeline.bash --version    prints tooling version
set -e

fail() {
    echo $@
    exit 1
}

pipeline() {
    .workflows/bats-pipeline.bash || fail "Unit tests are failed."
    (
        echo "Installation...."
        git config --global user.name "Elegant Git"
        git config --global user.email elegant.git@email.com
        git config --global core.editor vi
        git config --global elegant-git.acquired true
        ./install.bash /usr/local src
        echo "'Unknown command' testing..."
        git elegant unknown-command | grep "Unknown command: git elegant unknown-command"
        echo "Check installation of version's file..."
        git elegant --version || exit 1
    ) || fail "Installation test is failed."
}

say-version() {
    echo "<<< $@ >>>"
    echo "$($@)"
    echo ""
}

main() {
    case $1 in
        --version)
            say-version "bats --version"
            say-version "python --version"
            say-version "pip freeze"
            ;;
        testing)
            if [[ -z $EG_ENABLE_TESTING ]]; then
                echo "Testing is disabled!"
                exit 1
            fi
            pipeline
            ;;
        *)
            echo "Available commands: --version or testing"; exit 1
    esac
}

main $@
