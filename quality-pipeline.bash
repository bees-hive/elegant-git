#!/usr/bin/env bash
# This script is destructive! That's why it will work only if a specific variable is set.
# It's recommended to run it within a docker container only.
# Usage:
#   ./quality-pipeline.bash              runs quality pipeline
#   ./quality-pipeline.bash --version    prints tooling version
set -e

fail() {
    echo $@
    exit 1
}

pipeline() {
    bats --tap tests                                  || fail "Unit tests are failed."
    ./install.bash /usr/local src && git elegant help || fail "Installation test is failed."
    mkdocs build --clean --strict                     || fail "Unable to build the documentation."
    pdd --exclude=.idea/**/* \
        --exclude=site/**/* \
        --verbose --file=/dev/null                    || fail "Unreadable todo is identified."
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
            say-version "ruby --version"
            say-version "pdd --version"
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
