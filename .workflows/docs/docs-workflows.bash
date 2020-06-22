#!/usr/bin/env bash
set -e

generate() {
    python .workflows/docs/docs.py
}

build() {
    local site_directory=/tmp/elegnat-git-docs
    if test -z "${EG_ENABLE_TESTING}"; then
        site_directory=$(pwd)/elegnat-git-docs
    fi
    python -m mkdocs build --clean --strict --site-dir ${site_directory}
}

preview() {
    exec python -m mkdocs serve --dev-addr 0.0.0.0:80
}

check() {
    echo "Checking if there are uncommitted docs files ..."
    git update-index --really-refresh
    git diff-index --quiet HEAD docs
}

usage() {
cat <<MESSAGE
usage: ${BASH_SOURCE[0]} <command>

Available commands:
    help            prints this message
    generate        generates fresh commands documentation
    build           builds the static documentation site
    preview         previews the documentation site
    check           shows whether 'docs' directory is committed or not

MESSAGE
}

main() {
    case ${1} in
        help)  usage ;;
        ci)    generate && check ;;
        *)     "${@}" ;;
    esac
}

main "${@}"
