#!/usr/bin/env bash
set -e

generate() {
    exec python .workflows/docs/docs.py
}

build() {
    local site_directory=/tmp/elegnat-git-docs
    if test -z "${EG_ENABLE_TESTING}"; then
        site_directory=$(pwd)/elegnat-git-docs
    fi
    exec python -m mkdocs build --clean --strict --site-dir ${site_directory}
}

preview() {
    exec python -m mkdocs serve --dev-addr 0.0.0.0:80
}

help() {
cat <<MESSAGE
usage: ${BASH_SOURCE[0]} <command>

Available commands:
    help            prints this message
    generate        generates fresh commands documentation
    build           builds the static documentation site
    preview         previews the documentation site

MESSAGE
}

${1}
