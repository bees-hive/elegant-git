#!/usr/bin/env bash
# The addon allows manipulating what is returned by a system function `read`.
#
# In order to use it, you have to add `load addons-read` to your tests file.
#
# After, you can record answers by calling `read-answer <the answer>`. It
# records a queen of answers. And they will be read by the sequence of adding.
#
# If you didn't record any answers, and an empty string will be used.
#
# Don't forget to call `read-clean` to clean the recorded answers.
set -e

export next_write=0
export next_read=0
export answers_directory="/tmp/read-answers"

read-answer() {
    if [[ ! -d "${answers_directory}" ]]; then
        mkdir -pv ${answers_directory}
    fi
    export next_write=$(($next_write + 1))
    echo "==>> Register replay on 'read' function call #${next_write}"
    echo "${1}" | tee -i ${answers_directory}/${next_write}
}

read-clean() {
    if [[ -d "${answers_directory}" ]]; then
        rm -vr ${answers_directory}
    fi
}

read() {
    export next_read=$((next_read + 1))
    local value
    if [[ -f "${answers_directory}/${next_read}" ]]; then
        value=$(cat ${answers_directory}/${next_read})
    fi
    eval "export ${1}=\"${value}\""
    echo ""
}

export -f read
