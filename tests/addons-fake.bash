#!/usr/bin/env bash
set -e

FAKES_DIRECTORY="/tmp/elegant-git-fakes"
export PATH=${FAKES_DIRECTORY}:${PATH}

_log_fake(){
    echo "$(basename ${BASH_SOURCE[0]}): $@"
}

_ex_fake() {
    _log_fake "$@"
    eval "$@"
}

fake() {
    # sample: fake <command> <exit> <stdout> <stderr>
    local executable=$(basename ${1%% *})
    local fake_directory="${FAKES_DIRECTORY}/${executable}-app"
    local command_directory="${fake_directory}/$(echo "${1}" | sed 's/[^0-9a-zA-Z_-]*//g')"
    local fake="${FAKES_DIRECTORY}/${executable}"

    if [[ ! -e "${fake}" ]]; then
        mkdir -p ${FAKES_DIRECTORY}
        echo ""
        echo "==>> Creating fake executable: ${fake}"
        local origin_binary=$(which ${executable})
        cat <<FAKE | tee -i ${fake} && _ex_fake chmod +x "${fake}"
#!/usr/bin/env bash
# This is an alternative executable for "${executable}".
# It's purpose is to use a mock, if available, otherwise,
# run original executable.
root_directory=${fake_directory}
command_directory="\${root_directory}/\$(echo "${executable} \${@}" | sed 's/[^0-9a-zA-Z_-]*//g')"
if [[ -e "\${command_directory}" ]]; then
    cat "\${command_directory}/stdout"
    cat "\${command_directory}/stderr" >&2
    exit \$(cat "\${command_directory}/exit_code")
else
    ${origin_binary} "\$@"
fi
FAKE
    fi

    [[ -d "${command_directory}" ]] && rm -r "${command_directory}"
    echo ""
    echo "==>> Creating fake command: ${command_directory}"
    _ex_fake mkdir -p "${command_directory}"
    echo -e "${2}" | tee -i "${command_directory}/exit_code"
    echo -e "${3}" | tee -i "${command_directory}/stdout"
    echo -e "${4}" | tee -i "${command_directory}/stderr"
}

fake-pass() {
    # sample: fake-pass <command> <stdout>
    local command="${1}"; shift
    fake "${command}" 0 "$@"
}

fake-fail() {
    # sample: fake-fail <command> <stderr>
    local command="${1}"; shift
    fake "${command}" 100 " " "$@"
}

fake-clean() {
    if [[ -d "${FAKES_DIRECTORY}" ]]; then
        rm -r "${FAKES_DIRECTORY}"
    fi
}
