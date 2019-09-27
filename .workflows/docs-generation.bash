#!/usr/bin/env bash

export PATH=bin:${PATH}
if [[ ! $(ls -l README.md) ]]; then
    echo "Please run this script from the root of the repo."
    echo "Current path: ${PWD}"
    exit 1
fi

header() {
    echo "# \`${1}\`"
}

report-command() {
    header ${1} >> ${2}
    eval "git-elegant ${1} --help" >> ${2}
}

report-commands() {
    echo "Write all commands to '${1}'"
    for command in $(git-elegant commands); do
        report-command ${command} ${1}
    done
}

escape-usage() {
    local TMPFILE="TMPFILE"
    local usage=""
    local IFS=''
    while read -r; do
        if [[ ${REPLY} == usage* ]]; then
            usage="found"
        fi
        if [[ ${usage} == "found" ]]; then
            echo "\`\`\`bash" >> ${TMPFILE}
            usage="close"
        fi
        if [[ -z ${REPLY} && ${usage} == "close" ]]; then
            echo "\`\`\`" >> ${TMPFILE}
            usage=""
        fi
        echo -e ${REPLY} >> ${TMPFILE}
    done < ${1}
    echo "Replace escaped file with original: '${TMPFILE}' > '${1}' "
    rm -v ${1}
    mv -v ${TMPFILE} ${1}
}


main() {
    local final="docs/commands.md"
    local commands="docs/raw-commands.md"
    report-commands ${commands}
    escape-usage ${commands}

    echo "Craft '${final}' file  from '${commands}'"
    header git-elegant > ${final}
    echo "\`\`\`bash" >> ${final}
    git-elegant >> ${final}
    echo -e "\`\`\`\n" >> ${final}
    cat ${commands} | sed -e '$ d' >> ${final}
    rm -v ${commands}
}

main
