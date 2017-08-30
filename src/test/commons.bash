#!/usr/bin/env bash
set -e

THIS="$( cd -P "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BIN_DIR="$THIS/../../src/main"
MOCK_DIR="$THIS/../../mock"
export PATH=$MOCK_DIR:$BIN_DIR:$PATH


fake() {
    # sample: fake <command> <subcommand> <exit> <stdout> <stderr>
    BASENAME=$(basename $1)
    PROGRAM_PATH="$MOCK_DIR/$BASENAME-app"
    FIXTURE_HOME="$PROGRAM_PATH/$(echo "$2" | sed 's/[^0-9a-zA-Z]*//g')"
    MOCK="$MOCK_DIR/$BASENAME"

    [ -d "$FIXTURE_HOME" ] && rm -r "$FIXTURE_HOME"
    mkdir -p "$FIXTURE_HOME"
    echo -e "$3" > "$FIXTURE_HOME/exit_code"
    echo -e "$4" > "$FIXTURE_HOME/stdout"
    echo -e "$5" > "$FIXTURE_HOME/stderr"

    [ -e "$MOCK" ] && rm -r "$MOCK"
    echo "#!/usr/bin/env bash
PROGRAM_PATH=\"$MOCK_DIR/$BASENAME-app\"
FIXTURE_HOME=\"\$PROGRAM_PATH/\$(echo \"\$@\" | sed 's/[^0-9a-zA-Z]*//g')\"
cat \"\$FIXTURE_HOME/stdout\"
cat \"\$FIXTURE_HOME/stderr\" >&2
read -r exit_code < \"\$FIXTURE_HOME/exit_code\"
exit \$exit_code
" > "$MOCK"
    chmod +x "$MOCK"
}

fake-pass() {
    # sample: fake-pass <command> <subcommand> <stdout>
    COMMAND="$1"; shift
    SUBCOMMAND="$1"; shift
    fake "$COMMAND" "$SUBCOMMAND" 0 "$@"
}

fake-fail() {
    # sample: fake-fail <command> <subcommand> <stderr>
    COMMAND="$1"; shift
    SUBCOMMAND="$1"; shift
    fake "$COMMAND" "$SUBCOMMAND" 100 " " "$@"
}

teardown() {
    #teardown for bats tests
    if [ -d "$MOCK_DIR" ]; then
        rm -r "$MOCK_DIR"
    fi
}

