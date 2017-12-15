#!/usr/bin/env bash
set -e

MOCK_DIR="/tmp/elegant-git-mock"
export PATH=$MOCK_DIR:$PATH


_log_fake(){
    echo "$(basename ${BASH_SOURCE[0]}): $@"
}

_ex_fake() {
    _log_fake "$@"
    eval "$@"
}

fake() {
    # sample: fake <command> <subcommand> <exit> <stdout> <stderr>
    BASENAME=$(basename $1)
    PROGRAM_PATH="$MOCK_DIR/$BASENAME-app"
    FIXTURE_HOME="$PROGRAM_PATH/$(echo "$2" | sed 's/[^0-9a-zA-Z]*//g')"
    MOCK="$MOCK_DIR/$BASENAME"

    [ -d "$FIXTURE_HOME" ] && rm -r "$FIXTURE_HOME"
    _ex_fake mkdir -p "$FIXTURE_HOME"
    echo -e "$3" | tee -i "$FIXTURE_HOME/exit_code"
    echo -e "$4" | tee -i "$FIXTURE_HOME/stdout"
    echo -e "$5" | tee -i "$FIXTURE_HOME/stderr"

    [ -e "$MOCK" ] && rm -r "$MOCK"
    echo "#!/usr/bin/env bash
PROGRAM_PATH=\"$MOCK_DIR/$BASENAME-app\"
FIXTURE_HOME=\"\$PROGRAM_PATH/\$(echo \"\$@\" | sed 's/[^0-9a-zA-Z]*//g')\"
cat \"\$FIXTURE_HOME/stdout\"
cat \"\$FIXTURE_HOME/stderr\" >&2
read -r exit_code < \"\$FIXTURE_HOME/exit_code\"
exit \$exit_code
" | tee -i "$MOCK"
    _ex_fake chmod +x "$MOCK"
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

clean-fake() {
    if [ -d "$MOCK_DIR" ]; then
        rm -r "$MOCK_DIR"
    fi
}
