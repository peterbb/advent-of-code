#!/bin/sh
set -e

function bad_user {
    echo "WRONG! >:[" >&2
    exit 1
}

function compile {
    mlton -default-ann 'allowOptBar true' "$CODE_FILE"
}

function run {
    compile
    ./$EXEC_FILE < $INPUT_FILE
}

function benchmark {
    compile
}


if [ -f .config ]; then
    read DAY PART INPUT_FILE < .config
    EXEC_FILE="day$DAY-$PART"

    if [ -f "$EXEC_FILE.mlb" ]; then
        CODE_FILE="$EXEC_FILE.mlb"
    else
        CODE_FILE="$EXEC_FILE.sml"
    fi
fi


if [ $# = 3 ]; then
    echo $1 $2 $3 > .config
elif [ $# -eq 1 ]; then
    case "$1" in
    compile|compile|run|benchmark) $1;;
    *) bad_user;;
    esac
else
    bad_user
fi





