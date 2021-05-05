#!/bin/sh

set -e

SPOOL="$1"
TARGET="$2"

usage () {
    echo "Usage: $0 SPOOL TARGET"
    echo "Moves deleted elections from SPOOL to TARGET."
    exit 1
}

if ! [ -d "$SPOOL" ] || ! [ -d "$TARGET" ]; then
    usage
fi

for u in "$SPOOL"/*/deleted.json; do
    if [ -f "$u" ]; then
        uuid="${u%/*}"
        uuid="${uuid##*/}"
        if [ -d "$TARGET/$uuid" ]; then
            echo "$TARGET/$uuid already exists"
            exit 2
        else
            mv "$SPOOL/$uuid" "$TARGET"
        fi
    fi
done
