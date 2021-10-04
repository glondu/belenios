#!/bin/sh

set -e

if [ ! -f "$1" ]; then
    echo "Could not find VERSION!"
    exit 2
fi

VERSION="$(head -n1 "$1")"
SPEC="$(cat "$2")"
BUILD="$(./get_build.sh)"

if [ -n "$BELENIOS_DEBUG" ]; then
    DEBUG=true
else
    DEBUG=false
fi

echo "let version = \"$VERSION\""
echo "let build = \"$BUILD\""
echo "let spec = \"$SPEC\""
echo "let debug = $DEBUG"
