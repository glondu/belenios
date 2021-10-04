#!/bin/sh

set -e

ROOT=../../../../..
VERSION="$(head -n1 $ROOT/VERSION)"
SPEC="$(cat $ROOT/doc/spec_version.tex)"

if [ -d $ROOT/.git ] && command -v git >/dev/null; then
    BUILD=${BUILD:-$(git describe)}
else
    DATE=${SOURCE_DATE_EPOCH:-$(date +%s)}
    DATE=$(date -u -d @$DATE +%Y%m%d)
    BUILD=${BUILD:-$DATE}
fi

if [ -n "$BELENIOS_DEBUG" ]; then
    DEBUG=true
else
    DEBUG=false
fi

echo "let version = \"$VERSION\""
echo "let build = \"$BUILD\""
echo "let spec = \"$SPEC\""
echo "let debug = $DEBUG"
