#!/bin/sh

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <output>"
    exit 1
fi

OUTPUT="$1"

TMP="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP" EXIT

BUILD="$(src/platform/version/get_build.sh)"
TARGET="$TMP/belenios-$BUILD"
mkdir "$TARGET"

: ${SOURCE_DATE_EPOCH:="$(date +%s)"}

if command -v git >/dev/null && git rev-parse --show-toplevel >/dev/null 2>&1; then
    git ls-files > "$TARGET/MANIFEST"
    git log -n 20 --graph --pretty='format:%h %s [%an]' > "$TARGET/GIT_LOG"
    git log -1 --pretty=format:%ct > "$TARGET/SOURCE_DATE_EPOCH"
else
    echo "$SOURCE_DATE_EPOCH" > "$TARGET/SOURCE_DATE_EPOCH"
    if [ -f MANIFEST ]; then
        cp MANIFEST "$TARGET"
    else
        find -type f > "$TARGET/MANIFEST"
    fi
    if [ -f GIT_LOG ]; then
        cp GIT_LOG "$TARGET"
    fi
fi

SOURCE_DATE_EPOCH="$(cat "$TARGET/SOURCE_DATE_EPOCH")"
xargs -d '\n' cp --archive --parents --no-dereference -t "$TARGET" < "$TARGET/MANIFEST"
tar -C "$TMP" -caf "$OUTPUT" --sort=name --owner=root --group=root --mtime="@$SOURCE_DATE_EPOCH" "belenios-$BUILD"
