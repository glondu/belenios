#!/bin/sh

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

if command -v git >/dev/null && git rev-parse --show-toplevel >/dev/null 2>&1; then
    git ls-files > "$TARGET/MANIFEST"
    git log -n 20 --graph --pretty='format:%h %s [%an]%d' > "$TARGET/GIT_LOG"
else
    cp MANIFEST GIT_LOG "$TARGET"
fi

xargs -d '\n' cp --parents --no-dereference -t "$TARGET" < "$TARGET/MANIFEST"
tar -C "$TMP" -caf "$OUTPUT" --sort=name "belenios-$BUILD"
