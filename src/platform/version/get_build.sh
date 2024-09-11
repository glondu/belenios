#!/bin/sh

set -e

cd_to_root () {
    for d in . ../../..; do
        if [ -f $d/VERSION ]; then
            cd $d
            return
        fi
    done
    echo "Could not find VERSION!"
    exit 2
}

cd_to_root
VBEL="$(cat VERSION)"

if [ -n "$BELENIOS_BUILD" ]; then
    echo "$BELENIOS_BUILD"
elif command -v git >/dev/null && git rev-parse --show-toplevel >/dev/null 2>&1; then
    VGIT="$(git describe)"
    if [ "$VBEL" != "${VBEL%~dev}" ]; then
        VGIT="${VBEL%dev}${VGIT#*-}"
    fi
    echo "$VGIT"
else
    DATE=${SOURCE_DATE_EPOCH:-$(date +%s)}
    echo "${VBEL}+$(date -u -d @$DATE +%Y%m%d)"
fi
