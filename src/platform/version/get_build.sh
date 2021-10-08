#!/bin/sh

set -e

if [ -n "$BELENIOS_BUILD" ]; then
    echo $BELENIOS_BUILD
elif command -v git >/dev/null && git rev-parse --show-toplevel >/dev/null 2>&1; then
    git describe
else
    DATE=${SOURCE_DATE_EPOCH:-$(date +%s)}
    date -u -d @$DATE +%Y%m%d
fi
