#!/bin/sh

# the following is to be run from _build directory
if [ -d ../.git ] && which git >/dev/null 2>&1; then
    BUILD=${BUILD:-$(git describe)}
else
    DATE=${SOURCE_DATE_EPOCH:-$(date +%s)}
    DATE=$(date -u -d @$DATE +%Y%m%d)
    BUILD=${BUILD:-$DATE}
fi

head -n1 VERSION
echo $BUILD
