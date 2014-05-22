#!/bin/sh

# the following is to be run from _build directory
if [ -d ../.git ] && which git >/dev/null 2>&1; then
    BUILD=${BUILD:-$(git describe)}
else
    BUILD=${BUILD:-$(date -u +%Y%m%d)}
fi

head -n1 VERSION
echo $BUILD
