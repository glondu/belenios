#!/bin/sh

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <target>"
    exit 1
fi

if ! [ -d contrib/debian ]; then
    echo "This script must be run from the root of Belenios source tree"
    exit 1
fi

TARGET="$1"; shift

ORIGIN="$PWD"
ARCH="$(dpkg-architecture -q DEB_BUILD_ARCH)"

mkdir -p "$TARGET"
cd "$TARGET"
ln -sfT "$ORIGIN/contrib/debian/build.mk" Makefile

cat > Makefile.config <<EOF
BELENIOS_SOURCES := $ORIGIN
ARCH := $ARCH
EOF

echo "I: build directory successfully set up in $TARGET"
