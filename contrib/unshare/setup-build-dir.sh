#!/bin/sh

set -e

if [ "$#" -ne 4 ]; then
    echo "Usage: $0 <suite> <version> <bigtmp> <target>"
    exit 1
fi

if ! [ -f opam-bootstrap.sh ]; then
    echo "This script must be run from the root of Belenios source tree"
    exit 1
fi

SUITE="$1"; shift
VERSION="$1"; shift
BIGTMP="$1"; shift
TARGET="$1"; shift

ORIGIN="$PWD"
ARCH="$(dpkg-architecture -q DEB_BUILD_ARCH)"

mkdir -p "$TARGET"
cd "$TARGET"
ln -sfT "$ORIGIN/contrib/unshare/build.mk" Makefile

cat > Makefile.config <<EOF
TOOLCHAIN_VERSION := $VERSION
BELENIOS_SOURCES := $ORIGIN
SUITE := $SUITE
ARCH := $ARCH
BIGTMP := $BIGTMP
EOF

echo "I: build directory successfully set up in $TARGET"
