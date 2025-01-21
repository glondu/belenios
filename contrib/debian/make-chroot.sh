#!/bin/sh

# This script generates a chroot tarball suitable for compiling
# Belenios, using only Debian packages backported from testing to
# stable. It uses mmdebstrap. On some machine, it runs in approx. 3
# min and generates a .tar.zst of approx. 1.5 GB.

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <target>"
    exit 1
fi

TARGET="$1"

. "$(dirname "$0")/config.sh"

export SOURCE_DATE_EPOCH="$(git log -1 --pretty=format:%ct)"

. "$(dirname "$0")/deps.sh"

TMP="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP" EXIT
chmod a+rx "$TMP"

cat > "$TMP/ocaml.pref" <<EOF
Package: *
Pin: release a=$BACKPORTS_SUITE
Pin-Priority: 1000
EOF

cat > "$TMP/ocaml.list" <<EOF
deb $BACKPORTS_MIRROR/pool ./
EOF

cat > "$TMP/sources.list" <<EOF
deb $STABLE_MIRROR/debian $STABLE_SUITE main
deb $STABLE_MIRROR/debian $STABLE_SUITE-updates main
deb $STABLE_MIRROR/debian-security $STABLE_SUITE-security main
EOF

mkdir "$TMP/belenios-npm"
( cd frontend && npm install && npm ci --cache "$TMP/belenios-npm" )
cp frontend/package-lock.json "$TMP/belenios-npm"
rm -rf "$TMP/belenios-npm/_logs"

mmdebstrap --variant=buildd \
  --setup-hook='mkdir -p "$1"/etc/apt/trusted.gpg.d' \
  --setup-hook='copy-in "'"$KEYRING"'" /etc/apt/trusted.gpg.d' \
  --setup-hook='copy-in "'"$TMP"'"/ocaml.pref /etc/apt/preferences.d' \
  --setup-hook='copy-in "'"$TMP"'"/ocaml.list /etc/apt/sources.list.d' \
  --include="passwd build-essential debhelper $BELENIOS_DEVDEPS $BELENIOS_DEBDEPS" \
  --customize-hook='copy-in "'"$TMP"'"/belenios-npm /var/cache' \
  --customize-hook='chroot "$1" chown root:root -R /var/cache/belenios-npm' \
  "$STABLE_SUITE" "$TARGET" "$TMP/sources.list"
