#!/bin/bash

# This script generates a rootfs tarball suitable for compiling
# Belenios. It uses mmdebstrap. On some machine, it runs in approx. 6
# min and generates a .tar.zst of approx. 1.2 GB.

set -e

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <suite> <belenios-opam-changes> <target>"
    exit 1
fi

SUITE="$1"
DEB="$2"
TARGET="$3"

export SOURCE_DATE_EPOCH="$(git log -1 --pretty=format:%ct)"

SUFFIX="$(basename "$DEB")"
SUFFIX="${SUFFIX#belenios-opam}"
SUFFIX="${SUFFIX%%_*}"

echo "I: SUFFIX is \"$SUFFIX\""

BELENIOS_DEVDEPS="debhelper netbase libsodium-dev zip libgd-securityimage-perl cracklib-runtime git jq npm texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra lmodern texlive-science rubber pandoc"

if ! [ -f "$DEB" ]; then
    echo "$DEB does not exist"
    exit 1
fi

TMP="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP" EXIT
chmod a+rx "$TMP"

# We use a temporary file, and not a pipeline, so that the while loop
# body can operate on the variables outside the loop.
INCLUDES=()
N=0
dcmd --deb "$DEB" > "$TMP/debs"
while read x; do
    cp "$x" "$TMP"
    INCLUDES[$((N++))]="--include=$TMP/$(basename "$x")"
done < "$TMP/debs"

cat > "$TMP/stage2.sh" <<EOF
#!/bin/sh

set -e

useradd --create-home belenios

cat > /tmp/stage3.sh <<XOF
#!/bin/sh

set -e

PATH="/usr/bin"
export PATH
unset MANPATH

head -n2 /opt/belenios$SUFFIX/env.sh > /home/belenios/opam-env.sh
. /home/belenios/opam-env.sh
opam env >> /home/belenios/opam-env.sh
XOF

trap "rm -f /tmp/stage3.sh" EXIT
chmod +x /tmp/stage3.sh
runuser -u belenios /tmp/stage3.sh
EOF

chmod +x "$TMP/stage2.sh"

mmdebstrap --variant=buildd \
  --hook-dir=/usr/share/mmdebstrap/hooks/file-mirror-automount \
  --include="passwd $BELENIOS_DEVDEPS" \
  "${INCLUDES[@]}" \
  --customize-hook='copy-in "'"$TMP"'"/stage2.sh /tmp' \
  --customize-hook='chroot "$1" /tmp/stage2.sh' \
  --customize-hook='chroot "$1" rm -f /tmp/stage2.sh' \
  "$SUITE" "$TARGET"
