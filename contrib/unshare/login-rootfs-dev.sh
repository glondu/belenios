#!/bin/sh

# Log into a tarball created with make-rootfs-dev.sh.

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <tarball>"
    exit 1
fi

TARBALL="$(readlink -f "$1")"

TMP="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP" EXIT

echo "I: using directory $TMP..."
cd "$TMP"

mkdir rootfs

cat > run-shell.sh <<EOF
#!/bin/sh

set -e

tar --exclude='./dev/*' --directory=rootfs -xf "$TARBALL"
trap "rm -rf rootfs/*" EXIT
bwrap --bind rootfs / --dev /dev --proc /proc -- bash
EOF

chmod +x run-shell.sh
unshare --map-auto -r ./run-shell.sh
