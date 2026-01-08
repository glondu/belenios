#!/bin/sh

PROJECT_NAME=belenios
PROJECT_PKG=belenios-server

# This script creates a squashfs image suitable for use with
# systemd-nspawn.

set -e

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <belenios-server-deb> <target>"
    exit 1
fi

BELENIOS_SERVER_DEB="$1"
BELENIOS_SERVER_BUILDINFO="${BELENIOS_SERVER_DEB%.deb}.buildinfo"
TARGET="$2"

. "$(dirname "$0")/config.sh"

export SOURCE_DATE_EPOCH="$(git log -1 --pretty=format:%ct)"

TMP="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP" EXIT
echo "I: using directory $TMP..."

chmod 755 "$TMP"

cp "$BELENIOS_SERVER_DEB" "$TMP"
BELENIOS_SERVER_DEB="$TMP/${BELENIOS_SERVER_DEB##*/}"

# Filter out build date for reproducibility
grep -v "^Build-Date: " "$BELENIOS_SERVER_BUILDINFO" > "$TMP/buildinfo.txt"
BELENIOS_SERVER_BUILDINFO="$TMP/buildinfo.txt"


cp "$KEYRING" "$TMP"

cat > "$TMP/ocaml.pref" <<EOF
Package: *
Pin: release a=$BACKPORTS_SUITE
Pin-Priority: 1000
EOF

cat > "$TMP/ocaml.list" <<EOF
deb [signed-by=$TMP/${KEYRING##*/}] $BACKPORTS_MIRROR/pool ./
EOF

cat > "$TMP/sources.list" <<EOF
deb $STABLE_MIRROR/debian $STABLE_SUITE main
deb $STABLE_MIRROR/debian $STABLE_SUITE-updates main
deb $STABLE_MIRROR/debian-security $STABLE_SUITE-security main
EOF

cat > "$TMP/postinst.sh" <<EOF
#!/bin/sh

set -e

echo "I: setting up the rootfs..."

ln -sfT /usr/lib/systemd/resolv.conf /etc/resolv.conf
echo $PROJECT_NAME > /etc/hostname

cat > /etc/hosts <<XOF
127.0.0.1 localhost
127.0.1.1 $PROJECT_NAME
::1     localhost ip6-localhost ip6-loopback
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters
XOF

mkdir /etc/$PROJECT_NAME

cat > /etc/msmtprc <<XOF
account default
host localhost
from %U@$PROJECT_NAME
syslog LOG_MAIL
XOF

SBOM=/usr/share/$PROJECT_PKG/sbom/runtime-deb-packages.txt
echo "Installed-Packages:" > \$SBOM
dpkg-query -W -f=',\n \${binary:Package} (= \${Version})' | tail -n +2 >> \$SBOM
echo >> \$SBOM
chown root:root -R /usr/share/$PROJECT_PKG/sbom
EOF
chmod +x "$TMP/postinst.sh"

mmdebstrap --variant=essential \
  --setup-hook='mkdir -p "$1"'"$TMP" \
  --setup-hook='copy-in "'"$KEYRING"'" "'"$TMP"'"' \
  --setup-hook='mkdir -p "$1"/etc/apt/trusted.gpg.d' \
  --setup-hook='copy-in "'"$KEYRING"'" /etc/apt/trusted.gpg.d' \
  --setup-hook='copy-in "'"$TMP"'"/ocaml.pref /etc/apt/preferences.d' \
  --setup-hook='copy-in "'"$TMP"'"/ocaml.list /etc/apt/sources.list.d' \
  --dpkgopt='path-exclude=/usr/share/man/*' \
  --dpkgopt='path-exclude=/usr/share/locale/*' \
  --dpkgopt='path-include=/usr/share/locale/locale.alias' \
  --dpkgopt='path-exclude=/usr/share/doc/*' \
  --dpkgopt='path-include=/usr/share/doc/*/copyright' \
  --dpkgopt='path-include=/usr/share/doc/*/changelog.Debian.*' \
  --hook-dir=/usr/share/mmdebstrap/hooks/file-mirror-automount \
  --include="passwd systemd systemd-resolved dbus msmtp-mta logrotate" \
  --include="$BELENIOS_SERVER_DEB" \
  --customize-hook='copy-in "'"$BELENIOS_SERVER_BUILDINFO"'" /usr/share/'$PROJECT_PKG'/sbom' \
  --customize-hook='copy-in "'"$TMP"'/postinst.sh" /tmp' \
  --customize-hook='chroot "$1" /tmp/postinst.sh' \
  --customize-hook='chroot "$1" rm /tmp/postinst.sh' \
  --customize-hook='chroot "$1" rm -rf '"$TMP" \
  --customize-hook='chroot "$1" sed -i -r '\''s/(\[.*\]) //'\'' /etc/apt/sources.list.d/ocaml.list' \
  "$STABLE_SUITE" "$TARGET" "$TMP/sources.list"
