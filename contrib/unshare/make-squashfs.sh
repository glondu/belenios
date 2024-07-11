#!/bin/sh

# This script creates a squashfs image suitable for use with
# systemd-nspawn.

set -e

if [ "$#" -ne 4 ]; then
    echo "Usage: $0 <suite> <belenios-opam-changes> <tarball> <target>"
    exit 1
fi

ORIGIN="$PWD"
SUITE="$1"; shift
CHANGES="$(readlink -f "$1")"; shift
TARBALL="$(readlink -f "$1")"; shift
TARGET="$1"; shift

BELENIOS_RUNDEPS="netbase ca-certificates zip libgd-securityimage-perl cracklib-runtime"
BELENIOS_DEVDEPS="$BELENIOS_RUNDEPS libsodium-dev git jq npm"

if [ "$(git status --porcelain | grep -v '^?? ' | wc -l)" -ne 0 ]; then
    echo "git tree is not clean"
    exit 1
fi

VERSION="$(git describe)"
echo "I: building version $VERSION"

TMP="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP" EXIT
echo "I: using directory $TMP..."

git archive --prefix=belenios/ HEAD | tar -x -C "$TMP"

cd "$TMP/belenios"

mkdir debian

DEBNAME="$(git config --get user.name)"
DEBMAIL="$(git config --get user.email)"
DEBDATE="$(date -R)"
BDEPS="$(echo "$BELENIOS_DEVDEPS" | sed -r 's/\s+/, /g')"
BINDEPS="$(echo "$BELENIOS_RUNDEPS" | sed -r 's/\s+/, /g')"

cat > debian/changelog <<EOF
belenios-server ($VERSION) belenios-$SUITE; urgency=medium

  * Initial release

 -- $DEBNAME <$DEBMAIL>  $DEBDATE
EOF

cat > debian/control <<EOF
Source: belenios-server
Priority: optional
Section: misc
Maintainer: $DEBNAME <$DEBMAIL>
Build-Depends: debhelper-compat (= 13), $BDEPS
Standards-Version: 4.7.0
Rules-Requires-Root: no

Package: belenios-server
Architecture: any
Depends:
 \${perl:Depends},
 \${shlibs:Depends},
 \${misc:Depends},
 $BINDEPS
Description: Belenios server
EOF

cat > debian/rules <<EOF
#!/usr/bin/make -f
# -*- makefile -*-

%:
	dh \$@

override_dh_auto_build:
	BELENIOS_BUILD=$VERSION \$(MAKE) build-release-server

override_dh_auto_install:
	mkdir -p debian/belenios-server/opt/belenios
	cp -a -t debian/belenios-server/opt/belenios \\
	  /home/belenios/opam-env.sh _run/usr/bin _run/usr/share

override_dh_auto_test:
EOF
chmod +x debian/rules

cd ..
mkdir rootfs

cat > stage1.sh <<EOF
#!/bin/sh

set -e

tar --exclude='./dev/*' --directory=rootfs -xf "$TARBALL"
trap "rm -rf rootfs/*" EXIT
cp -a belenios rootfs/tmp

cat > rootfs/tmp/stage2.sh <<XOF
#!/bin/sh

set -e

export LC_ALL=C.UTF-8
export TMPDIR=/tmp

chown belenios:belenios -R /tmp/belenios
cd /tmp/belenios
runuser -u belenios -- sh -c '. /home/belenios/opam-env.sh; dpkg-buildpackage -b'
XOF

chmod +x rootfs/tmp/stage2.sh
bwrap --bind rootfs / --dev /dev --proc /proc -- /tmp/stage2.sh
dcmd cp rootfs/tmp/*.changes .
EOF

cat > stage3.sh <<EOF
#!/bin/sh

set -e

echo "I: setting up the rootfs..."

ln -sfT /usr/lib/systemd/resolv.conf /etc/resolv.conf
echo belenios > /etc/hostname

cat > /etc/hosts <<XOF
127.0.0.1 localhost
127.0.1.1 belenios
::1     localhost ip6-localhost ip6-loopback
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters
XOF

useradd belenios
mkdir /etc/belenios

cat > /etc/msmtprc <<XOF
account default
host localhost
from %U@belenios
syslog LOG_MAIL
XOF

cat > /etc/logrotate.d/belenios <<XOF
/var/belenios/log/*.log {
        daily
        missingok
        rotate 14
        compress
        delaycompress
        notifempty
        create 0640 belenios belenios
        sharedscripts
        postrotate
                echo reopen_logs > /tmp/belenios/ocsigenserver_command
        endscript
}
XOF

cat > /opt/belenios/belenios-env.sh <<XOF
. /opt/belenios/opam-env.sh
BELENIOS_CONFIG=/etc/belenios/ocsigenserver.conf.in
BELENIOS_VARDIR=/var/belenios
BELENIOS_RUNDIR=/tmp/belenios
BELENIOS_BINDIR=/opt/belenios/bin
BELENIOS_LIBDIR=/opt/belenios/lib
BELENIOS_SHAREDIR=/opt/belenios/share/belenios-server
XOF

cat > /etc/systemd/system/belenios.service <<XOF
[Unit]
Description=Belenios election server
After=network.target

[Service]
ExecStart=/opt/belenios/bin/belenios-start-server --preload /opt/belenios/belenios-env.sh
ExecStop=/opt/belenios/bin/belenios-stop-server --preload /opt/belenios/belenios-env.sh
TimeoutStopSec=15
User=belenios

[Install]
WantedBy=multi-user.target
XOF

ln -sfT /etc/systemd/system/belenios.service /etc/systemd/system/multi-user.target.wants/belenios.service

mkdir /var/belenios
chown belenios:belenios /var/belenios
EOF

chmod +x stage1.sh stage3.sh
unshare --map-auto -r ./stage1.sh

DEB_RUNTIME="$(dcmd --deb "$CHANGES" | grep '/belenios-opam-runtime_')"
DEB_SERVER="$(dcmd --deb *.changes | grep '^belenios-server_')"

TMP2="$(mktemp --tmpdir=/tmp --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP2" EXIT

chmod a+rx "$TMP2"
cp "$DEB_RUNTIME" "$TMP2"
cp "$DEB_SERVER" "$TMP2"
DEB_RUNTIME="$(basename "$DEB_RUNTIME")"
DEB_SERVER="$(basename "$DEB_SERVER")"

cd "$ORIGIN"

mmdebstrap --variant=essential \
  --dpkgopt='path-exclude=/usr/share/man/*' \
  --dpkgopt='path-exclude=/usr/share/locale/*' \
  --dpkgopt='path-include=/usr/share/locale/locale.alias' \
  --dpkgopt='path-exclude=/usr/share/doc/*' \
  --dpkgopt='path-include=/usr/share/doc/*/copyright' \
  --dpkgopt='path-include=/usr/share/doc/*/changelog.Debian.*' \
  --hook-dir=/usr/share/mmdebstrap/hooks/file-mirror-automount \
  --include="passwd systemd dbus msmtp-mta logrotate" \
  --include="$TMP2/$DEB_RUNTIME" \
  --include="$TMP2/$DEB_SERVER" \
  --customize-hook='copy-in "'"$TMP"'/stage3.sh" /tmp' \
  --customize-hook='chroot "$1" /tmp/stage3.sh' \
  --customize-hook='chroot "$1" rm /tmp/stage3.sh' \
  "$SUITE" "$TARGET"
