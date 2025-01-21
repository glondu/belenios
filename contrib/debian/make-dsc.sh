#!/bin/sh

# This script creates a Debian source package belenios-server

set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <target>"
    exit 1
fi

TARGET="$1"

export SOURCE_DATE_EPOCH="$(git log -1 --pretty=format:%ct)"

. "$(dirname "$0")/deps.sh"

if [ "$(git status --porcelain | grep -v '^?? ' | wc -l)" -ne 0 ]; then
    echo "git tree is not clean"
    exit 1
fi

VERSION="$(./src/platform/version/get_build.sh)"
echo "I: building version $VERSION"

TMP="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"
trap "rm -rf $TMP" EXIT
echo "I: using directory $TMP..."

contrib/make-tarball.sh "$TMP/belenios-server_$VERSION.orig.tar.gz"
tar -x -f "$TMP/belenios-server_$VERSION.orig.tar.gz" -C "$TMP"
cd "$TMP/belenios-"*

mkdir -p debian/source

echo "3.0 (quilt)" > debian/source/format

DEBNAME="Belenios Builder"
DEBMAIL="belenios.builder@example.org"
DEBDATE="$(date -d@$SOURCE_DATE_EPOCH -R -u)"
BDEPS="$(echo "$BELENIOS_DEVDEPS $BELENIOS_DEBDEPS" | sed -r 's/\s+/, /g')"
BINDEPS="$(echo "$BELENIOS_RUNDEPS" | sed -r 's/\s+/, /g')"

cat > debian/changelog <<EOF
belenios-server ($VERSION-1) stable; urgency=medium

  * Initial release

 -- $DEBNAME <$DEBMAIL>  $DEBDATE
EOF

cat > debian/control <<EOF
Source: belenios-server
Priority: optional
Section: misc
Maintainer: $DEBNAME <$DEBMAIL>
Build-Depends: debhelper-compat (= 13), dh-ocaml, $BDEPS
Standards-Version: 4.7.0
Rules-Requires-Root: no

Package: belenios-server
Architecture: any
Depends:
 \${ocaml:Depends},
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
	dh \$@ --with ocaml

override_dh_auto_build:
	if [ -d /var/cache/belenios-npm ]; then \
	  export BELENIOS_NPM_OFFLINE_CACHE=/var/cache/belenios-npm; \
	fi; \
	TMP="\$\$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"; \
	trap "rm -rf \$\$TMP" EXIT; \
	HOME="\$\$TMP" BELENIOS_BUILD="$VERSION" \$(MAKE) build-release-server
	\$(MAKE) install-doc DESTDIR=_run/usr/share/belenios-server/static

override_dh_auto_test:
EOF
chmod +x debian/rules

cat > debian/belenios-server.install <<EOF
_run/usr/bin usr
_run/usr/share usr
EOF

cat > debian/belenios-server.logrotate <<EOF
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
                echo reopen_logs > /run/belenios/ocsigenserver_command
        endscript
}
EOF

cat > debian/belenios-server.service <<EOF
[Unit]
Description=Belenios election server
After=network.target

[Service]
ExecStart=/usr/bin/belenios-start-server
ExecStop=/usr/bin/belenios-stop-server
TimeoutStopSec=15
User=belenios
RuntimeDirectory=belenios

[Install]
WantedBy=multi-user.target
EOF

cat > debian/belenios-server.postinst <<EOF
#!/bin/sh
set -e

if ! getent passwd belenios >/dev/null; then
    adduser --comment "Belenios user" --disabled-password --no-create-home --home /var/belenios belenios
fi

if [ ! -d /var/belenios ]; then
    mkdir /var/belenios
    chown belenios:belenios /var/belenios
fi

#DEBHELPER#
EOF

dpkg-buildpackage --no-sign -S
dcmd cp ../belenios-server_*.dsc $TARGET
