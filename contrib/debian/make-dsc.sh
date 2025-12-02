#!/bin/sh

PROJECT_NAME=belenios
PROJECT_PKG=belenios-server

# This script creates a Debian source package $PROJECT_PKG

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

contrib/make-tarball.sh "$TMP/${PROJECT_PKG}_$VERSION.orig.tar.gz"
tar -x -f "$TMP/${PROJECT_PKG}_$VERSION.orig.tar.gz" -C "$TMP"
cd "$TMP/$PROJECT_NAME-"*

mkdir -p debian/source

echo "3.0 (quilt)" > debian/source/format

DEBNAME="Belenios Builder"
DEBMAIL="belenios.builder@example.org"
DEBDATE="$(date -d@$SOURCE_DATE_EPOCH -R -u)"
BDEPS="$(echo "$BELENIOS_DEVDEPS $BELENIOS_DEBDEPS" | sed -r 's/\s+/, /g')"
BINDEPS="$(echo "$BELENIOS_RUNDEPS" | sed -r 's/\s+/, /g')"

cat > debian/changelog <<EOF
$PROJECT_PKG ($VERSION-1) stable; urgency=medium

  * Initial release

 -- $DEBNAME <$DEBMAIL>  $DEBDATE
EOF

cat > debian/control <<EOF
Source: $PROJECT_PKG
Priority: optional
Section: misc
Maintainer: $DEBNAME <$DEBMAIL>
Build-Depends: debhelper-compat (= 13), dh-ocaml, $BDEPS
Standards-Version: 4.7.0
Rules-Requires-Root: no

Package: $PROJECT_PKG
Architecture: any
Depends:
 \${ocaml:Depends},
 \${perl:Depends},
 \${shlibs:Depends},
 \${misc:Depends},
 $BINDEPS
Description: automatically generated package
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
	\$(MAKE) install-doc DESTDIR=_run/usr/share/$PROJECT_PKG/static

override_dh_auto_test:
EOF
chmod +x debian/rules

cat > debian/$PROJECT_PKG.install <<EOF
_run/usr/bin usr
_run/usr/share usr
EOF

cat > debian/$PROJECT_PKG.logrotate <<EOF
/var/$PROJECT_NAME/log/*.log {
        daily
        missingok
        rotate 14
        compress
        delaycompress
        notifempty
        create 0640 $PROJECT_NAME $PROJECT_NAME
        sharedscripts
        postrotate
                echo reopen_logs > /run/$PROJECT_NAME/ocsigenserver_command
        endscript
}
EOF

cat > debian/$PROJECT_PKG.service <<EOF
[Unit]
Description=$PROJECT_NAME server
After=network.target

[Service]
ExecStart=/usr/bin/belenios-start-server
ExecStop=/usr/bin/belenios-stop-server
TimeoutStopSec=15
User=$PROJECT_NAME
RuntimeDirectory=$PROJECT_NAME

[Install]
WantedBy=multi-user.target
EOF

cat > debian/$PROJECT_PKG.postinst <<EOF
#!/bin/sh
set -e

if ! getent passwd $PROJECT_NAME >/dev/null; then
    adduser --comment "$PROJECT_PKG user" --disabled-password --no-create-home --home /var/$PROJECT_NAME $PROJECT_NAME
fi

if [ ! -d /var/$PROJECT_NAME ]; then
    mkdir /var/$PROJECT_NAME
    chown $PROJECT_NAME:$PROJECT_NAME /var/$PROJECT_NAME
fi

#DEBHELPER#
EOF

dpkg-buildpackage --no-sign -S -nc
dcmd cp ../${PROJECT_PKG}_*.dsc $TARGET
