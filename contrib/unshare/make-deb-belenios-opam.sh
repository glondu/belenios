#!/bin/sh

# This script generates .deb packages that install in /opt the output
# of opam-bootstrap.sh. Run with TMPDIR set to a directory with >= 10
# GB free space. It uses mmdebstrap and bwrap. On some machine, it
# runs in approx. 15 min and the resulting packages are approx. 500
# MB.

# Additional opam packages can be installed by giving them as a
# space-separated list. This is useful for installing packages that
# are useful for development, but not for production (e.g. utop,
# merlin, ocamlformat, dune-deps...).

set -e

if [ "$#" -ne 5 ]; then
    echo "Usage: $0 <suite> <suffix> <version> <additional-opam-packages> <target>"
    exit 1
fi

OPAM_DEPS="wget ca-certificates git unzip rsync"
DEV_DEPS="libgmp-dev pkgconf m4 libssl-dev libsqlite3-dev libncurses-dev zlib1g-dev"
DEPS="bubblewrap $OPAM_DEPS $DEV_DEPS"

SUITE="$1"
SUFFIX="$2"
VERSION="$3"
ADDITIONAL_OPAM_PACKAGES="$4"
TARGET="$(readlink -f "$5")"

export SOURCE_DATE_EPOCH="$(git log -1 --pretty=format:%ct)"

if ! [ -d "$TARGET" ]; then
    echo "$TARGET is not a directory"
    exit 1
fi

BELDIR="$(readlink -f "$(dirname "$0")/../..")"
BOOTSTRAP="$BELDIR/opam-bootstrap.sh"

if ! [ -f "$BOOTSTRAP" ]; then
    echo "$BOOTSTRAP is missing"
    exit 1
fi

DEBDIR="$(mktemp --tmpdir --directory tmp.belenios.XXXXXXXXXX)"

trap "rm -rf $DEBDIR" EXIT

cd "$DEBDIR"

mmdebstrap --variant=buildd --include="debhelper passwd $DEPS" "$SUITE" rootfs.tar.zst

mkdir rootfs

cat > stage1.sh <<EOF
#!/bin/sh

set -e

tar --exclude='./dev/*' --directory=rootfs -xf rootfs.tar.zst
trap "rm -rf rootfs/*" EXIT
mkdir rootfs/build
bwrap --bind rootfs / --dev /dev --proc /proc --bind "$DEBDIR" /build -- /build/stage2.sh
dcmd mv rootfs/tmp/*.changes .
EOF

cat > stage2.sh <<EOF
#!/bin/sh

set -e

if [ -n "\$TMPDIR" ] && ! [ -d "\$TMPDIR" ]; then
   mkdir -p "\$TMPDIR"
   chmod 1777 "\$TMPDIR"
fi

export LC_ALL=C.UTF-8
useradd belenios
cp -a /build/deb /tmp
chown belenios:belenios -R /tmp/deb
cd /tmp/deb
runuser -u belenios -- dpkg-buildpackage -b
EOF

chmod +x *.sh

mkdir deb
cd deb

cp "$BOOTSTRAP" .

mkdir vendor
cp -a "$BELDIR/vendor/opam-overlay" vendor

mkdir debian

DEBNAME="$(git config --get user.name)"
DEBMAIL="$(git config --get user.email)"
DEBDATE="$(date -d@$SOURCE_DATE_EPOCH -R)"
DEBDEPS="$(echo "$DEPS" | sed -r 's/\s+/, /g')"
BINDEPS="$(echo "$DEV_DEPS" | sed -r 's/\s+/, /g')"
OPAM_BINDEPS="$(echo "$OPAM_DEPS" | sed -r 's/\s+/, /g')"

cat > debian/changelog <<EOF
belenios-opam$SUFFIX ($VERSION) belenios-$SUITE; urgency=medium

  * Run opam-bootstrap.sh with BELENIOS_SYSROOT set to /opt/belenios$SUFFIX

 -- $DEBNAME <$DEBMAIL>  $DEBDATE
EOF

cat > debian/control <<EOF
Source: belenios-opam$SUFFIX
Priority: optional
Section: misc
Maintainer: $DEBNAME <$DEBMAIL>
Build-Depends: debhelper-compat (= 13), $DEBDEPS
Standards-Version: 4.7.0
Rules-Requires-Root: no

Package: belenios-opam$SUFFIX
Architecture: all
Depends:
 belenios-opam-dev$SUFFIX,
 belenios-opam-switch$SUFFIX,
 \${misc:Depends}
Description: Belenios opam bootstrap (meta-package)

Package: belenios-opam-dev$SUFFIX
Architecture: any
Depends:
 belenios-opam-runtime$SUFFIX,
 \${perl:Depends},
 \${shlibs:Depends},
 \${misc:Depends},
 $BINDEPS
Description: Belenios opam bootstrap (dev files)

Package: belenios-opam-runtime$SUFFIX
Architecture: any
Depends:
 \${perl:Depends},
 \${shlibs:Depends},
 \${misc:Depends}
Description: Belenios opam bootstrap (runtime files)

Package: belenios-opam-switch$SUFFIX
Architecture: any
Depends:
 \${perl:Depends},
 \${shlibs:Depends},
 \${misc:Depends},
 $OPAM_BINDEPS
Description: Belenios opam bootstrap (opam files)
EOF

cat > debian/rules <<EOF
#!/usr/bin/make -f
# -*- makefile -*-

%:
	dh \$@

override_dh_auto_build:
	if ! [ -d build ]; then \\
	  mkdir -p build/opt; \\
	  BELENIOS_SYSROOT=/opt/belenios$SUFFIX bwrap \\
	    --bind / / --dev /dev --bind build/opt /opt -- \\
	    ./opam-bootstrap.sh $ADDITIONAL_OPAM_PACKAGES; \\
	fi

override_dh_auto_install:
	mkdir -p debian/tmp
	cp -a build/opt debian/tmp
	rm -rf debian/tmp/opt/belenios$SUFFIX/bootstrap/src
	rm -rf debian/tmp/opt/belenios$SUFFIX/cache
	VERSION=\$\$(debian/tmp/opt/belenios$SUFFIX/opam/*/bin/ocamlc -version); \\
	mkdir -p debian/belenios-opam-runtime$SUFFIX/opt/belenios$SUFFIX/opam/\$\$VERSION/lib; \\
	mv debian/tmp/opt/belenios$SUFFIX/opam/\$\$VERSION/lib/findlib.conf \\
	  debian/belenios-opam-runtime$SUFFIX/opt/belenios$SUFFIX/opam/\$\$VERSION/lib; \\
	mkdir -p debian/belenios-opam-dev$SUFFIX/opt/belenios$SUFFIX/opam; \\
	mv debian/tmp/opt/belenios$SUFFIX/opam/\$\$VERSION \\
	  debian/belenios-opam-dev$SUFFIX/opt/belenios$SUFFIX/opam; \\
	mv debian/tmp/opt/belenios$SUFFIX/bootstrap \\
	  debian/belenios-opam-dev$SUFFIX/opt/belenios$SUFFIX; \\
	mkdir -p debian/belenios-opam-switch$SUFFIX; \\
	mv debian/tmp/opt debian/belenios-opam-switch$SUFFIX; \\
	mkdir -p debian/belenios-opam-switch$SUFFIX/opt/belenios$SUFFIX/opam/\$\$VERSION; \\
	mv debian/belenios-opam-dev$SUFFIX/opt/belenios$SUFFIX/opam/\$\$VERSION/.opam-switch \\
	  debian/belenios-opam-switch$SUFFIX/opt/belenios$SUFFIX/opam/\$\$VERSION

override_dh_strip_nondeterminism:

override_dh_dwz:

override_dh_strip:
EOF
chmod +x debian/rules

cd ..
unshare --map-auto -r ./stage1.sh
dcmd mv *.changes "$TARGET"
