#!/bin/sh

set -e

DIR=/target

if [ -e "$DIR" ]; then
    if [ -d "$DIR" ]; then
        FILES=$(ls -1qAH -- "$DIR")
        if ! [ -z "$FILES" ]; then
        echo "$DIR is not an empty directory!"
        exit 1
        fi
    else
        echo "$DIR exists and is not a directory!"
    fi
fi

if ! [ -d "/home/belenios" ]; then
    echo "This script must be run from a root created with belenios-stage1.sh!"
    exit 1
fi

mkdir -p "$DIR"

cd /

echo "Debootstrapping..."

debootstrap --merged-usr --variant=minbase --include=systemd,dbus buster "$DIR/rootfs"
echo 'APT::Install-Recommends "false";' >> "$DIR/rootfs/etc/apt/apt.conf"

ln -sfT /usr/lib/systemd/resolv.conf "$DIR/rootfs/etc/resolv.conf"
echo belenios > "$DIR/rootfs/etc/hostname"

cat > "$DIR/rootfs/etc/hosts" <<EOF
127.0.0.1 localhost
127.0.1.1 belenios

# The following lines are desirable for IPv6 capable hosts
::1     localhost ip6-localhost ip6-loopback
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters
EOF

echo "Installing Debian prerequisites..."

chroot "$DIR/rootfs" sh -c "apt-get update -qq && apt-get upgrade -qq"
chroot "$DIR/rootfs" sh -c "debconf-set-selections" <<EOF
dma dma/mailname string belenios
dma dma/relayhost string localhost
EOF
chroot "$DIR/rootfs" sh -c "apt-get install -qq libgmp-dev libpcre3-dev libssl-dev libsqlite3-dev ca-certificates zip libncurses-dev zlib1g-dev libgd-securityimage-perl cracklib-runtime dma logrotate"
chroot "$DIR/rootfs" sh -c "apt-get clean"
chroot "$DIR/rootfs" useradd belenios

echo "Copying needed runtime files from opam root..."

for u in \
    bin/ocsigenserver.opt \
    lib/astring \
    lib/atdgen \
    lib/atdgen-runtime \
    lib/bigarray \
    lib/biniou \
    lib/bytes \
    lib/calendar \
    lib/camomile \
    lib/cryptokit \
    lib/csv \
    lib/domain-name \
    lib/dynlink \
    lib/easy-format \
    lib/eliom \
    lib/fileutils \
    lib/findlib \
    lib/findlib.conf \
    lib/fmt \
    lib/gettext \
    lib/gettext-camomile \
    lib/ipaddr \
    lib/js_of_ocaml \
    lib/lwt \
    lib/lwt_log \
    lib/lwt_react \
    lib/lwt_ssl \
    lib/macaddr \
    lib/mmap \
    lib/netstring \
    lib/netstring-pcre \
    lib/netsys \
    lib/ocplib-endian \
    lib/ocsigenserver \
    lib/pcre \
    lib/ppx_deriving \
    lib/re \
    lib/react \
    lib/reactiveData \
    lib/result \
    lib/seq \
    lib/sqlite3 \
    lib/ssl \
    lib/stdlib-shims \
    lib/str \
    lib/threads \
    lib/tyxml \
    lib/uchar \
    lib/unix \
    lib/uutf \
    lib/xml-light \
    lib/yojson \
    lib/zarith \
; do cp -a --parents home/belenios/.belenios/opam/4.11.2/$u "$DIR/rootfs"; done

echo "Copying needed runtime files from belenios source tree..."

cp -a --parents home/belenios/belenios/_run  home/belenios/opam-env.sh "$DIR/rootfs"

echo "Creating remaining runtime files and directories..."

cat > "$DIR/rootfs/etc/logrotate.d/belenios" <<EOF
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
EOF

cat > "$DIR/rootfs/home/belenios/belenios-env.sh" <<EOF
. /home/belenios/opam-env.sh
BELENIOS_CONFIG=/var/belenios/ocsigenserver.conf.in
BELENIOS_VARDIR=/var/belenios
BELENIOS_RUNDIR=/tmp/belenios
BELENIOS_BINDIR=/home/belenios/belenios/_run/usr/bin
BELENIOS_LIBDIR=/home/belenios/belenios/_run/usr/lib
BELENIOS_SHAREDIR=/home/belenios/belenios/_run/usr/share/belenios-server
EOF

cat > "$DIR/rootfs/etc/systemd/system/belenios.service" <<EOF
[Unit]
Description=Belenios election server
After=network.target

[Service]
ExecStart=/home/belenios/belenios/_run/usr/bin/belenios-start-server --preload /home/belenios/belenios-env.sh
ExecStop=/home/belenios/belenios/_run/usr/bin/belenios-stop-server --preload /home/belenios/belenios-env.sh
TimeoutStopSec=15
User=belenios

[Install]
WantedBy=multi-user.target
EOF

ln -sfT /etc/systemd/system/belenios.service "$DIR/rootfs/etc/systemd/system/multi-user.target.wants/belenios.service"

mkdir "$DIR/rootfs/var/belenios"
chroot "$DIR/rootfs" chown belenios:belenios /var/belenios

echo "Creating rootfs.squashfs..."

mksquashfs "$DIR/rootfs" "$DIR/rootfs.squashfs"
