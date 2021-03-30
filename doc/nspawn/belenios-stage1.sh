#!/bin/sh

set -e

SRC="$(dirname $0)"
DIR="$1"

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

echo "Debootstrapping..."

debootstrap --merged-usr --variant=buildd buster "$DIR"
echo 'APT::Install-Recommends "false";' >> "$DIR/etc/apt/apt.conf"

cp "$SRC/belenios-stage2.sh" "$DIR"
cp "$SRC/../../opam-bootstrap.sh" "$DIR"

systemd-nspawn --directory="$DIR" /belenios-stage2.sh

rm -f "$DIR/belenios-stage2.sh" "$DIR/opam-bootstrap.sh"
