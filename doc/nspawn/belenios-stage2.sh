#!/bin/sh

set -e

echo "Installing Debian prerequisites..."

apt-get update -qq && apt-get upgrade -qq
apt-get install -qq netbase build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates zip unzip libncurses-dev zlib1g-dev libgd-securityimage-perl cracklib-runtime git jq npm rsync debootstrap squashfs-tools
apt-get clean

echo "Running opam-bootstrap.sh..."

useradd --create-home belenios
su - belenios -c "env BELENIOS_OPAM_INIT_ARGS=--disable-sandboxing /opam-bootstrap.sh"
su - belenios -c "head -n2 env.sh > opam-env.sh && . ./opam-env.sh && opam env >> opam-env.sh"
