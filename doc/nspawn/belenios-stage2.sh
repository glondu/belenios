#!/bin/sh

set -e

echo "Installing Debian prerequisites..."

apt-get update -qq && apt-get upgrade -qq
apt-get install -qq netbase build-essential libgmp-dev libsodium-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates zip unzip libncurses-dev zlib1g-dev libgd-securityimage-perl cracklib-runtime git jq npm rsync debootstrap squashfs-tools
apt-get clean

echo "Creating belenios user..."

useradd --create-home belenios
