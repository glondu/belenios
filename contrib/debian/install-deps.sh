#!/bin/sh

# This script install all Debian dependencies of Belenios

set -e

. "$(dirname "$0")/deps.sh"

apt-get install -qq debhelper dh-ocaml $BELENIOS_DEVDEPS $BELENIOS_DEBDEPS
