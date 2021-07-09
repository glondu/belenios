#!/bin/sh

# This script is meant to be run from within ocaml/opam Docker image

# FIXME: temporary, until one of:
#   https://github.com/ocsigen/ocsigenserver/issues/201
#   https://github.com/mirage/ocaml-magic-mime/issues/21
# is fixed

set -e

cd /home/opam

opam install magic-mime

DIR="$(ls -d .opam/*/.opam-switch/sources/magic-mime.*)"
BASE="${DIR##*/}"

if [ ! -d "$BASE" ]; then
    cp -a "$DIR" .
    sed -i 's@application/javascript.*@application/javascript\tjs mjs@' "$BASE/mime.types"
    opam pin --yes magic-mime "$PWD/$BASE"
fi
