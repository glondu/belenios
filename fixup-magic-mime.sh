#!/bin/sh

# FIXME: temporary, until one of:
#   https://github.com/ocsigen/ocsigenserver/issues/201
#   https://github.com/mirage/ocaml-magic-mime/issues/21
# is fixed

set -e

cd $OPAMROOT/..

if [ ! -d magic-mime.1.1.3 ]; then
    cp -a opam/4.11.2/.opam-switch/sources/magic-mime.1.1.3 .
    sed -i 's@application/javascript.*@application/javascript js mjs@' magic-mime.1.1.3/mime.types
    opam pin --yes magic-mime $PWD/magic-mime.1.1.3
fi
