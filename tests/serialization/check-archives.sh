#!/bin/bash

# Check that every object of the given .bel archives is re-encoded to the
# same bytes (see roundtrip.ml).

set -eo pipefail

RT="$(dirname "$0")/../../_build/default/tests/serialization/roundtrip.exe"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

for bel in "$@"; do
    rm -rf "$TMP/x"
    mkdir "$TMP/x"
    tar -xf "$bel" -C "$TMP/x"
    "$RT" "$TMP/x" | sed "s|^$TMP/x|$bel|"
done
