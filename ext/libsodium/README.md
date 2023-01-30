Ed25519 group arithmetic in WebAssembly and JavaScript
======================================================

This directory contains an implementation of
[libsodium](https://www.libsodium.org) stubs required by Belenios to
run in the browser, for the Ed25519 group. It uses a WebAssembly build
of libsodium; a [build script](build.sh) is provided for reference and
completeness.

These stubs are optional; when missing, Belenios falls back to a pure
OCaml implementation of Ed25519.
