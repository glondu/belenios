#!/bin/sh

WASM_BASE64="$(base64 --wrap=0 < libsodium.wasm)"
sed "s,@WASM@,$WASM_BASE64," libsodium.in.js
