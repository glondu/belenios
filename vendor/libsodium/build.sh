#!/bin/sh

# Prerequisites:
# - Debian testing (2026-01-08)
# - wasm-ld (package lld)
# - wasm-strip (package wabt)

set -e

TMP="$(mktemp -d --tmpdir libsodium.XXXXXX)"
echo "Using $TMP..."

OLD="$PWD"
cd $TMP
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.21-stable.tar.gz
wget https://ziglang.org/download/0.15.2/zig-x86_64-linux-0.15.2.tar.xz
wget https://github.com/bytecodealliance/wasm-micro-runtime/releases/download/WAMR-2.4.4/iwasm-2.4.4-x86_64-ubuntu-22.04.tar.gz
cat > SHA256SUM <<EOF
ec60ff8daed26319dfc4371843c56ac2dfadd20e2218cbbca97aecb8b390b7a8  iwasm-2.4.4-x86_64-ubuntu-22.04.tar.gz
168efc6991e5f133f3c749eee9cc121c49b19ec503127193fc8b5d5eda696188  libsodium-1.0.21-stable.tar.gz
02aa270f183da276e5b5920b1dac44a63f1a49e55050ebde3aecc9eb82f93239  zig-x86_64-linux-0.15.2.tar.xz
EOF
sha256sum --check SHA256SUM
for u in *.tar.*; do tar xf $u; done
( cd libsodium-stable && PATH=$TMP:$TMP/zig-x86_64-linux-0.15.2:$PATH dist-build/wasm32-wasi.sh )
wasm-ld --no-entry --import-undefined \
  --export crypto_core_ed25519_is_valid_point \
  --export crypto_scalarmult_ed25519_noclamp \
  --export crypto_core_ed25519_add \
  --export crypto_core_ed25519_bytes \
  --export crypto_core_ed25519_scalarbytes \
  -o libsodium.wasm libsodium-stable/libsodium-wasm32-wasi/lib/libsodium.a
wasm-strip libsodium.wasm
chmod 644 libsodium.wasm
cp libsodium.wasm "$OLD"

echo "Build tree left in $TMP"
