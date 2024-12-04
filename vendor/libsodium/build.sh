#!/bin/sh

# Prerequisites:
# - Debian testing (2024-12-04)
# - wasm-ld (package lld)
# - wasm-strip (package wabt)

set -e

TMP="$(mktemp -d --tmpdir libsodium.XXXXXX)"
echo "Using $TMP..."

OLD="$PWD"
cd $TMP
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.20-stable.tar.gz
wget https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz
wget https://github.com/bytecodealliance/wasm-micro-runtime/releases/download/WAMR-2.2.0/iwasm-2.2.0-x86_64-ubuntu-22.04.tar.gz
cat > SHA256SUM <<EOF
86b2f81c0d88aeb7982051861129c28950071a2d3b7538b59782f8801c7364a0  iwasm-2.2.0-x86_64-ubuntu-22.04.tar.gz
94fda0763264decb3b4f4bcd060588e81681616eecadfc00864c30f512ea1ada  libsodium-1.0.20-stable.tar.gz
d45312e61ebcc48032b77bc4cf7fd6915c11fa16e4aad116b66c9468211230ea  zig-linux-x86_64-0.13.0.tar.xz
EOF
sha256sum --check SHA256SUM
for u in *.tar.*; do tar xf $u; done
( cd libsodium-stable && PATH=$TMP:$TMP/zig-linux-x86_64-0.13.0:$PATH dist-build/wasm32-wasi.sh )
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
