#!/bin/sh

# Prerequisites:
# - Debian testing (2023-01-30)
# - wasm-ld (package lld)
# - wasm-strip (package wabt)

set -e

TMP="$(mktemp -d --tmpdir libsodium.XXXXXX)"

OLD="$PWD"
cd $TMP
wget https://download.libsodium.org/libsodium/releases/libsodium-1.0.18-stable.tar.gz
wget https://ziglang.org/download/0.10.1/zig-linux-x86_64-0.10.1.tar.xz
wget https://github.com/bytecodealliance/wasm-micro-runtime/releases/download/WAMR-1.1.2/iwasm-1.1.2-x86_64-ubuntu-22.04.tar.gz
cat > SHA256SUM <<EOF
ca18fc52245650304ce419a66d317dc56bbe8b33c621fcbc90e7fba937afbbda  iwasm-1.1.2-x86_64-ubuntu-22.04.tar.gz
edba9dd57b03cfd95d677709204bb13666ac1336c76c79bca8a51b15a1ccf3ac  libsodium-1.0.18-stable.tar.gz
6699f0e7293081b42428f32c9d9c983854094bd15fee5489f12c4cf4518cc380  zig-linux-x86_64-0.10.1.tar.xz
EOF
sha256sum --check SHA256SUM
for u in *.tar.*; do tar xf $u; done
( cd libsodium-stable && PATH=$TMP:$TMP/zig-linux-x86_64-0.10.1:$PATH dist-build/wasm32-wasi.sh )
wasm-ld --no-entry --import-undefined \
  --export crypto_core_ed25519_is_valid_point \
  --export crypto_scalarmult_ed25519_noclamp \
  --export crypto_core_ed25519_bytes \
  --export crypto_core_ed25519_scalarbytes \
  -o libsodium.wasm libsodium-stable/libsodium-wasm32-wasi/lib/libsodium.a
wasm-strip libsodium.wasm
chmod 644 libsodium.wasm
cp libsodium.wasm "$OLD"

echo "Build tree left in $TMP"
