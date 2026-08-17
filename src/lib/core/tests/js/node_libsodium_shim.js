// BELENIOS
// Copyright © 2026 VCAST, uses libsodium by Frank Denis

if (typeof WebAssembly !== "undefined" && typeof require !== "undefined") {
    (function () {
        var fs = require("fs");
        var data = fs.readFileSync("../../../../../vendor/libsodium/libsodium.wasm");
        var env = {memset: belenios_platform.memset};
        var importObject = {env: env};
        var module = new WebAssembly.Module(data);
        var instance = new WebAssembly.Instance(module, importObject);
        var raw = instance.exports;
        var base = 0x10000 * raw.memory.grow(1);
        var buffer = new Uint8Array(raw.memory.buffer);
        belenios_platform.libsodium = {
            bytes: raw.crypto_core_ed25519_bytes,
            scalarbytes: raw.crypto_core_ed25519_scalarbytes,
            is_valid_point: raw.crypto_core_ed25519_is_valid_point,
            scalarmult: raw.crypto_scalarmult_ed25519_noclamp,
            add: raw.crypto_core_ed25519_add,
            base: base,
            buffer: buffer
        };
    })();
}
