// BELENIOS
// Copyright © 2023 Inria, uses libsodium by Frank Denis
// See "LICENSE" for details.

function memset(dst, c, n) {
    var limit = dst + n;
    for (var i = dst; i < limit; i++) {
        belenios.libsodium.buffer[i] = c;
    }
}

function compute_base(uri) {
    var i = uri.lastIndexOf("/static/");
    i = (i >= 0) ? i : uri.lastIndexOf("/actions/");
    if (i >= 0) {
        // For pages below /static/ or /actions/, base is the directory
        // just above
        return uri.slice(0, i + 1);
    } else {
        // We assume that other pages are directly at the root
        return "";
    }
}

const base = compute_base(location.href);

if (typeof WebAssembly !== "undefined" && WebAssembly.instantiateStreaming) {
    var env = {memset: memset};
    var importObject = {env: env};
    WebAssembly.instantiateStreaming(fetch(base + "static/libsodium.wasm"), importObject).then(function (obj) {
        var raw = obj.instance.exports;
        var base = 0x10000 * raw.memory.grow(1);
        var buffer = new Uint8Array(raw.memory.buffer);
        belenios.libsodium = {
            bytes: raw.crypto_core_ed25519_bytes,
            scalarbytes: raw.crypto_core_ed25519_scalarbytes,
            is_valid_point: raw.crypto_core_ed25519_is_valid_point,
            scalarmult: raw.crypto_scalarmult_ed25519_noclamp,
            add: raw.crypto_core_ed25519_add,
            base: base,
            buffer: buffer
        };
    });
}
