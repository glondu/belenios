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
    var n = uri.length;
    // For pages below static/, base is the directory just above
    for (var i = n - 1; i >= 0; i--) {
        if (uri[i] === "/") {
            if (uri.slice(i + 1, n) === "static") {
                return uri.slice(0, i);
            } else {
                n = i;
            }
        }
    }
    // We assume that pages that are not below static/ are
    // server-generated and one level below the root
    return "..";
}

const base = compute_base(location.href);

if (typeof WebAssembly !== "undefined" && WebAssembly.instantiateStreaming) {
    // TODO: use directly WebAssembly.instantiateStreaming(fetch("libsodium.wasm"), importObject).then(...)
    // when ocaml-magic-mime supports wasm (https://github.com/mirage/ocaml-magic-mime/issues/27)
    fetch(base + "/static/libsodium.wasm").then(function (response) {
        if (response.ok) {
            response.arrayBuffer().then(function (buffer) {
                var env = {memset: memset};
                var importObject = {env: env};
                WebAssembly.instantiate(buffer, importObject).then(function (obj) {
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
            });
        }
    });
}
