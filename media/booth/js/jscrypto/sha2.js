function hex_sha256(s) {
    return sjcl.codec.hex.fromBits(sjcl.hash.sha256.hash(s));
}

function b64_sha256(s) {
    return sjcl.codec.base64.fromBits(sjcl.hash.sha256.hash(s), true);
}
