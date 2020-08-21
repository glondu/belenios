function init_prng () {
    var random = belenios.sjcl.random;

    // Start SJCL built-in collectors
    random.startCollectors();

    // Seed from window.crypto if present
    var cryptoObj = window.crypto || window.msCrypto; // for IE 11
    if (cryptoObj) {
        var n = 8;
        var bytes = new Uint32Array(n);
        cryptoObj.getRandomValues(bytes);
        for (var i = 0; i < n; i++) {
            random.addEntropy(bytes[i], 32);
        }
        if (console) {
            console.log("PRNG successfully initialized using crypto object");
        }
    }
}

init_prng();
