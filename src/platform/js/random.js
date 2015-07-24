function init_prng () {
    // Start SJCL built-in collectors
    sjcl.random.startCollectors();

    // Seed from window.crypto if present
    var cryptoObj = window.crypto || window.msCrypto; // for IE 11
    if (cryptoObj) {
        var bytes = new Uint32Array(4);
        cryptoObj.getRandomValues(bytes);
        sjcl.random.addEntropy(bytes[0], 32);
        sjcl.random.addEntropy(bytes[1], 32);
        sjcl.random.addEntropy(bytes[2], 32);
        sjcl.random.addEntropy(bytes[3], 32);
        if (console) {
            console.log("PRNG successfully initialized using crypto object");
        }
    } else {
        alert("The random number generator could not be safely initialized. You should use a more modern browser.");
    }
}

init_prng();
