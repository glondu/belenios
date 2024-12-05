// BELENIOS
// Copyright © 2012-2024 Inria

// The following is an implementation of big integer operations needed by Belenios

function getNative () {
    var ZERO = BigInt(0);
    var ONE = BigInt(1);
    var windowsize = 6;
    var windowmask = (1 << windowsize) - 1;
    var windowmaskBI = BigInt(windowmask);
    function bitLength(n) {
        var r = 0;
        while (n > ZERO) {
            r++;
            n = n >> ONE;
        }
        return r;
    }
    function extEuclide(a, b) {
        var r0 = a, r1 = b;
        var s0 = ONE, s1 = ZERO;
        var t0 = s1, t1 = s0;
        var r, q, s, t;
        while (r = r0 % r1) {
            q = (r0 - r) / r1;
            r0 = r1; r1 = r;
            s = s0 - s1 * q; s0 = s1; s1 = s;
            t = t0 - t1 * q; t0 = t1; t1 = t;
        }
        return [r1, s1, t1];
    }
    return {
        ZERO: ZERO,
        ONE: ONE,
        ofInt: function(n) { return BigInt(n); },
        ofString: function(n) { return BigInt(n); },
        ofHex: function(n) { return BigInt("0x" + n); },
        add: function(a, b) { return a + b; },
        subtract: function(a, b) { return a - b; },
        multiply: function(a, b) { return a * b; },
        divide: function(a, b) { return a / b; },
        mod: function(a, b) { return a % b; },
        toInt: function(n) { return Number(n); },
        toString: function(n) { return n.toString(); },
        toHex: function(n) { return n.toString(16); },
        compare: function(a, b) {
            if (a < b) return -1;
            if (a > b) return 1;
            return 0;
        },
        modPow: function(a, b, p) {
            var T = new Array();
            T[0] = ONE;
            T[1] = a;
            for (var i = 2; i < windowmask; i += 2) {
                var z = T[i / 2];
                T[i] = (z * z) % p;
                T[i+1] = (T[i] * a) % p;
            }
            var S = T[0];
            for (var i = Math.ceil(bitLength(b) / windowsize) - 1; i >= 0; i--) {
                var z = (b >> BigInt(i * windowsize)) & windowmaskBI;
                S = (S * T[Number(z)]) % p;
                if (i) {
                    for (var j = 0; j < windowsize; j++) {
                        S = (S * S) % p;
                    }
                }
            }
            return S;
        },
        modInverse: function(a, m) {
            if (m == ZERO) throw "modInverse called with modulus zero";
            var b = extEuclide(a, m);
            if (b[0] != ONE) throw "modInverse called on not-coprime numbers";
            var r = b[1] % m; if (r < ZERO) r += m;
            return r;
        },
        bitLength: bitLength,
        shiftLeft: function(a, b) { return a << BigInt(b); },
        shiftRight: function(a, b) { return a >> BigInt(b); },
        and: function(a, b) { return a & b; },
        or: function(a, b) { return a | b; },
        xor: function(a, b) { return a ^ b; }
    };
}

var hasNativeBigInt = typeof BigInt !== 'undefined';

if (hasNativeBigInt) {
    belenios.BigIntCompat = getNative();
} else {
    alert("Missing BigInt feature!");
}
