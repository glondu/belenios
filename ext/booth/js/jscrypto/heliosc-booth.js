/*
  Copyright © 2012-2014 Inria. All rights reserved.
  Author: Stéphane Glondu <Stephane.Glondu@inria.fr>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

"use strict";

// Significant Helios-C-specific code is inside the heliosc.* namespace
var heliosc = {};

heliosc.booth = {};

heliosc.booth.derive_key = function (election) {
    var n58 = BigInt.fromInt(58);
    var n53 = BigInt.fromInt(53); // previous_prime(58)
    var digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    var salt = sjcl.codec.hex.toBits(election.uuid.replace(/-/g, ""));

    // Check that a secret credential is well formed
    function check(s) {
        var n = s.length-1, res = BigInt.ZERO;
        if (n < 14) return false; // < 82 bits of security
        var i, j;
        // main
        for (i = 0; i < n; i++) {
            j = digits.indexOf(s[i]);
            if (j < 0) return false; // bad digit
            res = res.multiply(n58).add(BigInt.fromInt(j));
        }
        // checksum
        j = digits.indexOf(s[i]);
        if (j < 0) return false; // bad digit
        var checksum = res.add(BigInt.fromInt(j));
        if (!checksum.mod(n53).equals(BigInt.ZERO)) return false; // bad checksum
        return true;
    }

    // Derive a Schnorr keypair from the secret credential
    function derive_key(secret) {
        if (check(secret)) {
            var hexkey = sjcl.codec.hex.fromBits(sjcl.misc.pbkdf2(secret, salt, 1000, 256));
            var pk = election.public_key;
            var x = (new BigInt(hexkey, 16)).mod(pk.q);
            var y = pk.g.modPow(x, pk.p);
            return { x: x, y: y };
        } else {
            throw "Bad token";
        }
    }

    return derive_key;
}

HELIOS.EncryptedVote.prototype.doSignature = function(cred) {
    // Schnorr signature
    var pk = this.election.public_key;
    var w = Random.getRandomInteger(pk.q);
    var commitment = pk.g.modPow(w, pk.p);
    var prefix = "sig|" + cred.y.toJSONObject() + "|" + commitment.toJSONObject() + "|";
    var challenge = (new BigInt(hex_sha256(prefix + _(this.encrypted_answers).map(function(ea) {
        return ElGamal.stringify_choices(ea.choices);
    }).join(",")), 16)).mod(pk.q);
    // we do (q-x*challenge)+w instead of directly w-x*challenge,
    // in case mod doesn't support negative numbers as expected
    var response = pk.q.subtract(cred.x.multiply(challenge).mod(pk.q));
    response = response.add(w).mod(pk.q);
    // hugly hijack of the DLogProof datatype...
    this.signature = new ElGamal.DLogProof(challenge, response);
    this.signature.public_key = cred.y;
}
