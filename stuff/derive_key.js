#!/usr/bin/env nodejs

// Hack taken from http://stackoverflow.com/questions/5625569/include-external-js-file-in-node-js-app

var fs = require('fs');
var vm = require('vm');
var includeInThisContext = function(path) {
    var code = fs.readFileSync(path);
    vm.runInThisContext(code, path);
}.bind(this);

// Use SJCL to derive the key

includeInThisContext(__dirname+"/../media/booth/js/jscrypto/sjcl.js");

var seed = process.argv[2];
var salt = process.argv[3];
console.log(sjcl.codec.hex.fromBits(sjcl.misc.pbkdf2(seed, sjcl.codec.hex.toBits(salt), 1000, 256)));
