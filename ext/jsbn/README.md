Big number arithmetic in JavaScript
===================================

This directory contains an implementation of the big number arithmetic
needed by Belenios, in the file `BigIntCompat.js`. It uses
[BigInt](https://tc39.es/ecma262/#sec-bigint-objects) if it is
available, and falls back to
[JSBN](http://www-cs-students.stanford.edu/~tjw/jsbn/) otherwise.
