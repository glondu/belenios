# Developer's notes

## Big numbers on the client side

Belenios's OCaml code uses a
[Zarith](https://github.com/ocaml/Zarith)-based interface for big
number arithmetics, and effectively uses Zarith on the server side.

At its beginnings in 2012, Belenios used bindings to
[JSBN](http://www-cs-students.stanford.edu/~tjw/jsbn/) on the client
side, then started to use native
[BigInt](https://tc39.es/ecma262/#sec-bigint-objects) when available,
with a fallback to JSBN. In December 2024, the JSBN fallback was
removed since it was considered [sufficiently
supported](https://caniuse.com/bigint).

It has been considered (in December 2024) to use
[zarith_stubs_js](https://github.com/janestreet/zarith_stubs_js), but
it was slower by at least 30%. My guess this is due to mimicking
Zarith's strategy with special-casing of small numbers.
