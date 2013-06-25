Belenios
========


Compilation
-----------

To run basic command-line tools, you will need:

 * [OCaml](http://caml.inria.fr/)
 * [Findlib](http://projects.camlcity.org/projects/findlib.html)
 * [Zarith](https://forge.ocamlcore.org/projects/zarith/)
 * [Calendar](http://calendar.forge.ocamlcore.org/)
 * [Uuidm](http://erratique.ch/software/uuidm)
 * [Cryptokit](https://forge.ocamlcore.org/projects/cryptokit/)
 * [Atdgen](http://mjambon.com/atdgen)
 * [Yojson](http://mjambon.com/yojson.html)

To generate credentials, you will need:

 * [Node.js](http://nodejs.org/)

On some systems, the Node.js interpreter might be available as either
`node` or `nodejs`, please adapt `stuff/derive_key.js` accordingly.

To run the web server, you will additionally need:

 * [Eliom](http://ocsigen.org/eliom/) version 3

On Debian and its derivatives, you can try the following:

    BELENIOS=`pwd`
    cd /tmp
    equivs-build $BELENIOS/stuff/belenios-deps-minimal.control
    equivs-build $BELENIOS/stuff/belenios-deps-all.control

Then install the chosen `deb`s with `dpkg -i`, followed by `apt-get -f
install` to install missing dependencies.

NOTE: `equiv-build` should not be run from a NFS directory!

There are two Makefile targets corresponding to the two levels of
dependencies: `minimal` (default) and `all`.


Trustee's guide
---------------

To generate a keypair, run:

    make trustee-keygen

It will generate two files, `XXXXXXXX.public` and `XXXXXXXX.private`,
containing respectively the public and the private key. Send the
public key file to the server administrator, and keep the private key
with extreme care.


Credential authority's guide
----------------------------

### Fully anonymous credential generation

To generate the credentials, run:

    ./stuff/credgen.sh --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --count N

where `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX` is the UUID of the
election given by the administrator, and `N` the number of credentials
to generate. It will generate two files, `TTTTTTTTTT.public` and
`TTTTTTTTTT.private`, each with `N` lines. Send the public file as a
whole to the administator, and each line of the private file to each
voter.

You can optionally add a `--dir` option to specify the directory where
these files will be written.

Note that, if you are honest, you must not keep the private file, nor
to whom you sent each individual private credential.

### Credential generation with identity matching

If you have a list of identities in a file `F` with `N` non-empty
lines, one identity per line, you can also run:

    ./stuff/credgen.sh --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --file F

It will create the same `TTTTTTTTTT.public` file as above, with `N`
lines. It will also generate `TTTTTTTTTT.private` with `N` lines, each
one containing a private credential and the identity of whom to send
it to. Additionally, it will create `TTTTTTTTTT.hashed` with `N`
lines, each one containing the hash of a public credential and the
corresponding identity. Only the hashed file is needed to disactivate
the credential of a specific identify; the private file must still be
forgotten once it is used.

Note that, as a safety measure, all output files are written sorted so
that there is no matching between them based on line numbers.

### Checking a private credential

To get the public key derived from a private credential, run:

    ./stuff/credgen.sh --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --derive YYYYYYYYYYYYYYY
