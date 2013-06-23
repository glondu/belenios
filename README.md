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

 * [The SpiderMonkey JavaScript shell](https://developer.mozilla.org/en-US/docs/SpiderMonkey)

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

To generate the credentials, run:

    ./stuff/credgen.sh --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --count NN

where XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX is the UUID of the election
given by the administrator, and NN the number of credentials to
generate. It will generate two files,
`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.public` and
`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.private`, each with NN lines. Send
the public file as a whole to the administator, and each line of the
private file to each voter.

Note that, if you are honest, you must not keep the private file, nor
to whom you sent each individual private credential.
