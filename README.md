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

To run the web server, you will additionally need:

 * [Eliom](http://ocsigen.org/eliom/) version 3

On Debian and its derivatives, you can try the following:

    BELENIOS=`pwd`
    cd /tmp
    equivs-build $BELENIOS/stuff/belenios-deps-minimal.control
    equivs-build $BELENIOS/stuff/belenios-deps-all.control

Then install the chosen `deb`s with `dpkg -i`, followed by `aptitude`
to install missing dependencies.

NOTE: `equiv-build` should not be run from a NFS directory!

There are two Makefile targets corresponding to the two levels of
dependencies: `minimal` (default) and `all`.


Trustee's guide
---------------

To generate a keypair, run:

    make trustee-keypair

It will generate two files, `XXXXXXXX.public` and `XXXXXXXX.private`,
containing respectively the public and the private key. Send the
public key file to the server administrator, and keep the private key
with extreme care.
