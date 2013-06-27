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

On Debian and its derivatives, you can find the list of packages to
install in the `stuff/belenios-deps-*.control` files. You can also
create meta-packages with `equivs` by using the following commands:

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


Server administrator's guide
----------------------------

### Overview of the web server

A sample web server can be run with the `tests/run-server.sh` script.

Here is an excerpt of the sample configuration file:

    <eliom module="_build/src/web/server.cma">
      <log file="_RUNDIR_/log/security.log"/>
      <data dir="tests/data"/>
    </eliom>

The `log` tag indicates a file where some security-sentive events will
be logged. The `data` tag indicates a directory with election
data. This directory must contain one subdirectory per election, and
in each of these directories, the following files are expected:

 * `election.json`: election parameters
 * `metadata.json`: additional parameters that are not published
 * `public_keys.jsons`: public keys of the trustees, one per line
 * `public_creds.txt`: anonymous public credentials, one per line

If a `result.json` file exists, the election is assumed to be finished
and ignored.

The *election fingerprint*, which is shown on the election page and in
the booth, is the compact Base64 encoding of the SHA256 of
`election.json`. It can be computed from a POSIX shell by piping it
into:

    sha256sum | xxd -r -p | base64

During an election, the web server stores dynamic data such as
accepted ballots in Ocsipersist's store.

In the following, we assume `ocsigenserver` is properly configured.

### Setup a new election

 1.  Generate an UUID with the `uuidgen` command.
 2.  Ask each trustee to generate a keypair, and the credential
     authority to generate credentials. Note that the UUID is needed
     to perform credential generation.
 3.  Create a new directory in the data directory. For uniqueness, it
     is suggested to include the UUID in the directory name. In the
     following, we denote by `$DIR` the full path to this directory.
 4.  Go to Belenios source tree (we assume its full path is
     `$BELENIOS`).
 5.  Edit `stuff/template.ml` with the election parameters, and run
     `make all`.
 6.  Go to `$DIR`.
 7.  Concatenate all trustee public keys into a `public_keys.jsons`
     file, and run `$BELENIOS/_build/stuff/template.native`. It should
     generate `election.json` and `metadata.json`. The `ydump` tool
     (from Yojson) can be used to pretty-print them.
 8.  (Optionally) Edit `election.json` and `metadata.json` to your
     convenience.
 9.  Save the file with public credentials generated by the credential
     authority into `public_creds.txt`.
 10. Launch `ocsigenserver`.
