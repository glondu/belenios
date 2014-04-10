Belenios
========


Introduction
------------

Belenios is a verifiable voting system that partly implements the
Helios-C protocol described [here](http://eprint.iacr.org/2013/177),
which is itself derived from [Helios](http://vote.heliosvoting.org).

It consists of a command-line tool used to perform administrative
tasks related to elections, as well as verifications, and a web server
used to collect ballots from voters. Voters can vote directly from
their web browser.

A specification is provided in doc/specification.tex.

Compilation instructions are provided in INSTALL.md.


Voter's guide
-------------

During an election, you should have access to the following files:

 * `election.json`: election parameters
 * `public_keys.jsons`: trustees' public keys
 * `public_creds.txt`: the public keys associated to valid credentials
 * `ballots.jsons`: accepted ballots

Note that the last two are dynamic, and evolve during the election. At
the end of the election, they are frozen and a `result.json` file will
be published.

If you put these files in a directory `/path/to/election`, the following
command will perform all possible verifications, depending on existing
files:

    belenios-tool verify --dir /path/to/election

For example, during the election, you can check if some candidate
ballot is acceptable by putting it alone in `ballots.jsons`, and
running the command above.

If you put your secret credential in a file `/path/to/credential` and
your choices in a file `/path/to/choices.json` (as an array of arrays
of 0/1 in JSON format), the following command will output a ballot
that can be directly submitted:

    belenios-tool vote --dir /path/to/election --privcred /path/to/credential --ballot /path/to/choices.json


Trustee's guide
---------------

### Key generation

To generate a keypair, run:

    belenios-tool trustee-keygen

It will generate two files, `XXXXXXXX.public` and `XXXXXXXX.private`,
containing respectively the public and the private key. Send the
public key file to the server administrator, and keep the private key
with extreme care.

### Partial decryption

To compute your decryption share, set `/path/to/election` up as
described in the _Voter's guide_ section above, and run:

    belenios-tool decrypt --dir /path/to/election --privkey /path/to/privkey > partial_decryption.json

and send `partial_decryption.json` to the election
administrator.

Note: be sure to authenticate all your input files when you use your
private key!


Credential authority's guide
----------------------------

### Fully anonymous credential generation

To generate the credentials, run:

    belenios-tool credgen --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --count N

where `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX` is the UUID of the
election given by the administrator, and `N` the number of credentials
to generate. It will generate two files, `TTTTTTTTTT.public` and
`TTTTTTTTTT.private` (where `TTTTTTTTTT` is a timestamp), each with
`N` lines. Send the public file as a whole to the administator, and
each line of the private file to each voter.

You can optionally add a `--dir` option to specify the directory where
these files will be written.

Note that, if you are honest, you must not keep the private file, nor
to whom you sent each individual private credential.

### Credential generation with identity matching

If you have a list of identities in a file `F` with `N` non-empty
lines, one identity per line, you can also run:

    belenios-tool credgen --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --file F

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

    belenios-tool credgen --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --derive YYYYYYYYYYYYYYY


Election organizer's guide
--------------------------

Once the server is up and running (see next section), anyone who can
log into the server can create an election, with the help of the
command-line tool. The interface is very rudimentary at the moment,
and it is not possible to edit or destroy an election once it has been
accepted by the server.

### Setup a new election

 1.  Generate an UUID with the `uuidgen` command. Let it be `$UUID`.
 2.  Go to an empty directory. In the following, we denote by `$DIR`
     the full path to this directory and by `$BELENIOS` the full path
     to the Belenios source tree.
 4.  Ask the credential authority to generate credentials. Note that
     `$UUID` is needed for that. Save the file with public
     credentials into `$DIR/public_creds.txt`.
 5.  Ask each trustee to generate a keypair. Concatenate all trustee
     public keys into a `$DIR/public_keys.jsons` file.
 6.  Edit `$BELENIOS/demo/templates/election.json`.
 7.  Run: `belenios-tool mkelection --uuid $UUID --group
     $BELENIOS/demo/groups/default.json --template
     $BELENIOS/demo/templates/election.json`. It should generate
     `election.json` in the current directory.
 8.  Create a `$DIR/metadata.json` file. Its format is currently
     undocumented and subject to future evolution, but you can get
     inspiration from `$BELENIOS/demo/data/*/metadata.json`.
 9.  Open a browser, go to the _Administer elections_ page (link at
     the bottom), log in, and upload all the files created in the
     previous steps.

### Election administration

Each election has its own administration page and authentication
configuration. An election owner has to log in from the site
administration page (accessible from the link at the bottom of pages)
to be able to access the administration page specific to the election.

### Tally

 1. Go to the election directory, which must contain `election.json`,
    `public_keys.jsons`, `public_creds.txt` and `ballots.jsons`.
    `ballots.jsons` must be downloaded from the running webserver,
    as well as `public_creds.txt` if some credentials were changed
    during the election.
 2. Concatenate the `partial_decryption.json` received from each
    trustee into a `partial_decryptions.jsons`, in the same order as in
    `public_keys.jsons`.
 3. Run `belenios-tool finalize`.  It will create
    `result.json`. Publish this file, along with the files listed in
    the first step above. The whole set will enable universal
    verifiability.

Note: `partial_decryptions.jsons` is a temporary file whose contents
is embedded in `result.json`, so there is no need to keep it.


Server administrator's guide
----------------------------

A sample web server can be run with the `demo/run-server.sh` script,
from the compiled source tree.

Here is an excerpt of the sample configuration file:

    <eliom module="_build/src/web/server.cma">
      <auth name="demo"><dummy/></auth>
      <auth name="local"><password db="demo/password_db.csv"/></auth>
      <source file="../belenios.tar.gz"/>
      <log file="_RUNDIR_/log/security.log"/>
      <import dir="demo/data"/>
      <spool dir="_RUNDIR_/spool"/>
    </eliom>

`<auth>` elements configure authentication for the whole
site. Available authentication methods:

 * `<dummy>`: just asks for a name. No security is intended. This is
   useful for debugging or demonstration purposes but obviously not
   suitable for production
 * `<password>`: password-based authentication. It takes as parameter
   a file, in CSV format, where each line consists of:
    + a user name
    + a salt
    + SHA256(salt concatenated with password)

   Additional fields are ignored. In the sample `password_db.csv`
   file, a fourth field with the plaintext password is included. The
   sample file has been generated with the following shell command:

   `for u in $(seq 1 5); do SALT=$(pwgen); PASS=$(pwgen); echo "user$u,$SALT,$(echo -n "$SALT$PASS" | sha256sum | read a b; echo $a),$PASS"; done`

 * `<cas>`: authenticate with a [CAS](http://www.jasig.org/cas)
   server. For example:

   `<auth name="example"><cas server="https://cas.example.com/cas"/></auth>`

   If the web server is behind a reverse-proxy, it might be needed to
   rewrite URLs passed to the CAS server. This can be done with the
   following directive:

   `<rewrite-prefix src="https://backend-server" dst="https://frontend-server/belenios"/>`

The `<source>` element gives the path to the source tarball. Note that
this is a path on the local filesystem and not a URL. If you made
local changes, an easy way to comply with the AGPL license is to
commit them in a local git checkout, and put in the `source` element
the path to the tarball generated by `make archive`.

If a `<main-election>` element is present, the home page will be a
redirection to the referenced election, and not a mere listing of
featured elections. This is useful if there is a single
election. Others elections (if any) are still accessible, though.

The `<log>` element indicates a file where some security-sentive
events will be logged. It is optional.

The `<spool>` element indicates a directory with election data. This
directory should be empty when the server is launched for the first
time, and will be populated with election data. A typical location
would be `/var/spool/belenios`.

The `<import>` element indicates a directory from where elections will
be imported when the server is launched for the first time. The
directory referenced here must contain an `index.json` file. Each
election subdirectory must contain the following files:

 * `election.json`: election parameters
 * `metadata.json`: additional parameters that are not published
 * `public_keys.jsons`: public keys of the trustees, one per line
 * `public_creds.txt`: anonymous public credentials, one per line

The *election fingerprint*, which is shown on the election page and in
the booth, is the compact Base64 encoding of the SHA256 of
`election.json`. It can be computed from a POSIX shell by piping it
into:

    sha256sum | xxd -r -p | base64


Legal
-----

### Internal code

By "internal code", we mean everything that is not in the `ext/`
directory.

Copyright © 2012-2014 Inria

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version, with the additional
exemption that compiling, linking, and/or using OpenSSL is allowed.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

### External code

The booth (web-based voting client) has been imported from Helios and
adapted for our needs. It uses code from many origins; please refer to
each file for accurate copyright and licensing information.
