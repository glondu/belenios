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

    belenios-tool election --dir /path/to/election verify

For example, during the election, you can check if some candidate
ballot is acceptable by putting it alone in `ballots.jsons`, and
running the command above.

If you put your secret credential in a file `/path/to/credential` and
your choices in a file `/path/to/choices.json` (as an array of arrays
of 0/1 in JSON format), the following command will output a ballot
that can be directly submitted:

    belenios-tool election --dir /path/to/election --privkey /path/to/credential vote /path/to/choices.json


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

    belenios-tool election --dir /path/to/election --privkey /path/to/privkey decrypt > partial_decryption.json

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


Server administrator's guide
----------------------------

### Overview of the web server

A sample web server can be run with the `demo/run-server.sh` script,
from the compiled source tree.

Here is an excerpt of the sample configuration file:

    <eliom module="_build/src/web/server.cma">
      <enable-dummy/>
      <enable-password db="demo/data/password_db.csv"/>
      <source file="../belenios.tar.gz"/>
      <main-election uuid="XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"/>
      <admin hash="XXX"/>
      <log file="_RUNDIR_/log/security.log"/>
      <data dir="demo/data"/>
    </eliom>

The `<enable-dummy>` tag enables the dummy authentication method,
which just asks for a name. No security is intended. This is useful
for debugging or demonstration purposes but obviously not suitable for
production. To disable it, just remove the tag.

The `<enable-password>` tag enables password-based authentication. It
takes as parameter a file, in CSV format, where each line consists of:

 * a user name
 * a salt
 * SHA256(salt concatenated with password)

Additional fields are ignored. In the sample `password_db.csv` file, a
fourth field with the plaintext password is included. The sample file
has been generated with the following shell command:

    for u in `seq 1 5`; do SALT=`pwgen`; PASS=`pwgen`; echo "user$u,$SALT,$(echo -n "$SALT$PASS" | sha256sum | read a b; echo $a),$PASS"; done

There is also the possibility to authenticate with a
[CAS](http://www.jasig.org/cas) server. To do that, use the
`<enable-cas>` instead of other `<enable-*>` tags:

    <enable-cas server="https://cas.example.org"/>

The `<source>` tag gives the path of the source tarball. Note that this
is a path on the local filesystem and not a URL. If you made local
changes, an easy way to comply with the AGPL license is to commit them
in a local git checkout, and put in the `source` tag the path to the
tarball generated by `make archive`.

If the `<main-election>` tag is present, the home page will be a
redirection to the referenced election, and not a mere listing of open
elections. This is useful if there is a single election.

The `<admin>` tag indicates the SHA256 of the admin password.

The `<log>` tag indicates a file where some security-sentive events will
be logged. It is optional.

The `<data>` tag indicates a directory with election data. This
directory must contain one subdirectory per election, and in each of
these directories, the following files are expected:

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

 1.  Generate an UUID with the `uuidgen` command. Name it `$UUID`.
 2.  Create a new directory in the data directory. For uniqueness, it
     is suggested to include `$UUID` in the directory name. In the
     following, we denote by `$DIR` the full path to this directory
     and by `$BELENIOS` the full path to the Belenios source tree.
 3.  Go to `$DIR`.
 4.  Ask the credential authority to generate credentials. Note that
     `$UUID` is needed for that. Save the file with public
     credentials into `public_creds.txt`.
 5.  Ask each trustee to generate a keypair. Concatenate all trustee
     public keys into a `public_keys.jsons` file.
 6.  Edit `$BELENIOS/demo/templates/election.json`.
 7.  Run: `belenios-tool mkelection --uuid $UUID --group
     $BELENIOS/demo/groups/default.json --template
     $BELENIOS/demo/templates/election.json`. It should generate
     `election.json` in the current directory.
 8.  (Optionally) Copy `$BELENIOS/demo/templates/metadata.json` to
     `$DIR` and edit the dates in it.
 9.  Go to `$BELENIOS`.
 10. Edit `demo/ocsigenserver.conf.in`, in particular:
      * the admin password
      * the UUID of the main election
      * the data directory (see above)
 11. Run `demo/run-server.sh`. This script is a wrapper around
     `ocsigenserver` and takes the same options. In particular, the
     `-V` option is useful for debugging and `--help` can be used to
     get help.

### Update a credential

 1. Go to `/login-admin` on the live server and log in using the admin
    password.
 2. Go to `/election/update-cred?uuid=UUID`, and fill in the form.

### Tally

 1. Go to the election directory, which must contain `election.json`,
    `public_keys.jsons`, `public_creds.txt` and `ballots.jsons`.
    `ballots.jsons` must be downloaded from the running webserver,
    as well as `public_creds.txt` if some credentials were changed
    during the election.
 2. Concatenate the `partial_decryption.json` received from each
    trustee into a `partial_decryptions.jsons`, in the same order as in
    `public_keys.jsons`.
 3. Run `belenios-tool election finalize`.  It will create
    `result.json`. Publish this file, along with the files listed in
    the first step above. The whole set will enable universal
    verifiability.

Note: `partial_decryptions.jsons` is a temporary file whose contents
is embedded in `result.json`, so there is no need to keep it.


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
