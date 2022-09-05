Belenios Tool
=============


Introduction
------------

`belenios-tool` is a command-line tool that can be used to perform
administrative tasks related to elections, as well as verifications.
If you do not wish to use the provided web server, a whole election
can be organized using this tool. As an illustration of that, you can
have a look at the `tests/tool/demo.sh` script that simulates an
election.

This file documents how to use `belenios-tool`, from the point of view
of the various roles involved in an election. You can also run it with
the `--help` option to get more information.


Auditor's guide
---------------

Note that anyone can be an auditor. Everyone who plays a specific role
in an election should start by auditing the election data.

During an election, you should have access to a file `$UUID.bel`.

Note that the file is dynamic, and evolves (in an append-only fashion)
during the election. It stops evolving once the election is tallied.

If you put this file in a directory `/path/to/election`, the following
command will perform all possible verifications:

    belenios-tool election verify --dir /path/to/election


Voter's guide
-------------

If you put your secret credential in a file `/path/to/credential` and
your choices in a file `/path/to/choices.json` (as an array of arrays
of 0/1 in JSON format), the following command will output a raw ballot
that can be sent to the administrator of the election:

    belenios-tool election generate-ballot --dir /path/to/election --privcred /path/to/credential --ballot /path/to/choices.json

In the case where the election is administered with the web interface,
a raw ballot prepared with the command-line tool can be uploaded directly
via the web interface.


Administrator's guide
---------------------

The command-line tool allows one to execute all cryptographic
operations needed to run a full election. This is illustrated in the
shell scripts in [../tests/tool](../tests/tool).


Credential authority's guide
----------------------------

### Credential generation

If you have a list of identities in a file `F` with `N` lines, one
identity per line, run:

    belenios-tool setup generate-credentials --uuid XXXXXXXXXXXXXX --file F

where `XXXXXXXXXXXXXX` is the UUID of the
election given by the administrator. It will generate two files:

 * `T.privcreds`: each line of this file contains an identity and a
   private credential. Send each voter the associated credential. Keep
   this file secret, and secure if you want to be able to re-send a
   credential later (e.g. if a voter lost or did not receive it).
 * `T.pubcreds`: this JSON file contains `N` public credentials.
   Send the whole file to the election administrator; it will be the
   `public_creds.json` of the election (and you must check that);

You can optionally add a `--dir` option to specify the directory where
these files will be written.


Trustee's guide
---------------

### Key generation

To generate a keypair, run:

    belenios-tool setup generate-trustee-key

It will generate two files, `XXXXXXXX.public` and `XXXXXXXX.private`,
containing respectively the public and the private key. Send the
public key file to the server administrator, and keep the private key
with extreme care. When the election is open, you must check that
your public key is present in the published `trustees.json`.

### Partial decryption

To compute your decryption share, set `/path/to/election` up as
described in the _Voter's guide_ section above, and run:

    belenios-tool election decrypt --dir /path/to/election --privkey /path/to/privkey > partial_decryption.json

and send `partial_decryption.json` to the election administrator.

Note: be sure to authenticate all your input files when you use your
private key!
