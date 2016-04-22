Belenios Tool
=============


Introduction
------------

`belenios-tool` is a command-line tool that can be used to perform
administrative tasks related to elections, as well as verifications.
If you do not wish to use the provided web server, a whole election
can be organized using this tool. As an illustration of that, you can
have a look at the `demo/demo.sh` script that simulates an election.

This file documents how to use `belenios-tool`, from the point of view
of the various roles involved in an election. You can also run it with
the `--help` option to get more information.


Auditor's guide
---------------

Note that anyone can be an auditor. Everyone who plays a specific role
in an election should start by auditing the election data.

During an election, you should have access to the following files:

 * `election.json`: election parameters
 * `public_keys.jsons`: trustees' public keys
 * `public_creds.txt`: the public keys associated to valid credentials
 * `ballots.jsons`: accepted ballots

Note that the last one is dynamic, and evolve during the election. At
the end of the election, it is frozen and a `result.json` file will be
published.

If you put these files in a directory `/path/to/election`, the following
command will perform all possible verifications, depending on existing
files:

    belenios-tool verify --dir /path/to/election

For example, during the election, you can check if some candidate
ballot is acceptable by putting it alone in `ballots.jsons`, and
running the command above.


Voter's guide
-------------

If you put your secret credential in a file `/path/to/credential` and
your choices in a file `/path/to/choices.json` (as an array of arrays
of 0/1 in JSON format), the following command will output a raw ballot
that can be sent to the administrator of the election:

    belenios-tool vote --dir /path/to/election --privcred /path/to/credential --ballot /path/to/choices.json

In the case where the election is administered with the web interface,
a raw ballot prepared with the command-line tool can be uploaded directly
via the web interface.


Administrator's guide
---------------------

### Setup a new election

 1. Generate an UUID with the `uuidgen` command. Let it be `$UUID`.
 2. Go to an empty directory. In the following, we denote by `$DIR`
    the full path to this directory and by `$BELENIOS` the full path
    to the Belenios source tree.
 4. Ask the credential authority to generate credentials. Note that
    `$UUID` is needed for that. Save the file with public
    credentials into `$DIR/public_creds.txt`.
 5. Ask each trustee to generate a keypair. Concatenate all trustee
    public keys into a `$DIR/public_keys.jsons` file.
 6. Edit `$BELENIOS/demo/templates/questions.json`.
 7. Go to `$DIR` and run: `belenios-tool mkelection --uuid $UUID
    --group $BELENIOS/demo/groups/default.json --template
    $BELENIOS/demo/templates/questions.json`. It should generate
    `election.json`.
 8. Create an empty `ballots.jsons` file in `$DIR`.

### Running the election

The contents of `$DIR` must be public.

For each received ballot, append it to `ballots.jsons` and run:

    belenios-tool verify --dir $DIR

If no error is reported, publish the new `ballots.jsons`; otherwise,
the new ballot is incorrect and you must revert `ballots.jsons` to its
previous state.

Note that each ballot must be authenticated in order to prevent the
credential authority from stuffing the ballot box. This issue is not
addressed by the command-line tool, but the web server provides
several authentication mechanisms.

### Tallying the election

 1. Go to the election directory, which must contain `election.json`,
    `public_keys.jsons`, `public_creds.txt` and `ballots.jsons`.
 2. Concatenate the `partial_decryption.json` received from each
    trustee into a `partial_decryptions.jsons`, in the same order as in
    `public_keys.jsons`.
 3. Run `belenios-tool finalize`.  It will create
    `result.json`. Publish this file, along with the files listed in
    the first step above. The whole set will enable universal
    verifiability.

Note: `partial_decryptions.jsons` is a temporary file whose contents
is embedded in `result.json`, so it can be discarded.


Credential authority's guide
----------------------------

### Credential generation

If you have a list of identities in a file `F` with `N` lines, one
identity per line, run:

    belenios-tool credgen --uuid XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX --file F

where `XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX` is the UUID of the
election given by the administrator. It will generate three files with
`N` lines:

 * `T.privcreds`: each line of this file contains an identity and a
   private credential. Send each voter the associated credential. Keep
   this file secret, and secure if you want to be able to re-send a
   credential later (e.g. if a voter lost or did not receive it).
 * `T.pubcreds`: each line of this file contains a public credential.
   Send the whole file to the election administrator; it will be the
   `public_creds.txt` for the election (and you must check that);
 * `T.hashcreds`: each line of this file contains, for each id in
   `T.privcreds`, the hash of the corresponding public key. At the
   moment, this file has no practical purpose (but this might change in
   the future). Destroy it.

You can optionally add a `--dir` option to specify the directory where
these files will be written.


Trustee's guide
---------------

### Key generation

To generate a keypair, run:

    belenios-tool trustee-keygen

It will generate two files, `XXXXXXXX.public` and `XXXXXXXX.private`,
containing respectively the public and the private key. Send the
public key file to the server administrator, and keep the private key
with extreme care. When the election is open, you must check that
your public key is present in the published `public_keys.jsons`.

### Partial decryption

To compute your decryption share, set `/path/to/election` up as
described in the _Voter's guide_ section above, and run:

    belenios-tool decrypt --dir /path/to/election --privkey /path/to/privkey > partial_decryption.json

and send `partial_decryption.json` to the election administrator.

Note: be sure to authenticate all your input files when you use your
private key!
