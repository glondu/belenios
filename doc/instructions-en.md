Who does what during a Belenios election ?
=============

Introduction
------------

Belenios proposes a verifiable e-voting system. Each voter can check that
her ballot is indeed in the ballot box, and any third-party can verify
that the claimed result corresponds to the ballots present in the ballot
box, and that these ballots come from legitimate voters. The vote secrecy
is ensured via the splitting of the decryption key between several
authorities (for instance, some members of the committee in charge of the
election) with a threshold mechanism (for instance, 3 among 5 are enough
to decrypt).

But the nice security properties hold only if everyone performs all the
verifications that they are supposed to do. This document lists in
detail, for each role in the election (voter, administrator, etc.) what
must be done at each step.

A key notion in the following instructions is the `url` where a
participant will connect. We will use the notion of `PREFIX` and of
`UUID`, where `PREFIX` gives the `url` of the Belenios server
handling the election, and `UUID` identifies the election we are talking
about among the possibly many elections which are run on this server.
Typically, if the main page of the election is of the form

    https://vote.belenios.org/elections/8GVH85AoSyweXG/

then `PREFIX=https://vote.belenios.org` and `UUID=8GVH85AoSyweXG`.

Instructions for the voter
--------------------------

Before the election starts, the voter receives an email that contains
their private `credential` and the `url` of the election. The election
page displays the opening time when it is not yet open.

During the election, a voter can visit the election page and vote as
follows:

- they enter their private `credential`. This step is done
  automatically if the `url` of the election received by the voter has
  been personalized with the `credential` of the voter (e.g. when the credentials
  are sent by the server).
- they can then see the questions and select their candidates
- their computer encrypts their choices
(with a Javascript program) and prints a `smart ballot tracker` which is
a fingerprint of the ballot. This `smart ballot tracker` is also sent by
email to the voter when they have completed the voting procedure.
- once the voter has reviewed their choices, they have to authenticate
  themself. To do so, they then receive a
  one-time password by email, that should be entered into the voting
  interface. Other authentication means may be used (for example, a
  password may be sent before the start of the election).
- note: a voter may vote again. Only the *last* vote will count.

The Belenios system is *verifiable*.

- a voter may check that their ballot is indeed taken into account, by
looking for their `smart ballot tracker` on the ballot box,
visiting the link `See accepted ballots` on the main page of the
election. They must complain if this is not the case.
For even more security, the voter can perform this check
with another device, possibly from a different location.
If a voter votes several times, only the last smart ballot tracker
appears on the ballot box.

- The voter must also vehemently protest if they receive a
confirmation email that contains a `smart ballot tracker` that is
different from the one that was printed on the screen during the voting
phase. Somebody probably managed to add a ballot in their name. This can
for instance be a hint of an attack by a system administrator who has
access to the mailbox of the voter, in the case where both the
credential and the one-time password are sent to the same address.



A voter can also verify the whole voting process. Instead of checking only
the presence of their ballot in the ballot box, they can verify the validity
of all the ballots, they can monitor the ballot box to check that no ballot
disappears (except in case of revote), and finally check that the claimed
result corresponds to the ballots in the ballot box. For all of this, the
voter can follow the auditor instructions below.

Instructions for the trustees
-----------------------------

During the setup of the election, each trustee is invited to generate
a decryption key by following a link sent by the election
administrator. The trustee must follow the link, and make sure the URL
of the resulting page has one of the following forms (where PREFIX and
UUID are as defined above, and TOKEN is a random-looking character
string):

- `PREFIX/static/trustee.html#generate/UUID/TOKEN`

Moreover, it is expected that the trustee saves:

- their decryption key (or PKI key, in threshold mode) (file
  `private_key.json` or `private_key.txt`). **This key must be saved in a
  secure place** (encrypted container, USB stick stored in a closed place,
  etc) because it protects the secret of the votes (in combination with
  the decryption keys of the other trustees);
- the `url` of the election;
- (in threshold mode) the fingerprint of their PKI public key, simply
  called `public key` below;
- the fingerprint of their verification key associated to their decryption
  key `verification key`.

To ensure that the decryption authorities have generated their key
correctly, the election administrator may, before the start of the
election, ask each authority to verify that they have their decryption
key. In this case, the decryption authority receives a URL of the
form:

    PREFIX/static/trustee.html#check/UUID

and is prompted to enter their decryption key. The authority must
check that the `UUID` of the election displayed is correct and that
their name appears correctly after clicking on `Verify private
key`. The decryption authority must *never* enter their decryption key
outside of this verification step (with the URL indicated above) and
the decryption step (see below).

As soon as the election is ready, it is expected that the trustee checks:

- that their `verification key` is present on the election page, next to their
  name;
- (in threshold mode) that their PKI `public key` is present on the
  election page, next to their name.

After the closing of the election, the trustee participates to the
computation of the tally. In the case of an election with alternative
voting (ordering of the candidates, giving them a grade), the tally
starts by a shuffle phase. For this, it is expected from the trustee that
they:

- check that the URL of the page has the following form:
  `PREFIX/election/shuffle.html#UUID-TOKEN`;
- save the fingerprint of the ballot box after their shuffle: `fingerprint
  of your shuffle`;
- check immediately thereafter that this fingerprint is present on the
  page of the election (to ensure that their shuffle has not be ignored).

In all cases, the tally then proceeds with a phase where the trustee uses
their private key to decrypt the result. It is expected that the trustee:

- check that the URL of the page has the following form:
  `PREFIX/election/trustees.html#UUID-TOKEN`;
- check (only for alternative voting) the fingerprint of their shuffle
  `fingerprint of your shuffle` as saved in the previous step is present
  on the page of the election, next to their name. If this is not the case,
  the private key must not be used;
- save the fingerprint of the ballot box to be decrypted: `fingerprint
  of the encrypted tally`.


Once the tally is finished, the results are published on the page of the
election. It is expected that the trustee:

- **destroys** their decryption key;
- checks that the following data
are present on this page, each time associated to their name:

- (in threshold mode) their PKI `public key`;
- their `verification key`;
- (for alternative voting) the fingerprint of their shuffle;
- the fingerprint of the ballot box to be decrypted `fingerprint of the
  encrypted tally` (in order to check that her private key has not be
  used to decrypt something else).


Instructions for the credential authority
-----------------------------------------

The main role of the credential authority is to generate and transmit
a private credential to each voter.

**Setup.** During the setup of the election, the credential authority
first obtains a private link. They must follow the link, and make sure
the URL of the resulting page has the following form:
`PREFIX/draft/credentials.html#UUID-TOKEN`.

On this page, there is the voter list. The credential authority must
verify with the committee in charge of the election that this list is
correct, as well as the weight of each voter in case of a weighted
vote;

Then the authority has two options to generate the credentials:

- either click on `Generate` on their browser;
- or:
  - copy the voter list to a file, e.g. `voter.txt`;
  - let `$UUID` be the identifier of the election (the last component
    in the given election URL, see at the top of this document);
  - run the command:

        belenios-tool setup generate-credentials --file voters.txt --group Ed25519 --uuid $UUID

    It will generate two files, `$TIMESTAMP.privcreds` and
    `$TIMESTAMP.pubcreds`, and output the `fingerprint of public credentials`;
  - upload the `.pubcreds` file with the `Submit by file` form;
  - keep the `.privcreds` file and save it as `creds.json`.

The second option should be preferred for more security, in particular
if there is no auditor in charge of monitoring the server.

During this step, it is expected that the credential authority saves:

- the list of the private credentials: the `creds.txt` file. **This
  file must be saved in a secure place** (encrypted container, USB
  stick stored in a closed place, etc) because it is a protection
  against ballot stuffing. It will also allow the credential authority
  to send again the credential to a voter who has lost it;
- the `url` of the election;
- the voter list `voters.txt`;
- the fingerprint of the voter list: `fingerprint of voters`;
- the `fingerprint of the public credentials`.

The credential authority is in charge of sending the credentials to the
voters. They must include the `url` of the election in the message that
they send (by email, by postal mail). For sending the credentials by
email, it is possible to use the `contrib/send_credentials.py` script
included in the Belenios sources (see the auditor section below for how
to get the sources). After editing this program according to the appropriate
settings, they can run it:

    contrib/send_credentials.py

The program produces in particular a sample email that can be adapted
to the election by the credential authority. However, the display of
the technical elements (e.g. `url` of the election and `credential` of
the voter in two separate fields) must not be modified without
conducting a security analysis.

**Voting phase.**
As soon as the election is open, and at the end of the election, it is
expected that the credential authority:

- verifies that the number of voters corresponds to the voter list used
  during the setup, as well as the total weight of the election in
  case of a weighted vote, and that the fingerprint of the voter list corresponds
  to the fingerprint saved before, for instance using one of the
  commands suggested [here](#hash);

- verifies that the fingerprint of the list of the public credentials
  corresponds to the one displayed close to their name;

- during the election, the credential authority can, when a voter asks,
  send them again their private credential if they lost it.


**After the tally.** At the end of the election, in order to validate the records, it is
expected that the credential authority :

- verifies that the voting records given by the administrator corresponds
  to the ballots in the ballot box. This verification can be done with
  the command:

        belenios-tool election compute-voters --privcreds /path/to/creds.json --url https://url/to/election

  (where `https://url/to/election` is of the form
  `PREFIX/elections/UUID`, as explained at the top of this document).

  The list output by this command must coincide with the one given by the
  administrator (maybe in a different order).


Once the election is finished and validated, it is expected that the
credential authority:

- destroys the file `creds.json`. Indeed, this file gives the link between
  a voter and their (encrypted) ballot. This link could compromise the vote
  secrecy in the long term, for instance if the encryption keys become
  too small for the computing power in the future (or if a quantum
  computer becomes available...).


Instructions for the committee in charge of the election
--------------------------------------------------------

At the very least, the election committee visits the page of the election
once it is open and checks that:

- the number of voters corresponds to the voter list;

- the value `voter list fingerprint` published corresponds to the one
  that is given (by the system or by the administrator of the election).
  This fingerprint can be computed using one of the
  commands suggested [here](#hash);

- the voter list `voters.txt` corresponds to the legitimate voters,
  with the right weight in case of a weighted vote;

- the list of questions and possible answers correspond to what is
  expected. These questions and answers are also in the `$UUID.bel`
  file that can be obtained by clicking on `public data` in the footer of
  the election page.

Ideally, the election committee also performs the tasks of an auditor or
commissions someone for doing it (a sysadmin of the organization, for
instance).


Instructions for the auditor
-------------------------

Anyone who knows the `url` of the election can be an auditor.
The `url` of an election is of the form
`PREFIX/elections/UUID/`, where, for instance,
`PREFIX=https://vote.belenios.org` and `UUID=8GVH85AoSyweXG`.

An auditor will ensure in particular that:

- the election data (public keys, public credentials, etc) are
consistent and do not change over time;

- the ballot box, that contains the encrypted votes, evolves
  consistently: no ballot is removed unless it is replaced by a ballot
  with the same credential (this corresponds to a revote);

- the ballot box only contains well-formed ballots (with valid
  zero-knowledge proofs, and valid credentials);

- the integrity of active files (HTML, Javascript, etc.) used by the
  voters, the trustees and the credential authority is preserved;

- the result of the election corresponds to the encrypted ballots,
  thanks to the zero-knowledge proofs of correct decryption produced
  by the decryption trustees.

The security of Belenios relies on the fact that the verifications described
below are performed by at least one honest person.

Note: these verifications are also run automatically by our servers for
the elections that are setup with a maximum security level (external
credential authority and at least two external trustees).

**Setup.**
In order to do these
tests, the auditor must use some software. We describe here how to run the
verifications using `belenios-tool`, the sources of which are available
from [Gitlab Inria](https://gitlab.inria.fr/belenios/belenios) and
that can be installed under Linux Debian/Ubuntu using
`sudo apt install belenios-tool`.
Then the auditor must create a directory `workdir`
where the election audit information will be stored as they are
downloaded, in the form of a `git` repository.

In order to check that the HTML/Javascript codes used by voters,
decryption trustees, and credential authorities are
not modified by a corrupted server, the auditor needs to find out the
"right" code for each of these programs. They will then make sure the server
delivers these files uncorrupted. First, a template must be
prepared: we just copy it from the Belenios sources:

    cp path/to/sources/belenios/contrib/reference_template.json workdir/hashref

Then, there are several solutions to ensure that the files served by the
server are valid, while monitoring the election identified by UUID:

- either the auditor simply trusts the files downloaded the first time
  and checks that they do not vary over time
  ([TOFU](https://en.wikipedia.org/wiki/Trust_on_first_use)
  principle).  Then the monitoring command is as follows:

        ./monitor_elections.py --url PREFIX --wdir workdir --checkhash yes --hashref workdir/hashref --outputref workdir/hashref --uuid UUID

  Each time the files change (including at the first run), this will
  print a warning message.

- or the auditor fetches the sources, recompile the code, start a local
  server, uses the previous command to fill-in the `workdir/hashref`
  file with trusted values, and copy it to use it as a reference for
  monitoring the real election that is run on an external server. The
  command is then the same as above.

- or the auditor trusts a well-identified person who published a
  gpg-signed version of the reference file. In that case additional
  arguments must be passed to the monitoring tool: the url of this signed
  version and a gpg keyring containing the public key of the person, as a
  trusted one. In the case of our voting platform, such a file is
  provided by the main developer of Belenios, Stéphane Glondu. We give
  the corresponding command-line, to be adapted for another server or
  another trusted person:

        ./monitor_elections.py --url https://vote.belenios.org/ --wdir workdir --checkhash yes --hashref workdir/hashref --outputref workdir/hashref --sighashref https://vote.belenios.org/monitoring-reference/reference.json.gpg --keyring workdir/trustdb.gpg --uuid UUID

In all cases, the auditor will regularly execute a monitoring command
that we call `monitor_elections`.
It is possible to redirect the messages with the `--logfile` option. Then
only abnormal behaviours are reported on `stdout/stderr`, which makes it
possible to run the command from a `crontab` and to be warned in case of
problem.

**Voting phase.**
During the election, it is expected that the auditor:

 - in case the auditor has access to the voter list `voters.txt`
   (requested to the committee in charge of the election), verifies that
   the number of voters displayed on the main page of the election
   corresponds to the voter list, as well as the total weight of the
   election in case of a weighted vote, and that the fingerprint of
   the voter list corresponds to the fingerprint saved before, for
   instance using one of the commands suggested [here](#hash);

 - if the auditor has not access to the voter list, verifies that the
   number of voters and total weight of the election displayed on the
   main page of the election correspond to the official data;

 - frequently runs `monitor_elections`. Ideally, this should be done
   at non guessable points of time, from various IP adresses that
   reflect the diversity of voters and trustees. The goal is that a
   corrupted server cannot guess when the request comes from an
   auditor or a genuine voter / trustee. Here are a few guidelines for
   a motivated auditor in order to look like a genuine user:
   * as already said, the requests to the server should be done
     frequently but not at a regular, predictable rate;
   * not only various IP adresses should be used but also with various
     device configuration information (browser type and version,
     operating system, active plugins, time zone, language, screen
     resolution, etc) from a large pool of typical device
     configurations used in reality by humans;
   * IP adresses should reflect the various locations and service
     providers of the population of voters;
   * the order in which the files are requested to the server should
     follow the order of typical visits of voters and trustees, with
     plausible (non predictible) delays between each request of files.

   Note that the script provided in `belenios-tool` does not offer support for this.


**After the tally.** After the election, it is expected that the auditor:

- runs again  `monitor_elections`. The election page now contains a
  `result.json` file and this command will check the cryptographic
  proofs associated to the result of the election;
- verifies that the result mentioned in the `result.json` file corresponds
  to the result published on the page of the election. This
  verification must be done manually.

Note: If the `belenios-tool` command-line tool is used, the trust in
the audit verifications partly relies on the trust in this tool. It is
possible to write independent verification software following Belenios
specification available
[here](https://www.belenios.org/specification.pdf).

Instructions for the administrator of the election
--------------------------------------------------

It might look strange but the administrator of the election has not so
many verifications to perform. This comes from the fact that the Belenios
voting system is designed so that we don't have to trust the
administrator. Security relies on the combined verifications of the
different participants: trustees, credential authority, election
committee, and auditors.

The important points for the administrator are the following:

- to obtain the voter list, as a list of valid email addresses, one
  address by voter. In case of a weighted vote, voters may be assigned
  a different weight. This list must be validated by the electoral
  committee;
- to check and check again these email addresses before starting the
  election (and even before the sending of the credentials in automatic
  mode). Once the setup of the election is finished, it is not possible
  to modify them and there is no alert in case a message could not be
  sent;
- to verify that all the participants use the same `url` for the
  election;
- if the administrator did not commission someone as a credential authority,
  they must download the list of private credentials (`Download private
  credentials`) in order to be able to send again the credential to a
  voter who has lost it. For better security, though, it is preferred to
  commission a third party to play the role of credential authority.
- if external election authorities are used (highly recommended for
  vote privacy), make sure that they have correctly stored their
  private key, otherwise it will not be possible to tally the
  election. For this, click on `Check private key ownership`, send
  the obtained link to the authorities, and make sure the test was
  sucessful, with the display of the name of the authority.

For getting the best security level, the administrator must have:

- a person (the credential authority) in charge of generating the
  credentials and sending them to the voters (the server can do this
  itself, but this opens the possibility of a ballot stuffing attack);
- several trustees in charge of protecting vote secrecy: in order to decrypt
  the individual ballots it is then required to attack all of them (or at
  least a proportion of them, in threshold mode).

<a name="hash"></a>How to compute the fingerprint of a file?
-------------------------------------------------------

To compute the fingerprint of a file, you need to use the same hash
algorithm as the one used in Belenios. We provide here possible
solutions with command lines. We use the file `voters.txt` as example
but of course you may replace it by any other file.

    sha256sum voters.txt | xxd -p -r | base64 | tr -d "="

(or `shasum -a256` instead of `sha256sum` for example on MacOS)

    cat voters.txt | python3 -c "import hashlib,base64,sys;m=hashlib.sha256();m.update(sys.stdin.read().encode());print(base64.b64encode(m.digest()).decode().strip('='))"

You may also use
[the online tool](https://vote.belenios.org/tools/compute-fingerprint)
supported by Belenios.
