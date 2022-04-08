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
detail, for each role in the election (voter, administrator, etc) what
must be done at each step.

Instructions for the voter
--------------------------

When a voter wants to vote online, his computer encrypts his choices
(with a Javascript program) and prints a `smart ballot tracker` which is
a fingerprint of the ballot. This `smart ballot tracker` is also sent by
email to the voter when he has completed the voting procedure.

In order to check that his ballot is indeed taken into account, the voter
must check that his `smart ballot tracker` is present in the ballot box
by visiting the link `See accepted ballots` on the main page of the
election. The voter must also vehemently protest if he receives a
confirmation email that contains a `smart ballot tracker` that is
different from the one that was printed on the screen during the voting
phase. Somebody probably managed to add a ballot in his name. This can
for instance be a hint of an attack by a system administrator who has
access to the mailbox of the voter, in the case where both the login /
password and the credential are sent to the same address.

Note: a voter can vote several times. Only the *last* vote is taken into
account. The voter must check that the last `smart ballot tracker` is in
the ballot box. The previous smart ballot trackers are removed from the
box.

A voter can also verify the whole voting process. Instead of checking only
the presence of his ballot in the ballot box, he can verify the validity
of all the ballots, he can monitor the ballot box to check that no ballot
disappears (except in case of revote), and finally check that the claim
result corresponds to the ballots in the ballot box. For all of this, the
voter can follow the auditor instructions below.

Instructions for the trustees
-----------------------------

During the setup of the election, it is expected that the trustee saves:

- her decryption key (or PKI key, in threshold mode) (file `
  private_key.json` or `private_key.txt`). **This key must be saved in a
  secure place** (encrypted container, USB stick stored in a closed place,
  etc) because it protects the secret of the votes (in combination with
  the decryption keys of the other trustees);
- the `url` of the election;
- (in threshold mode) the fingerprint of her PKI public key, simply
  called `public key` below;
- the fingerprint of her verification key associated to her decryption
  key `verification key`.

As soon as the election is ready, it is expected that the trustee checks:

- that her `verification key` is present on the election page, aside her
  name;
- (in threshold mode) that her PKI `public key` is present on the
  election page, aside her name.

After the closing of the election, the trustee participates to the
computation of the tally. In the case of an election with alternative
voting (ordering of the candidates, giving them a grade), the tally
starts by a shuffle phase. For this it is expected from the trustee that
she

- saves the fingerprint of the ballot box after her shuffle: `fingerprint
  of your shuffle`;
- checks immediately thereafter that this fingerprint is present on the
  page of the election (to ensure that her shuffle has not be ignored).

In all cases, the tally then proceeds with a phase where the trustee uses
her private key to decrypt the result. It is expected that the trustee:

- checks (only for alternative voting) the fingerprint of her shuffle
  `fingerprint of your shuffle` as saved in the previous step is present
  on the page of the election, aside her name. If this is not the case,
  the private key must not be used.
- saves the fingerprint of the ballot box to be decrypted: `fingerprint
  of the encrypted tally`.


Once the tally is finished, the results are published on the page of the
election. It is expected that the trustee checks that the following data
are present on this page, each time associated to her name:

- (in threshold mode) her PKI `public key`;
- her `verification key`;
- (for alternative voting) the fingerprint of her shuffle;
- the fingerprint of the ballot box to be decrypted `fingerprint of the
  encrypted tally` (in order to check that her private key has not be
  used to decrypt something else).


Instructions for the credential authority
-----------------------------------------

The main role of the credential authority is to generate and transmit
a private credential to each voter.

**Setup.** During the setup of the election, the credential authority
first obtains a private url that allows her to retrieve the voter list
`voters.txt`.  The credential authority must verify with the committee
in charge of the election that this list is correct, as well as the
weight of each voter in case of a weighted vote;

Then the authority has two options to generate the credentials:
- either click on `Generate` on her browser;
- or:
  - copy the voter list to a file, e.g. `voter.txt`;
  - let `$UUID` be the identifier of the election (the last component
    in the given election URL);
  - let `$GROUP` be either `BELENIOS-2048` (if there are no
    alternative questions) or `RFC-3526-2048` (if there is at least
    one alternative question);
  - run the command:

    belenios-tool credgen --file voters.txt --group <(echo $GROUP) --uuid $UUID`

    It will generate two files, `$TIMESTAMP.privcreds` and
    `$TIMESTAMP.pubcreds`;
  - upload the `.pubcreds` file with the `Submit by file` form;
  - [compute the fingerprint](#hash) of the `.pubcreds` file and save
    it as `fingerprint of the public credentials`;
  - keep the `.privcreds` file and save it as `creds.txt`.

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
voters. She must include the `url` of the election in the message that
she sends (by email, by postal mail). For sending the credentials by
email, it is possible to use the `contrib/send_credentials.py ` program
included in the Belenios sources (see the auditor section below for how
to get the sources). After editing this program according to the appropriate
settings, she can run it :

        contrib/send_credentials.py


**Voting phase.**
As soon as the election is open, and at the end of the election, it is
expected that the credential authority:

- verifies that the number of voters corresponds to the voter list used
  during the setup, as well as the total weight of the election in
  case of a weighted vote, and that the fingerprint of the voter list corresponds
  to the fingerprint saved before, for instance using one of the
  commands suggested [here](#hash).

- verifies that the fingerprint of the list of the public credentials
  corresponds to the one displayed close to his name.

- during the election, the credential authority can, when a voter asks,
  send him again his private credential if he lost it.


**After the tally.** At the end of the election, in order to validate the records, it is
expected that the credential authority :

- verifies that the voting records given by the administrator corresponds
  to the ballots in the ballot box. This verification can be done with
  the command:

        belenios-tool compute-voters --privcreds /path/to/creds.txt --url https://url/to/election

  The list output by this command must coincide with the one given by the
  administrator (maybe in a different order).


Once the election is finished and validated, it is expected that the
credential authority:

- destroys the file `creds.txt`. Indeed, this file gives the link between
  a voter and his (encrypted) ballot. This link could compromise the vote
  secrecy in the long term, for instance if the encryption keys become
  too small for the computing power in the future (or if a quantum
  computer becomes available...)


Instructions for the committee in charge of the election
--------------------------------------------------------

At the very least, the election committee visits the page of the election
once it is open and checks that:

- the number of voters corresponds to the voter list;

- the value `voter list fingerprint` published corresponds to the one
  that is given (by the system or by the administrator of the election).
  This fingerprint can be computed using one of the
  commands suggested [here](#hash).

- the voter list `voters.txt` corresponds to the legitimate voters,
  with the right weight in case of a weighted vote.

- the list of questions and possible answers correspond to what is
  expected. These questions and answers are also in the `election.json`
  file that can be obtained by clicking on `parameters` in the footer of
  the election page.

Ideally, the election committee also performs the tasks of an auditor or
commissions someone for doing it (a sysadmin of the organization, for
instance).


Instructions for the auditor
----------------------------

Anyone who knows the `url` of the election can be an auditor. The
security of Belenios relies on the fact that the verifications described
below are performed by at least one honest person. In order to do these
tests, the auditor must use some software. We describe here how to run the
verifications using `belenios-tool` the sources of which are available
from [Gitlab Inria](https://gitlab.inria.fr/belenios/belenios) and that
can be installed under Linux Debian/Ubuntu using `sudo apt install
belenios-tool`.

Note: these verifications are also run automatically by our servers for
the elections that are setup with a maximum security level (external
credential authority and at least two external trustees).

During and after the election, the auditor has access to the following
files (from the page of the election):

 * `election.json`: parameters of the election;
 * `trustees.json`: verification keys of the trustees, and their PKI
 public keys in threshold mode;
 * `public_creds.txt`: public parts of the credentials;
 * `ballots.jsons`: ballots that have been accepted for inclusion in the
   ballot box.

During the election, it is expected that the auditor:

- verifies that the printed number of voters is correct and that the
  fingerprint of the voter list is correct (if she has access to this
  information). See the instructions for the election committee.

- verifies that the file of public parts of the credentials
  `public_creds.txt` corresponds to the fingerprint of the public
  credentials displayed on the election main page, for example using
  one of the commands suggested [here](#hash).

- verifies that the number of voters is equal to the number of  public
  credentials in `public_creds.txt`.

- verifies the consistency of the ballot box. By copying the 4 files
  listed above in a directory `/path/to/election`, the following command
  runs all the required verifications:

        belenios-tool verify --dir /path/to/election

- verifies that the ballot box evolves in a way that is in accordance
  with the protocol : no ballot must disappear unless it is replaced by
  another ballot coming from the same credential (case of voter who re-votes).
  For this, download again the 4 files in a new directory
  `/path/to/election/new` and run the command:

        belenios-tool verify-diff --dir1 /path/to/election --dir2 /path/to/election/new

- verifies that the page given to the voters and the associated resources
  (images, css, Javascript files) do not change. The Javascript programs
  must correspond to the one obtained after compiling Belenios sources.
  The program `contrib/check_hash.py` given in the sources does this
  automatically:

        contrib/check_hash.py --url https://url/to/server

  Note that the url is the one of the server and not the one of the
  election; for instance `--url https://belenios.loria.fr`.

After the election, the auditor has also access to the `result.json`
file. It is expected that the auditor:

- run again both verifications mentioned above to verify the consistency
  of the final ballot box and the consistency with the last saved ballot
  box.
- verify that the result mentioned in the `result.json` file corresponds
  to the result published on the page of the election. In fact, this
  verification must be done manually.
- verify that the fingerprints that are present in these files correspond
  to what is published on the election page as read by the voters and the
  other participants.

For this last point and all the other tasks (except for the manual
verification of the claimed result), a software tool is distributed with
the Belenios sources. It requires that `belenios-tool` is compiled and
installed properly. Then the auditor must create a directory `workdir`
where the election audit information will be stored as long as they are
downloaded, in the form of a `git` repository. During the audit phase,
the following command must be frequently run:

        contrib/monitor_elections.py --uuid <uuid_of_the_election> --url https://url/to/server --wdir <workdir>

and it will download the current audit data, verify them, and compare
them with the previous one.

It is possible to redirect the messages with the `--logfile` option. Then
only abnormal behaviours are reported on `stdout/stderr`, which makes it
possible to run the command from a `crontab` and to be warned in case of
problem.


Note: If the `belenios-tool` command-line tool is used, the trust in
the audit verifications partly relies on the trust in this tool. It is
possible to write independent verification software following Belenios
specification available [here](https://www.belenios.org/specification.pdf).


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
  committee.
- to check and check again these email addresses before starting the
  election (and even before the sending of the credentials in automatic
  mode). Once the setup of the election is finished, it is not possible
  to modify them and there is no alert in case a message could not be
  sent.
- to verify that all the participants use the same `url` for the
  election.
- If the administrator did not commission someone as a credential authority,
  she must download the list of private credentials (`Download private
  credentials`) in order to be able to send again the credential to a
  voter who has lost it. For better security, though, it is preferred to
  commission a third party to play the role of credential authority.

For getting the best security level, the administrator must have:

- a person (the credential authority) in charge of generating the
  credentials and sending them to the voters (the server can do this
  itself, but this opens the possibility of a ballot stuffing attack).
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
[the online tool](https://belenios.loria.fr/compute-fingerprint)
supported by Belenios.
