Belenios Web Server
===================


Election administrator's guide
------------------------------

### Setup a new election

Once the server is up and running (see below), anyone who can log into
the server as an administrator can create an election. First, the
administrator has to choose how credentials will be handled:

 * in the automatic mode, the server will generate credentials and
   mail the private parts to each voter. Credential recovery (i.e. one
   voter loses his/her credential or does not receive it) is not
   possible in this case;
 * in the manual mode, a third party will generate credentials using
   a web interface or the command-line tool, and upload the public
   credentials to the server. It's up to this third party to send
   private credentials to each voter, and to implement credential
   recovery.

Moreover, the administrator has to choose the authentication mode for
voters:

 * with the password mode, the server will generate passwords and
   mail them to the voters, this is the most common mode;
 * the other one uses a [CAS](https://www.apereo.org/projects/cas)
   server.

Then, the administrator must:

 * set the name and description of the election;
 * edit questions;
 * edit voters, and have the server send them their password if this
   authentication mode has been chosen;
 * have the credential authority generate credentials and send
   e-mails; in the automatic mode, this is done simply by clicking
   on a button; in the manual mode, a link is generated for the
   credential authority;
 * (optionally) edit trustees. For good security there should be at
   least two trustees; a link is generated for each trustee;
 * finalize the election.

Each "link" above must be sent by the administrator to their intended
recipient. Each link leads to an interface that will help its
recipient accomplish his or her task.

The *election fingerprint*, which is shown on the election page and in
the booth, is the compact Base64 encoding of the SHA256 of
`election.json`. It can be computed from a POSIX shell by piping it
into:

    sha256sum | xxd -r -p | base64

### Election life cycle

An election starts by being in preparation (or "setup mode"), then
becomes finalized. Then, it is immediately opened and can be closed
and re-opened at will. When it is closed, the election administrator
can initiate the tallying process. The encrypted tally is then
computed and published. After each trustee has computed his/her share
of the decryption, the administrator triggers the release of the
result.

At any moment, a finalized election can be archived. This releases
some resources on the server and makes the election read-only. In
particular, it is no longer possible to vote in or to tally an
archived election. Be careful, this operation is not revertible.

### Auditing an election

During the election, the following files are published:

 * `election.json`: election parameters
 * `public_keys.jsons`: trustees' public keys
 * `public_creds.txt`: the public keys associated to valid credentials
 * `ballots.jsons`: accepted ballots

They are accessible from the bottom of the election page. Together,
they enable anyone to audit the election. At the end of the election,
an additional `result.json` file is published with the result and
other cryptographic proofs that everything went well.

Please refer to the auditor's guide in the documentation of the
command-line tool for more information.


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

 * `<cas>`: authenticate with a [CAS](https://www.apereo.org/projects/cas)
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

The `<log>` element indicates a file where some security-sensitive
events will be logged. It is optional.

The `<spool>` element indicates a directory with election data. This
directory should be empty when the server is launched for the first
time, and will be populated with election data. A typical location
would be `/var/spool/belenios`. Warning: it may contain sensitive data
(e.g. the private key when no external trustees are set).
