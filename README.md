Belenios
========


Introduction
------------

Belenios is a verifiable voting system that partly implements the
Helios-C protocol described [here](http://eprint.iacr.org/2013/177),
which is itself derived from [Helios](http://vote.heliosvoting.org).

It consists of a command-line tool and a web server. Both use the same
backend and can be used to organize elections and perform
verifications. They employ messages formatted in a common format, a
specification of which is available in doc/specification.tex.

Compilation instructions are provided in INSTALL.md.


Election overview
-----------------

An election involves several roles: an administrator, a credential
authority, trustees and voters. For maximum security, each of these
roles must be performed by a different entity. An election can be
summarized as follows:

 1. The administrator initiates the process.
 2. The credential authority generates one credential per voter; he
    sends the private part to each voter and all public parts to
    the administrator.
 3. Each trustee generates a keypair and sends his/her public key to
    the administrator.
 4. The administrator collects all public credentials and trustees'
    public keys and sets up the election.
 5. The administrator opens the election.
 6. Each voter votes; the administrator collects, checks and publishes
    all the ballots.
 7. The administrator closes the election.
 8. Trustees collectively decrypt the result.
 9. The administrator announces the result of the election.


The command-line tool
---------------------

Each step can be performed with the help of the command-line tool. The
tool is also the most convenient way to exercise the verifiability
capabilities of the system.

More information in doc/tool.md.


The web server
--------------

The whole process can be executed using the web server. Each step can
be done with a browser. In this case, the formal "administrator" role
above is typically shared between the server and a human operator. The
server can also assume the roles of credential authority and
trustee. Therefore, in its simplest (and weakest) form, an election
involves only an operator henceforth called "election administrator"
(usually distinct from the person who sets up and administrates the
server itself) and voters. In its strongest form, an election involves
the election administrator, a credential authority, (at least) two
trustees and voters.

More information in doc/web.md.


Legal
-----

### Internal code

By "internal code", we mean everything that is not in the `ext/`
directory.

Copyright © 2012-2016 Inria

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

Please refer to each file for accurate copyright and licensing
information.
