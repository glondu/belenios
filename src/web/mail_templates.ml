(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Affero General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version, with the additional   *)
(*  exemption that compiling, linking, and/or using OpenSSL is allowed.   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with this program.  If not, see                         *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)


let password name username password url = Printf.sprintf
  "You are listed as a voter for the election

  %s

You will find below your login and password.  To cast a vote, you will
also need a credential, sent in a separate email.  Be careful,
passwords and credentials look similar but play different roles.  You
will be asked to enter your credential before entering the voting
booth.  Login and passwords are required once your ballot is ready to
be cast.

Username: %s
Password: %s
Page of the election: %s

Note that you are allowed to vote several times.  Only the last vote
counts.

-- \nBelenios" name username password url


let credential name username cred url = Printf.sprintf
  "You are listed as a voter for the election

  %s

You will find below your login and credential.  To cast a vote, you will
also need a password, sent in a separate email.  Be careful,
passwords and credentials look similar but play different roles.  You
will be asked to enter your credential before entering the voting
booth.  Login and passwords are required once your ballot is ready to
be cast.

Username: %s
Credential: %s
Page of the election: %s

Note that you are allowed to vote several times.  Only the last vote
counts.

-- \nBelenios" name username cred url


let confirmation id name hash ballots_url url = Printf.sprintf
  "Dear %s,

Your vote for election

  %s

has been recorded. Your smart ballot tracker is

  %s

You can check its presence in the ballot box, accessible at

  %s

Results will be published on the election page

  %s

-- \nBelenios" id name hash ballots_url url
