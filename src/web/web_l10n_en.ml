(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

let lang = "en"
let accepted_ballots = "Accepted ballots"
let ballots_have_been_accepted_so_far = " ballot(s) have been accepted so far."
let ballots_have_been_accepted = " ballot(s) have been accepted."
let ballots_have_been_accepted_and = " ballot(s) have been accepted, and "
let have_been_tallied = " have been tallied."
let username = "Username:"
let password = "Password:"
let login = "Login"
let password_login = "Password login"
let not_found = "Not found"
let election_does_not_exist = "This election does not exist. This may happen for elections that have not yet been open or have been deleted."
let cookies_are_blocked = "Cookies are blocked"
let please_enable_them = "Your browser seems to block cookies. Please enable them."


let mail_password_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Your password for election %s"

let mail_password : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
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
counts.%s"


let mail_credential_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Your credential for election %s"

let mail_credential : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "You are listed as a voter for the election

  %s

%s

Credential: %s
Page of the election: %s

Note that you are allowed to vote several times.  Only the last vote
counts.%s"

let mail_credential_password =
"You will find below your credential.  To cast a vote, you will also
need a password, sent in a separate email.  Be careful, passwords and
credentials look similar but play different roles.  You will be asked
to enter your credential before entering the voting booth.  Login and
passwords are required once your ballot is ready to be cast."

let mail_credential_cas =
"You will find below your credential."

let mail_confirmation_subject : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Your vote for election %s"

let mail_confirmation : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Dear %s,

Your vote for election

  %s

has been recorded. Your smart ballot tracker is

  %s%s


You can check its presence in the ballot box, accessible at
  %s

Results will be published on the election page
  %s%s

-- \nBelenios"

let this_vote_replaces = "\n\nThis vote replaces any previous vote."
let please_contact = "To get more information, please contact:"
