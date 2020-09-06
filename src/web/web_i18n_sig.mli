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

module type LocalizedStrings = sig
  val lang : string
  val username : string
  val password : string
  val login : string
  val password_login : string
  val not_found : string
  val election_does_not_exist : string
  val cookies_are_blocked : string
  val please_enable_them : string
  val mail_password_subject : (string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
  val mail_password : (string -> string -> string -> string -> string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
  val mail_credential_subject : (string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
  val mail_credential : (string -> string -> string -> string -> string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
  val mail_credential_password : string
  val mail_credential_cas : string
  val mail_confirmation_subject : (string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
  val mail_confirmation : (string -> string -> string -> string -> string -> string -> string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
  val this_vote_replaces : string
  val please_contact : string
end

module type GETTEXT = sig
  val lang : string
  val s_ : string -> string
  val f_ : ('a, 'b, 'c, 'c, 'c, 'd) format6 -> ('a, 'b, 'c, 'c, 'c, 'd) format6
  val sn_ : string -> string -> int -> string
  val fn_ :
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    ('a, 'b, 'c, 'c, 'c, 'd) format6 ->
    int ->
    ('a, 'b, 'c, 'c, 'c, 'd) format6
end
