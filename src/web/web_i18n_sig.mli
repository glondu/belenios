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
  val booth_step5 : string
  val booth_step6 : string
  val i_am : string
  val and_ : string
  val i_cast_my_vote : string
  val please_login_to_confirm : string
  val your_ballot_for : string
  val has_been_received : string
  val nobody_can_see : string
  val you_have_already_voted : string
  val go_back_to_election : string
  val has_been_accepted : string
  val you_can_check_its_presence : string
  val ballot_box : string
  val anytime_during_the_election : string
  val confirmation_email : string
  val thank_you_for_voting : string
  val is_rejected_because : string
  val fail : string
  val accepted_ballots : string
  val ballots_have_been_accepted_so_far : string
  val ballots_have_been_accepted : string
  val ballots_have_been_accepted_and : string
  val have_been_tallied : string
  val username : string
  val password : string
  val login : string
  val password_login : string
  val not_found : string
  val election_does_not_exist : string
  val cookies_are_blocked : string
  val please_enable_them : string
  val the_raw_results : string
  val json_result : string
  val it_contains_all_clear : string
  val years : string
  val months : string
  val days : string
  val hours : string
  val minutes : string
  val seconds : string
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
  val error_Serialization : (string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
  val error_ProofCheck : string
  val error_ElectionClosed : string
  val error_MissingCredential : string
  val error_InvalidCredential : string
  val error_RevoteNotAllowed : string
  val error_ReusedCredential : string
  val error_WrongCredential : string
  val error_UnauthorizedVoter : string
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
