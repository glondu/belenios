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
  val start : string
  val advanced_mode : string
  val see_accepted_ballots : string
  val belenios_booth : string
  val here : string
  val question_header : string
  val at_least : string
  val at_most : string
  val previous : string
  val next : string
  val nothing : string
  val enter_cred : string
  val invalid_cred : string
  val input_credential : string
  val answer_to_questions : string
  val warning_0_255 : string
  val alert_0_255 : string
  val at_least_one_invalid : string
  val review_and_encrypt : string
  val authenticate : string
  val confirm : string
  val done_ : string
  val booth_step1 : string
  val booth_step2 : string
  val booth_step3 : string
  val booth_step5 : string
  val booth_step6 : string
  val input_your_credential : string
  val wait_while_encrypted : string
  val encrypting : string
  val restart : string
  val successfully_encrypted : string
  val not_cast_yet : string
  val qmark : string
  val your_tracker_is : string
  val we_invite_you_to_save_it : string
  val continue : string
  val election_uuid : string
  val election_fingerprint : string
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
  val administer_elections : string
  val administer_this_election : string
  val powered_by : string
  val get_the_source_code : string
  val audit_data : string
  val parameters : string
  val public_credentials : string
  val ballots : string
  val election_server : string
  val accepted_ballots : string
  val ballots_have_been_accepted_so_far : string
  val ballots_have_been_accepted : string
  val ballots_have_been_accepted_and : string
  val have_been_tallied : string
  val username : string
  val password : string
  val login : string
  val password_login : string
  val by_using_you_accept : string
  val privacy_policy : string
  val privacy_policy_short : string
  val accept : string
  val not_yet_open : string
  val come_back_later : string
  val cookies_are_blocked : string
  val please_enable_them : string
  val election_currently_closed : string
  val election_closed_being_tallied : string
  val election_has_been_tallied : string
  val election_archived : string
  val number_accepted_ballots : string
  val you_can_also_download : string
  val result_with_crypto_proofs : string
  val the_raw_results : string
  val json_result : string
  val with_the_jsquery : string
  val it_contains_all_clear : string
  val blank_vote : string
  val no_other_blank : string
  val it_will_open_in : string
  val the_election_will_close_in : string
  val years : string
  val months : string
  val days : string
  val hours : string
  val minutes : string
  val seconds : string
  val result_currently_not_public : (string -> 'f, 'b, 'c, 'e, 'e, 'f) format6
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
