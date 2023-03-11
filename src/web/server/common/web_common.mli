(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Belenios_core
open Signatures
open Serializable_t
open Web_serializable_t

val ( let&* ) : 'a option -> ('a -> 'b option Lwt.t) -> 'b option Lwt.t
val ( !! ) : string -> string
val ( /// ) : uuid -> string -> string

module Datetime = Web_types.Datetime
module Period = Web_types.Period

module Random : RANDOM

type error =
  | ElectionClosed
  | UnauthorizedVoter
  | CastError of cast_error

exception BeleniosWebError of error

val fail : error -> 'a Lwt.t

val explain_error : (module Belenios_ui.I18n.GETTEXT) -> error -> string

val format_period : (module Belenios_ui.I18n.GETTEXT) -> Period.t -> string

val open_security_log : string -> unit Lwt.t
(** Set the path to the security logger. *)

val security_log : (unit -> string) -> unit Lwt.t
(** Add an entry to the security log. *)

val fail_http : Cohttp.Code.status -> 'a Lwt.t

val rewrite_prefix : string -> string
val set_rewrite_prefix : src:string -> dst:string -> unit

val get_election_home_url : uuid -> string

type election_file =
  | ESArchive of uuid
  | ESRaw
  | ESVoters
  | ESRecords
  | ESETally
  | ESResult

val election_file_of_string : string -> election_file
val string_of_election_file : election_file -> string

val election_file :
  string ->
  (election_file, [ `WithoutSuffix ],
   [ `One of election_file ] Eliom_parameter.param_name)
    Eliom_parameter.params_type

val uuid :
  string ->
  (uuid, [ `WithoutSuffix ],
   [ `One of uuid ] Eliom_parameter.param_name)
    Eliom_parameter.params_type

type site_cont =
  | ContSiteHome
  | ContSiteAdmin
  | ContSiteElection of uuid

val string_of_site_cont : site_cont -> string

val site_cont :
  string ->
  (site_cont, [ `WithoutSuffix ],
   [ `One of site_cont ] Eliom_parameter.param_name)
    Eliom_parameter.params_type

type privacy_cont =
  | ContAdmin
  | ContSignup of string

val string_of_privacy_cont : privacy_cont -> string

val privacy_cont :
  string ->
  (privacy_cont, [ `WithoutSuffix ],
   [ `One of privacy_cont ] Eliom_parameter.param_name)
    Eliom_parameter.params_type

type captcha_error =
  | BadCaptcha
  | BadAddress

type add_account_error =
  | UsernameTaken
  | AddressTaken
  | BadUsername
  | BadPassword of string
  | PasswordMismatch
  | BadSpaceInPassword

val generate_token : ?length:int -> unit -> string
val generate_numeric : ?length:int -> unit -> string

val format_password : string -> string

val string_of_user : user -> string

type mail_kind =
  | MailCredential of uuid
  | MailPassword of uuid
  | MailConfirmation of uuid
  | MailAutomaticWarning of uuid
  | MailAccountCreation
  | MailPasswordChange
  | MailLogin
  | MailSetEmail

val send_email : mail_kind -> recipient:string -> subject:string -> body:string -> unit Lwt.t

val get_languages : string list option -> string list
val string_of_languages : string list option -> string
val languages_of_string : string -> string list

val urlize : string -> string
val unurlize : string -> string

val markup : string -> [> Html_types.span ] Eliom_content.Html.elt

val webize_trustee_public_key : 'a trustee_public_key -> 'a web_trustee_public_key
val unwebize_trustee_public_key : 'a web_trustee_public_key -> 'a trustee_public_key

val get_suitable_group_kind : template -> [ `H | `NH ]
val is_group_fixed : draft_election -> bool

val get_booth_index : int option -> int option

val compute_hash_link :
  service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
           [`WithoutSuffix], 'e, unit, 'f) Eliom_service.t ->
  uuid:uuid -> token:string -> string

type credential_record = {
    cr_ballot : string option;
    cr_weight : weight;
    cr_username : string option;
}

val check_password_with_file :
  db:string -> name_or_email:string -> password:string ->
  (string * string) option Lwt.t

val has_explicit_weights : draft_voter list -> bool

val default_contact : string
val default_questions : question array
val default_name : string
val default_description : string

val default_creation_date : datetime
val default_validation_date : datetime
val default_tally_date : datetime
val default_archive_date : datetime

val days_to_archive : int
val days_to_delete : int
val days_to_mail : int
val days_between_mails : int
val days_to_publish_result : int

val max_election_name_size : int
val max_total_weight : int

val supported_booth_versions : int list
