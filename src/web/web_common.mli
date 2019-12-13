(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Signatures
open Serializable_t
open Web_serializable_builtin_t
open Web_serializable_t

module LwtRandom : RANDOM with type 'a t = 'a Lwt.t
(** Lwt-compatible random number generation. *)

type cast_error =
  | ECastSerialization of exn
  | ECastMissingCredential
  | ECastInvalidCredential
  | ECastProofCheck
  | ECastWrongCredential
  | ECastRevoteNotAllowed
  | ECastReusedCredential

type error =
  | ElectionClosed
  | UsedCredential
  | CredentialNotFound
  | UnauthorizedVoter
  | CastError of cast_error

exception BeleniosWebError of error

val fail : error -> 'a Lwt.t

val explain_error : (module Web_i18n_sig.LocalizedStrings) -> error -> string

val format_period : (module Web_i18n_sig.LocalizedStrings) -> period -> string

val open_security_log : string -> unit Lwt.t
(** Set the path to the security logger. *)

val security_log : (unit -> string) -> unit Lwt.t
(** Add an entry to the security log. *)

val fail_http : int -> 'a Lwt.t
val forbidden : unit -> 'a Lwt.t

val rewrite_prefix : string -> string
val set_rewrite_prefix : src:string -> dst:string -> unit

type election_file =
  | ESRaw
  | ESKeys
  | ESTParams
  | ESCreds
  | ESBallots
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

val site_cont :
  string ->
  (site_cont, [ `WithoutSuffix ],
   [ `One of site_cont ] Eliom_parameter.param_name)
  Eliom_parameter.params_type

type privacy_cont =
  | ContAdmin
  | ContSignup of string

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

val generate_token : ?length:int -> unit -> string Lwt.t

val string_of_user : user -> string

val send_email : string -> string -> string -> unit Lwt.t

val split_identity : string -> string * string

val available_languages : string list
val get_languages : string list option -> string list
val string_of_languages : string list option -> string
val languages_of_string : string -> string list

val pcre_exec_opt : rex:Pcre.regexp -> string -> Pcre.substrings option

val is_email : string -> bool
val extract_email : string -> string option

val file_exists : string -> bool Lwt.t
val read_file : ?uuid:uuid -> string -> string list option Lwt.t
val write_file : ?uuid:uuid -> string -> string list -> unit Lwt.t

val cleanup_file : string -> unit Lwt.t
val rmdir : string -> unit Lwt.t

val urlize : string -> string
val unurlize : string -> string

val webize_trustee_public_key : 'a trustee_public_key -> 'a web_trustee_public_key
val unwebize_trustee_public_key : 'a web_trustee_public_key -> 'a trustee_public_key

val get_suitable_group_kind : template -> [ `H | `NH ]
val is_group_fixed : draft_election -> bool

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
