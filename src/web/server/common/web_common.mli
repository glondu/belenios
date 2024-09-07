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

open Belenios_server_core

type 'a updatable = 'a * ('a -> unit Lwt.t)

exception BeleniosWebError of Belenios_ui.Confirmation.error

val fail : Belenios_ui.Confirmation.error -> 'a Lwt.t
val format_period : (module Belenios_ui.I18n.GETTEXT) -> Period.t -> string
val fail_http : Cohttp.Code.status -> 'a Lwt.t
val get_election_home_url : uuid -> string

val uuid :
  string ->
  ( uuid,
    [ `WithoutSuffix ],
    [ `One of uuid ] Eliom_parameter.param_name )
  Eliom_parameter.params_type

type site_cont_path = ContSiteHome | ContSiteElection of uuid
type site_cont_admin = Classic | Basic | New
type site_cont = { path : site_cont_path; admin : site_cont_admin }

val default_admin : site_cont_path -> site_cont
val string_of_site_cont : site_cont -> string

val site_cont :
  string ->
  ( site_cont,
    [ `WithoutSuffix ],
    [ `One of site_cont ] Eliom_parameter.param_name )
  Eliom_parameter.params_type

type privacy_cont = ContAdmin | ContSignup of string

val string_of_privacy_cont : privacy_cont -> string

val privacy_cont :
  string ->
  ( privacy_cont,
    [ `WithoutSuffix ],
    [ `One of privacy_cont ] Eliom_parameter.param_name )
  Eliom_parameter.params_type

type captcha_error = BadCaptcha | BadAddress

type add_account_error =
  | UsernameTaken
  | AddressTaken
  | BadUsername
  | BadPassword of string
  | PasswordMismatch
  | BadSpaceInPassword
  | DatabaseError

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

val send_email :
  mail_kind -> recipient:string -> subject:string -> body:string -> unit Lwt.t

val get_languages : string list option -> string list
val string_of_languages : string list option -> string
val languages_of_string : string -> string list
val urlize : string -> string
val unurlize : string -> string
val get_booth_index : int option -> int option

type credential_record = {
  cr_ballot : string option;
  cr_weight : weight;
  cr_username : string option;
}

val check_password : password_record -> string -> bool
val has_explicit_weights : draft_voter list -> bool
val exhaust_file : Ocsigen_multipart.file_info -> string Lwt.t
