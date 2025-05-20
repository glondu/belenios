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

open Lwt.Syntax
open Belenios
open Belenios_storage_api
open Belenios_server_core

type 'a updatable = 'a * ('a -> unit Lwt.t)
type ('a, 'r) with_lwt_cont = ('a -> 'r Lwt.t) -> 'r Lwt.t

exception BeleniosWebError of Belenios_web_api.cast_error

let fail e = Lwt.fail (BeleniosWebError e)

let fail_http status =
  Lwt.fail
    (Ocsigen_extensions.Ocsigen_http_error (Ocsigen_cookie_map.empty, status))

let get_election_home_url ?credential uuid =
  let suffix = match credential with None -> "" | Some x -> "/" ^ x in
  Printf.sprintf "%s/election#%s%s" !Web_config.prefix (Uuid.unwrap uuid) suffix

let uuid x =
  Eliom_parameter.user_type ~of_string:Uuid.wrap ~to_string:Uuid.unwrap x

type site_cont_path = ContSiteHome | ContSiteElection of uuid
type site_cont_admin = Default | Basic
type site_cont = { path : site_cont_path; admin : site_cont_admin }

let default_admin path = { path; admin = Default }

let site_cont_of_string x =
  let fail () = invalid_arg "site_cont_of_string" in
  let path, admin =
    match String.split_on_char '@' x with
    | [ path; "basic" ] -> (path, Basic)
    | [ path ] -> (path, Default)
    | _ -> fail ()
  in
  let path =
    match String.split_on_char '/' path with
    | [ "home" ] -> ContSiteHome
    | [ "elections"; uuid ] -> ContSiteElection (Uuid.wrap uuid)
    | _ -> fail ()
  in
  { path; admin }

let string_of_site_cont x =
  let path =
    match x.path with
    | ContSiteHome -> "home"
    | ContSiteElection uuid -> Printf.sprintf "elections/%s" (Uuid.unwrap uuid)
  in
  let admin = match x.admin with Default -> "" | Basic -> "@basic" in
  path ^ admin

let site_cont x =
  Eliom_parameter.user_type ~of_string:site_cont_of_string
    ~to_string:string_of_site_cont x

type privacy_cont = ContAdmin | ContSignup of string

let privacy_cont_of_string x =
  match String.split_on_char '/' x with
  | [ "admin" ] -> ContAdmin
  | [ "signup"; service ] -> ContSignup service
  | _ -> invalid_arg "privacy_cont_of_string"

let string_of_privacy_cont = function
  | ContAdmin -> "admin"
  | ContSignup service -> "signup/" ^ service

let privacy_cont x =
  Eliom_parameter.user_type ~of_string:privacy_cont_of_string
    ~to_string:string_of_privacy_cont x

type captcha_error = BadCaptcha | BadAddress

type add_account_error =
  | UsernameTaken
  | AddressTaken
  | BadUsername
  | BadPassword of string
  | PasswordMismatch
  | BadSpaceInPassword
  | DatabaseError

let format_password x =
  if String.length x = 15 then
    String.sub x 0 5 ^ "-" ^ String.sub x 5 5 ^ "-" ^ String.sub x 10 5
  else x

let string_of_user { user_domain; user_name } = user_domain ^ ":" ^ user_name
let get_languages xs = match xs with None -> [ "en" ] | Some xs -> xs
let string_of_languages xs = String.concat " " (get_languages xs)
let languages_of_string x = Re.Pcre.(split ~rex:(regexp "\\s+") x)
let urlize = String.map (function '+' -> '-' | '/' -> '_' | c -> c)
let unurlize = String.map (function '-' -> '+' | '_' -> '/' | c -> c)
let get_booth_index = function Some 2 -> Some 0 | _ -> None

type credential_record = {
  cr_ballot : string option;
  cr_weight : weight;
  cr_username : string option;
}

let check_password { salt; hashed; _ } password =
  sha256_hex (salt ^ String.trim password) = hashed

let has_explicit_weights voters =
  List.exists
    (fun v ->
      let (_, { weight; _ }) : Voter.t = v.sv_id in
      weight <> None)
    voters

let exhaust_file file =
  let fname = file.Ocsigen_extensions.tmp_filename in
  let* result = Lwt_stream.to_string (Lwt_io.chars_of_file fname) in
  let* () = Lwt_unix.unlink fname in
  Lwt.return result
