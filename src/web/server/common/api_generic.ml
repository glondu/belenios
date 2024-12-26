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
open Belenios_web_api
open Belenios_storage_api
open Belenios_server_core
open Web_common

let ( let& ) = Option.bind

type token = { expiration : float; account : account }

let tokens = ref SMap.empty

let new_token account =
  let token = generate_token ~length:22 () in
  let expiration = Unix.gettimeofday () +. 86400. in
  tokens := SMap.add token { expiration; account } !tokens;
  Lwt.return token

let filter tokens =
  let now = Unix.gettimeofday () in
  SMap.filter (fun _ { expiration; _ } -> now < expiration) tokens

let lookup_token token =
  tokens := filter !tokens;
  let& { account; _ } = SMap.find_opt token !tokens in
  Some account

let invalidate_token token = tokens := SMap.remove token !tokens

let () =
  let@ a = Accounts.add_update_hook in
  let f { expiration; account } =
    let account = if a.id = account.id then a else account in
    { expiration; account }
  in
  tokens := SMap.map f !tokens;
  Lwt.return_unit

exception Error of Belenios_web_api.error

type result = [ `Json of int * string | `Bel of string ]
type body = { run : 'a. (string -> 'a) -> ('a -> result Lwt.t) -> result Lwt.t }

let return_json code x = Lwt.return @@ `Json (code, x)
let ok = return_json 200 "{}"
let bad_request = return_json 400 {|"Bad Request"|}
let unauthorized = return_json 401 {|"Unauthorized"|}
let forbidden = return_json 403 {|"Forbidden"|}
let not_found = return_json 404 {|"Not Found"|}
let method_not_allowed = return_json 405 {|"Method Not Allowed"|}
let precondition_failed = return_json 412 {|"Precondition Failed"|}
let conflict = return_json 409 {|"Conflict"|}

let handle_ifmatch ifmatch current cont =
  match ifmatch with
  | None -> cont ()
  | Some x ->
      let* current = current () in
      if sha256_b64 current = x then cont () else precondition_failed

let handle_generic_error f =
  Lwt.catch f (function
    | Error error ->
        let request_status = { code = 400; status = "Bad Request"; error } in
        return_json 400 (string_of_request_status request_status)
    | exn ->
        let error = `GenericError (Printexc.to_string exn) in
        let request_status =
          { code = 500; status = "Internal Server Error"; error }
        in
        return_json 500 (string_of_request_status request_status))

let handle_get get =
  let@ () = handle_generic_error in
  let* x = get () in
  return_json 200 x

let handle_get_option get =
  let@ () = handle_generic_error in
  let* x = get () in
  match x with None -> not_found | Some x -> return_json 200 x

let get_configuration_uris () =
  {
    home = !Web_config.prefix ^ "/";
    logo = !Web_config.prefix ^ "/LOGO";
    belenios = Belenios_ui.Links.belenios;
    source_code = !Web_config.prefix ^ "/belenios.tar.gz";
    tos = !Web_config.tos;
  }

let get_configuration () =
  let open Defaults in
  {
    restricted_mode = !Web_config.restricted_mode;
    vendor = !Web_config.vendor;
    tos_last_update = !Web_config.tos_last_update;
    uris = get_configuration_uris ();
    belenios_version = Version.version;
    belenios_build = Version.build;
    spec_version = Version.spec;
    api_version = 6;
    supported_crypto_versions =
      (let open Belenios.Election in
       List.map
         (function Version v -> int_of_version v)
         supported_crypto_versions);
    supported_booth_versions;
    authentications =
      List.map
        (function
          | `BuiltinPassword -> `Password
          | `BuiltinCAS -> `CAS
          | `Export a ->
              `Configured
                {
                  configured_instance = a.auth_instance;
                  configured_system = a.auth_system;
                })
        !Web_config.exported_auth_config;
    default_group = !Web_config.default_group;
    default_nh_group = !Web_config.nh_group;
    max_voters = !Web_config.maxmailsatonce;
    languages = Belenios_ui.Languages.available;
  }

let get_account (a : account) =
  {
    id = a.id;
    name = a.name;
    address = a.email;
    language = a.language;
    default_voter_languages = a.default_voter_languages;
    default_contact = a.default_contact;
  }

let put_account ((a, set) : account updatable) (b : api_account) =
  if b.address <> a.email then raise (Error (`CannotChange "address"));
  if b.id <> a.id then raise (Error (`CannotChange "id"));
  let a =
    {
      a with
      name = b.name;
      language = b.language;
      default_voter_languages = b.default_voter_languages;
      default_contact = b.default_contact;
    }
  in
  set a
