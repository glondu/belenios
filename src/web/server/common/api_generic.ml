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
open Web_serializable_t
open Belenios_core.Common
open Belenios_api.Serializable_j
open Web_common

let ( let& ) = Option.bind

type token = { expiration : datetime; account : account }

let tokens = ref SMap.empty

let new_token account =
  let token = generate_token ~length:22 () in
  let expiration = Period.add (Datetime.now ()) (Period.day 1) in
  tokens := SMap.add token { expiration; account } !tokens;
  Lwt.return token

let filter tokens =
  let now = Datetime.now () in
  SMap.filter
    (fun _ { expiration; _ } -> Datetime.compare now expiration < 0)
    tokens

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

exception Error of Belenios_api.Serializable_t.error

type result = int * string
type body = { run : 'a. (string -> 'a) -> ('a -> result Lwt.t) -> result Lwt.t }

let ok = Lwt.return (200, "{}")
let bad_request = Lwt.return (400, "\"Bad Request\"")
let unauthorized = Lwt.return (401, "\"Unauthorized\"")
let forbidden = Lwt.return (403, "\"Forbidden\"")
let not_found = Lwt.return (404, "\"Not Found\"")
let method_not_allowed = Lwt.return (405, "\"Method Not Allowed\"")
let precondition_failed = Lwt.return (412, "\"Precondition Failed\"")

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
        Lwt.return (400, string_of_request_status request_status)
    | _ -> bad_request)

let handle_get get =
  let@ () = handle_generic_error in
  let* x = get () in
  Lwt.return (200, x)

let handle_get_option get =
  let@ () = handle_generic_error in
  let* x = get () in
  match x with None -> not_found | Some x -> Lwt.return (200, x)

let get_configuration () =
  let open Web_defaults in
  {
    privacy_policy = !Web_config.gdpr_uri;
    belenios_version = Belenios_platform.Version.version;
    belenios_build = Belenios_platform.Version.build;
    spec_version = Belenios_platform.Version.spec;
    api_version = 3;
    supported_crypto_versions;
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

let put_account (a : account) (b : api_account) =
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
  Accounts.update_account a
