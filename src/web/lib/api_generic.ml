(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Belenios_api.Serializable_t
open Web_serializable_builtin_t
open Web_common

let ( let& ) = Option.bind

type token = {
    expiration : datetime;
    account : account;
  }

let tokens = ref SMap.empty

let new_token account =
  let* token = generate_token ~length:22 () in
  let expiration = datetime_add (now ()) (day 1) in
  tokens := SMap.add token {expiration; account} !tokens;
  Lwt.return token

let filter tokens =
  let now = now () in
  SMap.filter (fun _ {expiration; _} -> datetime_compare now expiration < 0) tokens

let lookup_token token =
  tokens := filter !tokens;
  let& {account; _} = SMap.find_opt token !tokens in
  Some account

let invalidate_token token =
  tokens := SMap.remove token !tokens

let () =
  let@ a = Accounts.add_update_hook in
  let f {expiration; account} =
    let account = if a.account_id = account.account_id then a else account in
    {expiration; account}
  in
  tokens := SMap.map f !tokens;
  Lwt.return_unit

exception Error of string

type result = int * string

type body = {
    run : 'a. (string -> 'a) -> ('a -> result Lwt.t) -> result Lwt.t
}

let ok = Lwt.return (200, "{}")
let bad_request = Lwt.return (400, "\"Bad Request\"")
let unauthorized = Lwt.return (401, "\"Unauthorized\"")
let forbidden = Lwt.return (403, "\"Forbidden\"")
let not_found = Lwt.return (404, "\"Not Found\"")
let method_not_allowed = Lwt.return (405, "\"Method Not Allowed\"")

let handle_generic_error f =
  Lwt.catch f
    (function
     | Error msg ->
        let json =
          `Assoc [
              "status", `String "Bad Request";
              "error", `String msg;
            ]
        in
        Lwt.return (400, Yojson.Safe.to_string json)
     | _ -> bad_request
    )

let with_administrator token se f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some a when Accounts.check_account a se.se_owner -> f a
  | _ -> not_found

let with_administrator_or_credential_authority token se f =
  let@ token = Option.unwrap unauthorized token in
  if token = se.se_public_creds then (
    f `CredentialAuthority
  ) else (
    match lookup_token token with
    | Some a when Accounts.check_account a se.se_owner -> f (`Administrator a)
    | _ -> not_found
  )

let get_configuration () =
  {
    belenios_version = Belenios_platform.Belenios_version.version;
    belenios_build = Belenios_platform.Belenios_version.build;
    spec_version;
    api_version = 1;
    supported_crypto_versions;
    supported_booth_versions;
    authentications =
      begin
        List.map
          (function
           | `BuiltinPassword -> `Password
           | `BuiltinCAS -> `CAS
           | `Export a ->
              `Configured {
                  configured_instance = a.auth_instance;
                  configured_system = a.auth_system;
                }
          ) !Web_config.exported_auth_config
      end;
    default_group = !Web_config.default_group;
    default_nh_group = !Web_config.nh_group;
  }

let get_account a =
  {
    api_account_name = a.account_name;
    api_account_address = a.account_email;
  }

let put_account a b =
  if b.api_account_address <> a.account_email then
    raise (Error "cannot change address");
  let a = {a with account_name = b.api_account_name} in
  Accounts.update_account a
