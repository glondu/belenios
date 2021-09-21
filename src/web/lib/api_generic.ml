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

open Web_serializable_t
open Belenios_api.Serializable_t

exception Error of string

let get_configuration () =
  {
    belenios_version = Belenios_platform.Belenios_version.version;
    belenios_build = Belenios_platform.Belenios_version.build;
    api_version = 1;
    default_crypto_version = Belenios_core.Common.default_version;
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
