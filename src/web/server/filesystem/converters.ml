(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

open Belenios

let account =
  {
    of_string =
      (fun x ->
        let open Types in
        let {
          id;
          name;
          email;
          last_connected;
          authentications;
          consent;
          capabilities;
          language;
          voters_limit;
          preferences;
        } =
          !*account_of_yojson x
        in
        let open Belenios_storage_api in
        {
          id;
          name;
          email;
          last_connected;
          authentications;
          consent;
          capabilities;
          language;
          voters_limit;
          preferences;
        });
    to_string =
      (fun x ->
        let open Belenios_storage_api in
        let {
          id;
          name;
          email;
          last_connected;
          authentications;
          consent;
          capabilities;
          language;
          voters_limit;
          preferences;
        } =
          x
        in
        let open Types in
        let x =
          {
            id;
            name;
            email;
            last_connected;
            authentications;
            consent;
            capabilities;
            language;
            voters_limit;
            preferences;
          }
        in
        !+yojson_of_account x);
  }
