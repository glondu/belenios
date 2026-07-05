(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2025-2025 Inria                                           *)
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
open Belenios_web_api
include Types

type metadata = message_metadata

let dummy_metadata : metadata =
  {
    uuid = Belenios.Uuid.dummy;
    admin_id = -1;
    title = "Belenios election";
    contact = None;
    has_weights = true;
    has_passwords = false;
    langs = [ Belenios.Language.(unwrap default) ];
  }

type text_message = { recipient : recipient; subject : string; body : string }

let hmac ~key x =
  let open Cryptokit in
  hash_string (MAC.hmac_sha256 key) x
  |> transform_string (Hexa.encode ())
  |> Belenios.Hash.of_hex

let wrap_message ~key (message : message) =
  let timestamp = Unix.gettimeofday () in
  let payload = { timestamp; message } in
  payload |> !+yojson_of_message_payload |> hmac ~key |> fun hmac ->
  (~payload, ~hmac)

let check_message ~key ~hmac:hmac' (message : message_payload) =
  message |> !+yojson_of_message_payload |> hmac ~key |> fun x -> hmac' = x
