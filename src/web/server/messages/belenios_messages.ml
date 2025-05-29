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

include Serializable_j

type text_message = { recipient : recipient; subject : string; body : string }

let hmac ~key x =
  let open Cryptokit in
  hash_string (MAC.hmac_sha256 key) x
  |> transform_string (Hexa.encode ())
  |> Belenios.Hash.of_hex

let wrap_message ~key (message : message) =
  let timestamp = Unix.gettimeofday () in
  { timestamp; message; hmac = None } |> string_of_message_payload |> hmac ~key
  |> fun x -> ({ timestamp; message; hmac = Some x } : message_payload)

let check_message ~key (message : message_payload) =
  { message with hmac = None } |> string_of_message_payload |> hmac ~key
  |> fun x -> message.hmac = Some x

module Serializable_t = Serializable_t
module Serializable_j = Serializable_j
