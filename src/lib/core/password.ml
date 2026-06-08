(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026 VCAST                                                *)
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

open Common
open Crypto_std.Pbkdf2 (Crypto_std.HMAC_SHA256)

let std_iterations = 600_000
let std_salt_length = 16

let check ~salt ~hash ~password =
  let password = String.trim password in
  match String.split_on_char '$' hash with
  | [ hash ] -> (~ok:(sha256_hex (salt ^ password) = hash), ~obsolete:true)
  | [ iterations; hash ] ->
      let iterations = int_of_string iterations in
      let (`Hex computed) =
        pbkdf2 ~iterations ~salt password 32 |> Hex.of_string
      in
      ( ~ok:(computed = hash),
        ~obsolete:(iterations < std_iterations
                  || String.length salt < std_salt_length) )
  | _ -> invalid_arg __FUNCTION__

let hash random ~password =
  let module R = (val random : Signatures_core.RANDOM) in
  let open Common.MakeGenerateToken (R) in
  let password = String.trim password in
  let salt = generate_token ~length:std_salt_length () in
  let (`Hex hash) =
    pbkdf2 ~iterations:std_iterations ~salt password 32 |> Hex.of_string
  in
  let hash = Printf.sprintf "%d$%s" std_iterations hash in
  (~salt, ~hash)
