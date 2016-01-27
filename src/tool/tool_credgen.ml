(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Platform
open Signatures
open Common

module type PARAMS = sig
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : unit -> string * string * string
end

module type PARSED_PARAMS = sig
  val uuid : Uuidm.t
  module G : GROUP
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module R = struct
    let uuid =
      match Uuidm.of_string P.uuid with
      | Some u -> u
      | None -> Printf.ksprintf failwith "%s is not a valid UUID" P.uuid
    module G = (val Group.of_string P.group : GROUP)
  end
  in (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct
  open P

  (* Some helpers *)

  (* Beware: the following must be changed in accordance with the booth! *)
  let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  let token_length = 14
  let n58 = Z.of_int 58
  let n53 = Z.of_int 53

  let derive x =
    let hex = derive_cred uuid x in
    let x = Z.(of_string_base 16 hex mod G.q) in
    let y = G.(g **~ x) in
    G.to_string y

  let prng = lazy (pseudo_rng (random_string secure_rng 16))

  let random_char () =
    int_of_char (random_string (Lazy.force prng) 1).[0]

  let generate_raw_token () =
    let res = Bytes.create token_length in
    let rec loop i accu =
      if i < token_length then (
        let digit = random_char () mod 58 in
        Bytes.set res i digits.[digit];
        loop (i+1) Z.(n58 * accu + of_int digit)
      ) else (Bytes.to_string res, accu)
    in loop 0 Z.zero

  let add_checksum (raw, value) =
    let checksum = 53 - Z.(to_int (value mod n53)) in
    raw ^ String.make 1 digits.[checksum]

  let compute_pub_and_hash priv =
    let pub = derive priv in
    let hashed = sha256_hex pub in
    priv, pub, hashed

  let generate () =
    generate_raw_token () |> add_checksum |> compute_pub_and_hash

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)

let int_length n =
  string_of_int n |> String.length

let rec find_first n first =
  if int_length first = int_length (first + n) then first
  else find_first n (10 * first)

let generate_ids n =
  (* choose the first id so that they all have the same length *)
  let first = find_first n 1 in
  let last = first + n - 1 in
  let rec loop last accu =
    if last < first then accu
    else loop (last-1) (string_of_int last :: accu)
  in loop last []
