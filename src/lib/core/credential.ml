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

open Belenios_platform.Platform
open Signatures
open Serializable_t
open Common

let token_length = 14
let n58 = Z.of_int 58
let n53 = Z.of_int 53

let format x =
  assert (token_length = 14);
  assert (String.length x = 15);
  String.sub x 0 3 ^ "-" ^ String.sub x 3 3 ^ "-" ^ String.sub x 6 3 ^ "-"
  ^ String.sub x 9 3 ^ "-" ^ String.sub x 12 3

let check_raw x =
  let rec loop i accu =
    if i < token_length then
      let& digit = String.index_opt b58_digits x.[i] in
      loop (i + 1) Z.((n58 * accu) + of_int digit)
    else Some accu
  in
  match (loop 0 Z.zero, String.index_opt b58_digits x.[token_length]) with
  | Some n, Some checksum -> Z.((n + of_int checksum) mod n53 =% zero)
  | _, _ -> false

let parse x =
  let n = String.length x in
  if n = token_length + 1 then if check_raw x then `Valid else `Invalid
  else if n = token_length + 5 then (
    assert (n = 19);
    if x.[3] = '-' && x.[7] = '-' && x.[11] = '-' && x.[15] = '-' then
      let actual =
        String.sub x 0 3 ^ String.sub x 4 3 ^ String.sub x 8 3
        ^ String.sub x 12 3 ^ String.sub x 16 3
      in
      if check_raw actual then `Valid else `Invalid
    else `Invalid)
  else if n = token_length + 3 then (
    assert (n = 17);
    if x.[5] = '-' && x.[11] = '-' then `MaybePassword else `Invalid)
  else `Invalid

let check x =
  match parse x with `Valid -> true | `Invalid | `MaybePassword -> false

type 'a t = { private_cred : string; private_key : 'a }

type batch = {
  private_creds : private_credentials;
  public_creds : public_credentials;
  public_with_ids : string list;
}

module type ELECTION = sig
  val uuid : Uuid.t
end

module type S = sig
  type private_key
  type public_key

  val generate : Voter.t list -> batch
  val derive : string -> (private_key, [ `Invalid | `MaybePassword ]) result
  val parse_public_credential : string -> (Weight.t * public_key) option
end

module Make (R : RANDOM) (G : GROUP) (E : ELECTION) = struct
  module GMap = Map.Make (G)

  let get_random_digit () =
    let x = R.random n58 in
    Z.to_int x

  let generate_raw_token () =
    let res = Bytes.create token_length in
    let rec loop i accu =
      if i < token_length then (
        let digit = get_random_digit () in
        Bytes.set res i b58_digits.[digit];
        loop (i + 1) Z.((n58 * accu) + of_int digit))
      else (Bytes.to_string res, accu)
    in
    loop 0 Z.zero

  let add_checksum (raw, value) =
    let checksum = 53 - Z.(to_int (value mod n53)) in
    raw ^ String.make 1 b58_digits.[checksum]

  let derive_raw x =
    let uuid = Uuid.unwrap E.uuid in
    let derived = pbkdf2_utf8 ~iterations:1000 ~salt:uuid x in
    Z.(of_hex derived) |> G.Zq.of_Z

  let derive x =
    match parse x with
    | `Valid -> Ok (derive_raw x)
    | (`Invalid | `MaybePassword) as x -> Error x

  let generate_one () =
    let private_cred = generate_raw_token () |> add_checksum |> format in
    let private_key = derive_raw private_cred in
    { private_cred; private_key }

  let generate voters =
    let implicit_weights = not (has_explicit_weights voters) in
    let privs, pubs =
      List.fold_left
        (fun (privs, pubs) v ->
          let _, username, weight = Voter.get v in
          let { private_cred; private_key } = generate_one () in
          ( (username, private_cred) :: privs,
            GMap.add G.(g **~ private_key) (weight, username) pubs ))
        ([], GMap.empty) voters
    in
    let serialize_with_id (e, (w, id)) =
      G.to_string e
      ^ (if implicit_weights then ","
         else Printf.sprintf ",%s" (Weight.to_string w))
      ^ Printf.sprintf ",%s" id
    in
    let serialize_public (e, (w, _)) =
      G.to_string e
      ^
      if implicit_weights then "" else Printf.sprintf ",%s" (Weight.to_string w)
    in
    let bindings = GMap.bindings pubs in
    {
      private_creds = List.rev privs;
      public_creds = List.map serialize_public bindings;
      public_with_ids = List.map serialize_with_id bindings;
    }

  let parse_public_credential s =
    try
      match String.index s ',' with
      | exception Not_found ->
          let x = G.of_string s in
          if G.check x then Some (Weight.one, x) else None
      | i ->
          let n = String.length s in
          let w = Weight.of_string (String.sub s (i + 1) (n - i - 1)) in
          let x = G.of_string (String.sub s 0 i) in
          if G.check x then Some (w, x) else None
    with _ -> None
end
