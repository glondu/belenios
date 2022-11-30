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

open Belenios_platform
open Platform
open Common

module Hash = struct
  type t = string

  let check x =
    String.length x = 64
    && String.for_all
         (function
          | '0'..'9' | 'a'..'f' -> true
          | _ -> false) x

  let of_hex x =
    if check x then x
    else Printf.ksprintf invalid_arg "%S is not a valid hex-encoded hash" x

  let to_hex x = x

  let of_b64 x =
    match Base64.decode ~pad:true (x ^ "=") with
    | Ok x when String.length x = 32 -> let `Hex x = Hex.of_string x in x
    | _ -> Printf.ksprintf invalid_arg "%S is not a valid b64-encoded hash" x

  let to_b64 x =
    match Base64.encode ~pad:false (Hex.to_string (`Hex x)) with
    | Ok x -> x
    | _ -> assert false

  let hash_string = sha256_hex
end

let sha256_b64 x = Hash.hash_string x |> Hash.to_b64

type hash = Hash.t

type 'a shape = 'a Shape.t =
  | SAtomic of 'a
  | SArray of 'a shape array

type weight = Z.t

let weight_of_raw_string x =
  try
    let x = Z.of_string x in
    if Z.(compare x zero >= 0) then
      x
    else
      raise Exit
  with _ -> Printf.ksprintf invalid_arg "%S is not a valid weight" x

let weight_of_int x =
  if x >= 0 then
    Z.of_int x
  else
    Printf.ksprintf invalid_arg "%d is not a valid weight" x

let weight_of_json = function
  | `Int x -> weight_of_int x
  | `Intlit x | `String x -> weight_of_raw_string x
  | _ -> invalid_arg "invalid weight"

let max_int31 = Z.of_string "1073741823"

let json_of_weight x =
  if Z.(compare x max_int31 <= 0) then
    `Int (Z.to_int x)
  else
    `String (Z.to_string x)

module Weight = struct
  include Z

  let max_expanded_weight = of_string "100000000000"

  let is_int x i = Z.(compare x (of_int i) = 0)

  let of_string x = weight_of_json (`String x)

  let expand ~total:_ x = x

  let reduce ~total:_ x = x

  let min a b =
    if compare a b < 0 then a else b

  let max a b =
    if compare a b > 0 then a else b
end

let extract_weight str =
  try
    let i = String.rindex str ',' in
    let w = Weight.of_string (String.sub str (i + 1) (String.length str - i - 1)) in
    String.sub str 0 i, w
  with _ -> str, Weight.one

let split_identity x =
  match String.split_on_char ',' x with
  | [address] -> address, address, Weight.one
  | [address; login] -> address, (if login = "" then address else login), Weight.one
  | [address; login; weight] ->
     address,
     (if login = "" then address else login),
     Weight.of_string weight
  | _ -> failwith "Common.split_identity"

let split_identity_opt x =
  match String.split_on_char ',' x with
  | [address] -> address, None, None
  | [address; login] -> address, (if login = "" then None else Some login), None
  | [address; login; weight] ->
     address,
     (if login = "" then None else Some login),
     Some (Weight.of_string weight)
  | _ -> failwith "Common.split_identity_opt"

type question_result =
  | RHomomorphic of weight array
  | RNonHomomorphic of int array array

let json_of_question_result = function
  | RHomomorphic xs ->
     xs
     |> Array.map json_of_weight
     |> (fun x -> `List (Array.to_list x))
  | RNonHomomorphic xs ->
     xs
     |> Array.map
          (fun ys ->
            ys
            |> Array.map (fun i -> `Int i)
            |> (fun y -> `List (Array.to_list y))
          )
     |> (fun x -> `List (Array.to_list x))
