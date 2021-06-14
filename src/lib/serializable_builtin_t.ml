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

type number = Z.t
type uuid = string

let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
let min_uuid_length = 14 (* at least 82 bits of entropy *)

let check token =
  let n = String.length token in
  n >= min_uuid_length &&
    let rec loop i =
      if i >= 0 then
        match String.index_opt digits token.[i] with
        | Some _ -> loop (i-1)
        | None -> false
      else true
    in loop (n-1)

let uuid_of_raw_string x =
  if check x then x
  else Printf.ksprintf invalid_arg "%S is not a valid UUID" x

let raw_string_of_uuid x = x

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

  let two = of_string "2"
  let max_weight = of_string "100000"

  let of_string x = weight_of_json (`String x)
  let of_int x = weight_of_json (`Int x)

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
