(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

module Number = struct
  type t = Z.t
  let wrap = Z.of_string
  let unwrap = Z.to_string
end

module Uuid = struct

  type t = string

  let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  let min_length = 14 (* at least 82 bits of entropy *)

  let check token =
    let n = String.length token in
    n >= min_length
    && let rec loop i =
         if i >= 0 then
           match String.index_opt digits token.[i] with
           | Some _ -> loop (i-1)
           | None -> false
         else true
       in
       loop (n-1)

  let wrap x =
    if check x then x
    else Printf.ksprintf invalid_arg "%S is not a valid UUID" x

  let unwrap x = x

end

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
  let wrap = of_hex
  let unwrap = to_hex

end

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

  let wrap = weight_of_json
  let unwrap = json_of_weight

end
