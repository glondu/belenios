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

open Lwt.Syntax
open Ppx_yojson_conv_lib.Yojson_conv
open Belenios_platform

let () =
  Printexc.register_printer (function
    | Of_yojson_error (e, j) ->
        Some
          (Printf.sprintf "Of_yojson_error(%s, %s)" (Printexc.to_string e)
             (Yojson.Safe.to_string j))
    | _ -> None)

let b58_digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
let yojson_of_generic_string to_string x = yojson_of_string @@ to_string x
let generic_string_of_yojson of_string x = of_string @@ string_of_yojson x

type 'a serializers = { of_string : string -> 'a; to_string : 'a -> string }
type number = Z.t

module Json = struct
  type t = Yojson.Safe.t

  let yojson_of_t = Fun.id
  let t_of_yojson = Fun.id
  let to_string x = Yojson.Safe.to_string x
  let of_string x = Yojson.Safe.from_string x
end

type json = Json.t [@@deriving yojson]

let yojson_of_number = yojson_of_generic_string Z.to_string
let number_of_yojson = generic_string_of_yojson Z.of_string

module Uuid = struct
  type t = string

  let min_length = 14 (* at least 82 bits of entropy *)

  let check token =
    let n = String.length token in
    n >= min_length
    &&
    let rec loop i =
      if i >= 0 then
        match String.index_opt b58_digits token.[i] with
        | Some _ -> loop (i - 1)
        | None -> false
      else true
    in
    loop (n - 1)

  let of_string x =
    if check x then x
    else Printf.ksprintf invalid_arg "%S is not a valid UUID" x

  let to_string = Fun.id
  let yojson_of_t = yojson_of_generic_string to_string
  let t_of_yojson = generic_string_of_yojson of_string
  let dummy = of_string (String.make min_length '1')
end

type uuid = Uuid.t [@@deriving yojson]

module Hash = struct
  type t = string

  let compare = String.compare

  let check x =
    String.length x = 64
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         x

  let of_hex x =
    if check x then x
    else Printf.ksprintf invalid_arg "%S is not a valid hex-encoded hash" x

  let to_hex = Fun.id

  let of_b64 x =
    match Base64.decode ~pad:true (x ^ "=") with
    | Ok x when String.length x = 32 ->
        let (`Hex x) = Hex.of_string x in
        x
    | _ -> Printf.ksprintf invalid_arg "%S is not a valid b64-encoded hash" x

  let to_b64 x =
    match Base64.encode ~pad:false (Hex.to_string (`Hex x)) with
    | Ok x -> x
    | _ -> assert false

  let hash_string x = Digestif.SHA256.(x |> digest_string |> to_hex)
  let hash_yojson x = x |> Json.to_string |> hash_string
  let yojson_of_t = yojson_of_generic_string to_hex
  let t_of_yojson = generic_string_of_yojson of_hex
end

type hash = Hash.t [@@deriving yojson]

module Weight = struct
  type t = int64 [@@deriving yojson]

  let zero = 0L
  let one = 1L

  let of_int x =
    if x >= 0 then Int64.of_int x
    else Printf.ksprintf invalid_arg "%d is not a valid weight" x

  let of_string x =
    try
      let x = Int64.of_string x in
      if Int64.(compare x zero >= 0) then x else raise Exit
    with _ -> Printf.ksprintf invalid_arg "%S is not a valid weight" x

  let of_Z x =
    try
      let x = Z.to_int64 x in
      if Int64.(compare x zero >= 0) then x else raise Exit
    with _ ->
      Printf.ksprintf invalid_arg "%s is not a valid weight" (Z.to_string x)

  let to_string = Int64.to_string
  let to_Z = Z.of_int64
  let ( + ) = Int64.add
  let max_weight = 100000000000L
  let min = Int64.min
  let max = Int64.max
  let compare = Int64.compare
end

type weight = Weight.t [@@deriving yojson]

module Array = struct
  include Stdlib.Array

  let mapi2 f a b =
    let n = Array.length a in
    if n = Array.length b then Array.init n (fun i -> f i a.(i) b.(i))
    else invalid_arg "Array.mapi2"

  let for_all3 f a b c =
    let n = Array.length a in
    n = Array.length b
    && n = Array.length c
    &&
    let rec check i =
      if i >= 0 then f a.(i) b.(i) c.(i) && check (pred i) else true
    in
    check (pred n)

  let map3 f a b c = Array.mapi (fun i ai -> f ai b.(i) c.(i)) a

  let findi f a =
    let n = Array.length a in
    let rec loop i =
      if i < n then
        match f i a.(i) with None -> loop (i + 1) | Some _ as x -> x
      else None
    in
    loop 0

  let for_alli f a =
    let n = Array.length a in
    let rec loop i = if i < n then f i a.(i) && loop (i + 1) else true in
    loop 0

  let init_lwt n f =
    if n = 0 then Lwt.return [||]
    else
      let* x = f 0 in
      let r = Array.make n x in
      let rec loop i =
        if i < n then (
          let* x = f i in
          r.(i) <- x;
          loop (i + 1))
        else Lwt.return_unit
      in
      let* () = loop 1 in
      Lwt.return r
end

module Shape = struct
  type 'a t = Atomic of 'a | Array of 'a t array

  let rec yojson_of_t to_yojson : 'a t -> json = function
    | Array xs -> `List (Array.map (yojson_of_t to_yojson) xs |> Array.to_list)
    | Atomic o -> to_yojson o

  let rec t_of_yojson of_yojson : json -> 'a t = function
    | `List xs -> Array (List.map (t_of_yojson of_yojson) xs |> Array.of_list)
    | o -> Atomic (of_yojson o)

  let of_array x = Array (Array.map (fun x -> Atomic x) x)

  let to_array = function
    | Atomic _ -> invalid_arg "Shape.to_array"
    | Array xs ->
        Array.map
          (function Atomic x -> x | Array _ -> invalid_arg "Shape.to_array")
          xs

  let to_shape_array = function
    | Atomic _ -> invalid_arg "Shape.to_shape_array"
    | Array xs -> xs

  let rec map f = function
    | Atomic x -> Atomic (f x)
    | Array x -> Array (Array.map (map f) x)

  let rec map2 f a b =
    match (a, b) with
    | Atomic x, Atomic y -> Atomic (f x y)
    | Array x, Array y -> Array (Array.map2 (map2 f) x y)
    | _, _ -> invalid_arg "Shape.map2"

  let rec flatten = function
    | Atomic x -> [ x ]
    | Array xs -> Array.map flatten xs |> Array.to_list |> List.flatten

  let split x = (map fst x, map snd x)

  let rec forall p = function
    | Atomic x -> p x
    | Array x -> Array.for_all (forall p) x

  let rec forall2 p x y =
    match (x, y) with
    | Atomic x, Atomic y -> p x y
    | Array x, Array y -> Array.for_all2 (forall2 p) x y
    | _, _ -> invalid_arg "Shape.forall2"

  let rec forall3 p x y z =
    match (x, y, z) with
    | Atomic x, Atomic y, Atomic z -> p x y z
    | Array x, Array y, Array z -> Array.for_all3 (forall3 p) x y z
    | _, _, _ -> invalid_arg "Shape.forall3"
end

type 'a shape = 'a Shape.t [@@deriving yojson]

(** {2 Misc} *)

type voter = {
  address : string option; [@yojson.option]
  login : string option; [@yojson.option]
  weight : weight option; [@yojson.option]
}
[@@deriving yojson]

type voter_list = voter list [@@deriving yojson]
type recipient = { name : string; address : string } [@@deriving yojson]

type 'a public_credential = {
  credential : 'a;
  weight : weight option; [@yojson.option]
  username : string option; [@yojson.option]
}
[@@deriving yojson]
