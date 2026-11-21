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

let string_based of_string to_string =
  Desc.Core.map Desc.Core.string of_string to_string

type 'a serializers = { of_string : string -> 'a; to_string : 'a -> string }
type number = Z.t

module Json = struct
  type t = Yojson.Safe.t

  let t_d = Desc.Core.json
  let to_string x = Yojson.Safe.to_string x
  let of_string x = Yojson.Safe.from_string x
end

type json = Json.t [@@deriving desc]

let number_d = string_based Z.of_string Z.to_string

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
  let t_d = string_based of_string to_string
  let dummy = of_string (String.make min_length '1')
end

type uuid = Uuid.t [@@deriving desc]

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
  let t_d = string_based of_hex to_hex
  let hash_desc d x = x |> Desc.Json.to_json d |> hash_yojson
end

type hash = Hash.t [@@deriving desc]

module Weight = struct
  type t = int64 [@@deriving desc]

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
  let to_float = Int64.to_float
  let ( + ) = Int64.add
  let max_weight = 100000000000L
  let min = Int64.min
  let max = Int64.max
  let compare = Int64.compare
end

type weight = Weight.t [@@deriving desc]

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

  (* Untagged: an array for [Array], the value itself for [Atomic]. *)
  let t_d a =
    let open Desc.Core in
    fix "shape" (fun self ->
        let array = case "Array" (array self) (fun x -> Array x)
        and atomic = case "Atomic" a (fun x -> Atomic x) in
        untagged [ Any array; Any atomic ] (function
          | Array x -> Value (array, x)
          | Atomic x -> Value (atomic, x)))

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

type 'a shape = 'a Shape.t [@@deriving desc]

module SMap = struct
  include Map.Make (String)

  let t_d v = Desc.Core.map (Desc.Core.sorted_assoc v) of_list to_list
end

type 'a smap = 'a SMap.t [@@deriving desc]

module HMap = struct
  include Map.Make (Hash)

  let t_d v =
    Desc.Core.map (Desc.Core.sorted_assoc v)
      (List.fold_left (fun accu (a, b) -> add (Hash.of_hex a) b accu) empty)
      (fun xs -> bindings xs |> List.map (fun (a, b) -> (Hash.to_hex a, b)))
end

type 'a hmap = 'a HMap.t [@@deriving desc]

(** {2 Misc} *)

type voter = { login : string; address : string option; weight : weight option }
[@@deriving desc]

type voter_list = voter list [@@deriving desc]
type voters = voter smap [@@deriving desc]
type recipient = { name : string; address : string } [@@deriving desc]

type 'a public_credential = {
  credential : 'a;
  weight : weight option;
  username : string option;
}
[@@deriving desc]
