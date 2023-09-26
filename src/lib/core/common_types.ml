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

let b58_digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

module Number = struct
  type t = Z.t

  let wrap = Z.of_string
  let unwrap = Z.to_string
end

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

  let wrap x =
    if check x then x
    else Printf.ksprintf invalid_arg "%S is not a valid UUID" x

  let unwrap x = x
  let dummy = wrap (String.make min_length '1')
end

module Hash = struct
  type t = string

  let check x =
    String.length x = 64
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         x

  let of_hex x =
    if check x then x
    else Printf.ksprintf invalid_arg "%S is not a valid hex-encoded hash" x

  let to_hex x = x

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

  let hash_string = sha256_hex
  let wrap = of_hex
  let unwrap = to_hex
end

let weight_of_raw_string x =
  try
    let x = Z.of_string x in
    if Z.(compare x zero >= 0) then x else raise Exit
  with _ -> Printf.ksprintf invalid_arg "%S is not a valid weight" x

let weight_of_int x =
  if x >= 0 then Z.of_int x
  else Printf.ksprintf invalid_arg "%d is not a valid weight" x

let weight_of_json = function
  | `Int x -> weight_of_int x
  | `Intlit x | `String x -> weight_of_raw_string x
  | _ -> invalid_arg "invalid weight"

let max_int31 = Z.of_string "1073741823"

let json_of_weight x =
  if Z.(compare x max_int31 <= 0) then `Int (Z.to_int x)
  else `String (Z.to_string x)

module Weight = struct
  include Z

  let max_expanded_weight = of_string "100000000000"
  let is_int x i = Z.(compare x (of_int i) = 0)
  let of_string x = weight_of_json (`String x)
  let expand ~total:_ x = x
  let reduce ~total:_ x = x
  let min a b = if compare a b < 0 then a else b
  let max a b = if compare a b > 0 then a else b
  let wrap = weight_of_json
  let unwrap = json_of_weight
end

module Question_signature = struct
  type _ t =
    | Nil : unit t
    | Homomorphic : 'a t -> ([ `Homomorphic ] * 'a) t
    | NonHomomorphic : int * 'a t -> ([ `NonHomomorphic ] * 'a) t
end

module Question_result = struct
  type t =
    [ `Homomorphic of Weight.t array | `NonHomomorphic of int array array ]
end

module Election_result = struct
  type _ t =
    | Nil : unit t
    | Homomorphic : Weight.t array * 'a t -> ([ `Homomorphic ] * 'a) t
    | NonHomomorphic : int array array * 'a t -> ([ `NonHomomorphic ] * 'a) t

  let wrap_homomorphic = function
    | `List xs -> xs |> List.map Weight.wrap |> Array.of_list
    | _ -> failwith "list expected in Election_result.wrap_homomorphic"

  let wrap_nonhomomorphic = function
    | `List xs ->
        xs
        |> List.map (function
             | `List xs ->
                 xs
                 |> List.map (function
                      | `Int i -> i
                      | _ ->
                          failwith
                            "int expected in \
                             Election_result.wrap_nonhomomorphic")
                 |> Array.of_list
             | _ ->
                 failwith
                   "(inner) list expected in \
                    Election_result.wrap_nonhomomorphic")
        |> Array.of_list
    | _ ->
        failwith "(outer) list expected in Election_result.wrap_nonhomomorphic"

  let wrap s x =
    let x =
      match x with
      | `List x -> x
      | _ -> failwith "list expected in Election_result.wrap"
    in
    let rec loop : 'a. 'a Question_signature.t -> Yojson.Safe.t list -> 'a t =
     fun (type a) (s : a Question_signature.t) x ->
      let r : a t =
        match (s, x) with
        | Nil, [] -> Nil
        | Nil, _ -> failwith "list too long in Election_result.wrap"
        | Homomorphic s, x :: xs -> Homomorphic (wrap_homomorphic x, loop s xs)
        | NonHomomorphic (_, s), x :: xs ->
            NonHomomorphic (wrap_nonhomomorphic x, loop s xs)
        | _, [] -> failwith "list too short in Election_result.wrap"
      in
      r
    in
    loop s x

  let unwrap_homomorphic xs =
    xs |> Array.map Weight.unwrap |> fun x -> `List (Array.to_list x)

  let unwrap_nonhomomorphic xs =
    xs
    |> Array.map (fun ys ->
           ys |> Array.map (fun i -> `Int i) |> fun y -> `List (Array.to_list y))
    |> fun x -> `List (Array.to_list x)

  let unwrap x =
    let rec loop : 'a. 'a t -> Yojson.Safe.t list =
     fun (type a) (x : a t) ->
      match x with
      | Nil -> []
      | Homomorphic (x, xs) -> unwrap_homomorphic x :: loop xs
      | NonHomomorphic (x, xs) -> unwrap_nonhomomorphic x :: loop xs
    in
    `List (loop x)

  let nth x i =
    if i < 0 then invalid_arg "Election_result.nth: negative index"
    else
      let rec loop : 'a. int -> 'a t -> Question_result.t =
       fun (type a) i (xs : a t) ->
        match (i, xs) with
        | _, Nil -> invalid_arg "Election_result.nth: out of bounds"
        | 0, Homomorphic (x, _) -> `Homomorphic x
        | 0, NonHomomorphic (x, _) -> `NonHomomorphic x
        | i, Homomorphic (_, xs) -> loop (i - 1) xs
        | i, NonHomomorphic (_, xs) -> loop (i - 1) xs
      in
      loop i x

  let map2 (type c) f xs ys =
    let n = Array.length ys in
    let fail () = invalid_arg "Election_result.map2" in
    let rec loop : 'a. int -> 'a t -> c list -> c list =
     fun (type a) i (xs : a t) accu ->
      match xs with
      | Nil -> if i = n then List.rev accu else fail ()
      | Homomorphic (x, xs) ->
          if i < n then
            let accu = f i (`Homomorphic x) ys.(i) :: accu in
            loop (i + 1) xs accu
          else fail ()
      | NonHomomorphic (x, xs) ->
          if i < n then
            let accu = f i (`NonHomomorphic x) ys.(i) :: accu in
            loop (i + 1) xs accu
          else fail ()
    in
    loop 0 xs []
end

module Array = struct
  include Stdlib.Array

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
end

module Shape = struct
  type 'a t = [ `Atomic of 'a | `Array of 'a t array ]

  let of_array x = `Array (Array.map (fun x -> `Atomic x) x)

  let to_array = function
    | `Atomic _ -> invalid_arg "Shape.to_array"
    | `Array xs ->
        Array.map
          (function `Atomic x -> x | `Array _ -> invalid_arg "Shape.to_array")
          xs

  let to_shape_array = function
    | `Atomic _ -> invalid_arg "Shape.to_shape_array"
    | `Array xs -> xs

  let rec map f = function
    | `Atomic x -> `Atomic (f x)
    | `Array x -> `Array (Array.map (map f) x)

  let rec map2 f a b =
    match (a, b) with
    | `Atomic x, `Atomic y -> `Atomic (f x y)
    | `Array x, `Array y -> `Array (Array.map2 (map2 f) x y)
    | _, _ -> invalid_arg "Shape.map2"

  let rec flatten = function
    | `Atomic x -> [ x ]
    | `Array xs -> Array.map flatten xs |> Array.to_list |> List.flatten

  let split x = (map fst x, map snd x)

  let rec forall p = function
    | `Atomic x -> p x
    | `Array x -> Array.for_all (forall p) x

  let rec forall2 p x y =
    match (x, y) with
    | `Atomic x, `Atomic y -> p x y
    | `Array x, `Array y -> Array.for_all2 (forall2 p) x y
    | _, _ -> invalid_arg "Shape.forall2"

  let rec forall3 p x y z =
    match (x, y, z) with
    | `Atomic x, `Atomic y, `Atomic z -> p x y z
    | `Array x, `Array y, `Array z -> Array.for_all3 (forall3 p) x y z
    | _, _, _ -> invalid_arg "Shape.forall3"
end

module Atd_shape_t = struct
  type 'a shape = 'a Shape.t
end

module Atd_shape_j = struct
  let rec write_shape write buf = function
    | `Atomic x -> write buf x
    | `Array xs -> Atdgen_runtime.Oj_run.write_array (write_shape write) buf xs

  let rec read_shape read state buf =
    Yojson.Safe.read_space state buf;
    let open Lexing in
    if buf.lex_curr_pos >= buf.lex_buffer_len then buf.refill_buff buf;
    if buf.lex_curr_pos >= buf.lex_buffer_len then
      Yojson.json_error "Unexpected end of input";
    if Bytes.get buf.lex_buffer buf.lex_curr_pos = '[' then
      `Array (Yojson.Safe.read_array (read_shape read) state buf)
    else `Atomic (read state buf)
end
