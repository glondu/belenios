(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

module Array = struct
  include Array

  let exists f a =
    let n = Array.length a in
    (let rec check i =
       if i >= 0 then f a.(i) || check (pred i)
       else false
     in check (pred n))

  let forall f a =
    let n = Array.length a in
    (let rec check i =
       if i >= 0 then f a.(i) && check (pred i)
       else true
     in check (pred n))

  let forall2 f a b =
    let n = Array.length a in
    n = Array.length b &&
    (let rec check i =
       if i >= 0 then f a.(i) b.(i) && check (pred i)
       else true
     in check (pred n))

  let forall3 f a b c =
    let n = Array.length a in
    n = Array.length b &&
    n = Array.length c &&
    (let rec check i =
       if i >= 0 then f a.(i) b.(i) c.(i) && check (pred i)
       else true
     in check (pred n))

  let map2 f a b =
    Array.mapi (fun i ai -> f ai b.(i)) a

  let map3 f a b c =
    Array.mapi (fun i ai -> f ai b.(i) c.(i)) a

  let findi f a =
    let n = Array.length a in
    let rec loop i =
      if i < n then
        match f i a.(i) with
        | None -> loop (i+1)
        | Some _ as x -> x
      else None
    in loop 0

  let split a =
    Array.map fst a, Array.map snd a

end

module String = struct
  include String

  let startswith x s =
    let xn = String.length x and sn = String.length s in
    xn >= sn && String.sub x 0 sn = s
end

module List = struct
  include List

  let rec join sep = function
    | [] -> []
    | [x] -> [x]
    | x :: xs -> x :: sep :: join sep xs

  let rec filter_map f = function
    | [] -> []
    | x :: xs ->
       let ys = filter_map f xs in
       match f x with
       | None -> ys
       | Some y -> y :: ys
end

module Option = struct
  let iter f = function
    | None -> ()
    | Some x -> f x

  let get x default_value = match x with
    | None -> default_value
    | Some x -> x

  let map f = function
    | Some x -> Some (f x)
    | None -> None
end

module Shape = struct
  type 'a t =
    | SAtomic of 'a
    | SArray of 'a t array

  let of_array x =
    SArray (Array.map (fun x -> SAtomic x) x)

  let to_array = function
    | SAtomic _ -> invalid_arg "Shape.to_array"
    | SArray xs ->
       Array.map (function
           | SAtomic x -> x
           | SArray _ -> invalid_arg "Shape.to_array"
         ) xs

  let to_shape_array = function
    | SAtomic _ -> invalid_arg "Shape.to_shape_array"
    | SArray xs -> xs

  let rec map f = function
    | SAtomic x -> SAtomic (f x)
    | SArray x -> SArray (Array.map (map f) x)

  let rec map2 f a b =
    match a, b with
    | SAtomic x, SAtomic y -> SAtomic (f x y)
    | SArray x, SArray y -> SArray (Array.map2 (map2 f) x y)
    | _, _ -> invalid_arg "Shape.map2"

  let rec flatten = function
    | SAtomic x -> [x]
    | SArray xs -> Array.map flatten xs |> Array.to_list |> List.flatten

  let split x =
    map fst x, map snd x

  let rec forall p = function
    | SAtomic x -> p x
    | SArray x -> Array.forall (forall p) x

  let rec forall2 p x y =
    match x, y with
    | SAtomic x, SAtomic y -> p x y
    | SArray x, SArray y -> Array.forall2 (forall2 p) x y
    | _, _ -> invalid_arg "Shape.forall2"

  let rec forall3 p x y z =
    match x, y, z with
    | SAtomic x, SAtomic y, SAtomic z -> p x y z
    | SArray x, SArray y, SArray z -> Array.forall3 (forall3 p) x y z
    | _, _, _ -> invalid_arg "Shape.forall3"

end

let save_to filename writer x =
  let oc = open_out filename in
  let ob = Bi_outbuf.create_channel_writer oc in
  writer ob x;
  Bi_outbuf.add_char ob '\n';
  Bi_outbuf.flush_channel_writer ob;
  close_out oc;;

let b64_order = "+/0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"

let compare_b64 a b =
  let na = String.length a and nb = String.length b in
  let value_of c =
    match String.index_opt b64_order c with
    | Some i -> i
    | None -> -1
  in
  let rec loop i =
    match (i < na), (i < nb) with
    | true, true ->
       let diff = value_of a.[i] - value_of b.[i] in
       if diff = 0 then loop (i+1) else diff
    | true, false -> 1
    | false, true -> -1
    | false, false -> 0
  in loop 0

module SSet = Set.Make(String)
module SMap = Map.Make(String)

(** Direct random monad *)

let bytes_to_sample q =
  (* we take 128 additional bits of random before the mod q, so that
     the statistical distance with a uniform distribution in [0,q[ is
     negligible *)
  Z.bit_length q / 8 + 17

let check_modulo p x = Z.(compare x zero >= 0 && compare x p < 0)

module DirectRandom = struct
  type 'a t = 'a
  let yield () = ()
  let return x = x
  let bind x f = f x
  let fail e = raise e

  let prng = lazy (pseudo_rng (random_string secure_rng 16))

  let random q =
    let size = bytes_to_sample q in
    let r = random_string (Lazy.force prng) size in
    Z.(of_bits r mod q)
end
