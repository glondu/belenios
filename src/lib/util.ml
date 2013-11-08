(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2013 Inria                                           *)
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

let ( |> ) x f = f x
let ( =% ) = Z.equal

module Array = struct
  include Array

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

  let fforall f xs =
    let rec loop_outer i =
      if i >= 0 then
        let x = xs.(i) in
        let n = Array.length x in
        let rec loop_inner j =
          if j >= 0 then f x.(j) && loop_inner (pred j)
          else true
        in loop_inner (pred n)
      else true
    in
    let n = Array.length xs in
    loop_outer (pred n)

  let fforall2 f xs ys =
    let rec loop_outer i =
      if i >= 0 then
        let x = xs.(i) and y = ys.(i) in
        let n = Array.length x in
        n = Array.length y &&
        let rec loop_inner j =
          if j >= 0 then f x.(j) y.(j) && loop_inner (pred j)
          else loop_outer (pred i)
        in loop_inner (pred n)
      else true
    in
    let n = Array.length xs in
    n = Array.length ys &&
    loop_outer (pred n)

  let fforall3 f xs ys zs =
    let rec loop_outer i =
      if i >= 0 then
        let x = xs.(i) and y = ys.(i) and z = zs.(i) in
        let n = Array.length x in
        n = Array.length y &&
        n = Array.length z &&
        let rec loop_inner j =
          if j >= 0 then f x.(j) y.(j) z.(j) && loop_inner (pred j)
          else loop_outer (pred i)
        in loop_inner (pred n)
      else true
    in
    let n = Array.length xs in
    n = Array.length ys &&
    n = Array.length zs &&
    loop_outer (pred n)

  let map2 f a b =
    Array.mapi (fun i ai -> f ai b.(i)) a

  let map3 f a b c =
    Array.mapi (fun i ai -> f ai b.(i) c.(i)) a

  let mmap f a =
    Array.map (fun ai ->
      Array.map f ai
    ) a

  let mmap2 f a b =
    Array.mapi (fun i ai ->
      let bi = b.(i) in
      Array.mapi (fun j aj ->
        f aj bi.(j)
      ) ai
    ) a

  let mmap3 f a b c =
    Array.mapi (fun i ai ->
      let bi = b.(i) and ci = c.(i) in
      Array.mapi (fun j aj ->
        f aj bi.(j) ci.(j)
      ) ai
    ) a

  let ssplit a =
    mmap fst a, mmap snd a
end

module String = struct
  include String

  let map f s =
    let n = String.length s in
    let res = String.create n in
    for i = 0 to n-1 do res.[i] <- f s.[i] done;
    res

  let startswith x s =
    let xn = String.length x and sn = String.length s in
    xn >= sn && String.sub x 0 sn = s
end

let rec list_join sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: list_join sep xs

let sha256_hex x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Hexa.encode ())
)

let sha256_b64 x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
)

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let int_msb i =
  let result = String.create 4 in
  result.[0] <- char_of_int (i lsr 24);
  result.[1] <- char_of_int ((i lsr 16) land 0xff);
  result.[2] <- char_of_int ((i lsr 8) land 0xff);
  result.[3] <- char_of_int (i land 0xff);
  result

let xor a b =
  let n = String.length a in
  assert (n = String.length b);
  let result = String.create n in
  for i = 0 to n-1 do
    result.[i] <- char_of_int (int_of_char a.[i] lxor int_of_char b.[i])
  done;
  result

let pbkdf2 ~prf ~salt ~iterations ~size password =
  let c = iterations - 1 in
  let hLen = (prf password)#hash_size in
  let result = String.create (hLen * size) in
  let one_iteration i =
    let u = Cryptokit.hash_string (prf password) (salt ^ int_msb i) in
    let rec loop c u accu =
      if c > 0 then
        let u' = Cryptokit.hash_string (prf password) u in
        loop (c-1) u' (xor accu u')
      else accu
    in loop c u u
  in
  for i = 1 to size do
    let offset = (i-1) * hLen in
    String.blit (one_iteration i) 0 result offset hLen;
  done;
  result
