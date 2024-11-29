(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2021-2023 Inria                                           *)
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
open Common

(** Pure OCaml implementation of Ed25519 group *)

(* https://en.wikipedia.org/wiki/EdDSA *)

let q = Z.(shift_left one 255 - of_int 19)

let l =
  Z.(shift_left one 252 + of_string "27742317777372353535851937790883648493")

module F = MakeField (struct
  let q = q
end)

module Zq = MakeField (struct
  let q = l
end)

let a = F.(zero - one)
let d = F.(zero - (of_int 121665 * invert (of_int 121666)))

(* https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html *)

type t = F.t * F.t * F.t * F.t

let of_coordinates (x, y) = (x, y, F.one, F.(x * y))

let to_coordinates (x, y, z, _) =
  let open F in
  let invz = invert z in
  (x * invz, y * invz)

let curve x y z t =
  let open F in
  let x2 = x * x and y2 = y * y and z2 = z * z and t2 = t * t in
  (a * x2) + y2 - z2 - (d * t2)

let one = of_coordinates F.(zero, one)

let g =
  of_coordinates
    ( F.of_string
        "15112221349535400772501151409588531511454012693041857206046113283949847762202",
      F.of_string
        "46316835694926478169428394003475163141307993866256225615783033603165251855960"
    )

(* https://hyperelliptic.org/EFD/g1p/auto-twisted.html *)

let k = F.double d

let ( *~ ) (x1, y1, z1, t1) (x2, y2, z2, t2) =
  let open F in
  let a = (y1 - x1) * (y2 - x2) in
  let b = (y1 + x1) * (y2 + x2) in
  let c = t1 * k * t2 in
  let d = z1 * double z2 in
  let e = b - a in
  let f = d - c in
  let g = d + c in
  let h = b + a in
  let x3 = e * f in
  let y3 = g * h in
  let t3 = e * h in
  let z3 = f * g in
  (x3, y3, z3, t3)

let windowsize = 4
let windowmask = (1 lsl windowsize) - 1
let windowmaskZ = Z.of_int windowmask
let windowiterations = int_of_float (ceil (255. /. float_of_int windowsize))

let raw_pow p n =
  let t = Array.make (windowmask + 1) one in
  t.(1) <- p;
  let rec init i =
    if i < windowmask then (
      let z = t.(i / 2) in
      let s = z *~ z in
      t.(i) <- s;
      t.(i + 1) <- s *~ p;
      init (i + 2))
    else ()
  in
  init 2;
  let rec loop i s =
    if i >= 0 then
      let k = i * windowsize in
      let j = Z.(logand (shift_right n k) windowmaskZ |> to_int) in
      let s = s *~ t.(j) in
      let s =
        if i <> 0 then
          let rec loop i s = if i > 0 then loop (i - 1) (s *~ s) else s in
          loop windowsize s
        else s
      in
      loop (i - 1) s
    else s
  in
  loop (windowiterations - 1) one

let ( **~ ) p n = raw_pow p (Zq.to_Z n)

let compare (x1, y1, z1, _) (x2, y2, z2, _) =
  let a = F.(compare (x1 * z2) (x2 * z1)) in
  if a = 0 then F.(compare (y1 * z2) (y2 * z1)) else a

let ( =~ ) p1 p2 = compare p1 p2 = 0

let check ((x, y, z, t) as p) =
  F.(compare z zero > 0)
  && F.(compare (x * y) (z * t) = 0)
  && F.(compare (curve x y z t) zero = 0)
  && raw_pow p l =~ one

let is_even x = Z.(compare (logand (F.to_Z x) one) zero = 0)

let is_base_point =
  let four_fifth = F.(of_int 4 * invert (of_int 5)) in
  fun p ->
    check p
    &&
    let x, y = to_coordinates p in
    F.compare four_fifth y = 0 && is_even x

let invert (x, y, z, t) = F.(zero - x, y, z, zero - t)

let compress (x, y) =
  let open Z in
  let x = F.to_Z x and y = F.to_Z y in
  let b = shift_left (logand x one) 255 in
  logxor y b

let mask255 = Z.(shift_left one 255 - one)

let modsqrt_check, modsqrt =
  (* https://www.rieselprime.de/ziki/Modular_square_root *)
  let open Z in
  let five = of_int 5 and eight = of_int 8 in
  let exp = (q - five) / eight in
  ( (fun () -> Z.(compare (q mod eight) five = 0)),
    fun a ->
      let a = F.to_Z a in
      let v = powm (shift_left a 1) exp q in
      let i = erem (shift_left (a * v * v) 1) q in
      F.reduce (a * v * (i - one)) )

let uncompress raw =
  let open Z in
  let y = F.reduce (logand raw mask255) in
  let y2 = F.(y * y) in
  let x2 = F.((y2 - one) * invert ((d * y2) + one)) in
  let x = modsqrt x2 in
  if F.(compare (x * x) x2) = 0 then
    let xsign = logand (F.to_Z x) one in
    let rsign = shift_right raw 255 in
    let x = if compare xsign rsign = 0 then x else F.(zero - x) in
    Some (x, y)
  else None

let hex_size = 64

let to_string p =
  let r = Z.to_hex (compress (to_coordinates p)) in
  let n = String.length r in
  assert (n <= hex_size);
  if n < hex_size then String.make (hex_size - n) '0' ^ r else r

let of_string s =
  assert (String.length s = hex_size);
  match uncompress (Z.of_hex s) with
  | Some p -> of_coordinates p
  | None -> invalid_arg "Ed25519_pure.of_string"

let padding = 14
let bits_per_int = 8
let max_ints = (255 - padding) / bits_per_int

let of_ints =
  let mask_per_int = pred (1 lsl bits_per_int) in
  fun xs ->
    (* Koblitz method *)
    let open Z in
    let n = Array.length xs in
    let@ () =
     fun cont -> if n > max_ints then Error `Vector_size else cont ()
    in
    let rec encode_int i accu =
      if i < n then
        let x = xs.(i) land mask_per_int in
        let@ () =
         fun cont -> if x <> xs.(i) then Error `Int_size else cont ()
        in
        encode_int (succ i) (shift_left accu bits_per_int + of_int x)
      else Ok (shift_left accu padding)
    in
    let rec find_element accu =
      match uncompress accu with
      | None -> find_element Z.(accu + one)
      | Some p ->
          let p = of_coordinates p in
          if check p then p else find_element Z.(accu + one)
    in
    match encode_int 0 Z.zero with
    | Error _ as e -> e
    | Ok x -> Ok (find_element x)

let to_ints =
  let open Z in
  let mask_per_int = shift_left one bits_per_int - one in
  fun n p ->
    let x = compress (to_coordinates p) in
    let xs = Array.make n 0 in
    let rec decode_int i x =
      if i >= 0 then (
        xs.(i) <- to_int (logand x mask_per_int);
        decode_int (pred i) (shift_right x bits_per_int))
    in
    decode_int (pred n) (shift_right x padding);
    xs

let hash prefix xs =
  let x = prefix ^ map_and_concat_with_commas to_string xs in
  Zq.reduce_hex (sha256_hex x)

let hash_to_int p =
  let x, y = to_coordinates p in
  let x = F.to_Z x and y = F.to_Z y in
  Z.(hash_to_int (shift_left x 256 + y))

let description = "Ed25519"
let cofactor = Zq.of_int 8

let get_generator i =
  let s = Printf.sprintf "ggen|%d" i in
  let base = Z.(shift_right (of_hex (sha256_hex s)) 2) in
  let rec find_element accu =
    match uncompress accu with
    | None -> find_element Z.(accu + one)
    | Some p ->
        let p = of_coordinates p in
        p **~ cofactor
  in
  let h = find_element base in
  (* it is very unlikely (but theoretically possible) that one of the following assertions fails *)
  assert (compare h one <> 0);
  assert (compare h g <> 0);
  h

let selfcheck () =
  check one && is_base_point g && raw_pow g F.q =~ one && modsqrt_check ()
