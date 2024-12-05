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
open Serializable_j
open Common

(** Finite field arithmetic *)

module type GROUP = Signatures.GROUP with type t = Z.t

let make description ff_params =
  let { p; q; g; embedding } = ff_params in
  let module G = struct
    open Z

    module Zq = MakeField (struct
      let q = q
    end)

    type t = Z.t

    let p = p
    let one = Z.one
    let g = g
    let ( *~ ) a b = a * b mod p
    let ( **~ ) a b = powm a (Zq.to_Z b) p
    let invert x = Z.invert x p
    let ( =~ ) = Z.( =% )
    let check x = check_modulo p x && powm x q p =~ one
    let to_string = Z.to_string
    let of_string = Z.of_string

    let max_ints, bits_per_int =
      match embedding with
      | None -> (0, 0)
      | Some { padding; bits_per_int } ->
          (Stdlib.((Z.bit_length p - padding) / bits_per_int), bits_per_int)

    let of_ints =
      match embedding with
      | None -> fun _ -> Error `No_encoding
      | Some { padding; bits_per_int } -> (
          let mask_per_int = pred (1 lsl bits_per_int) in
          fun xs ->
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
                encode_int (succ i) (Z.shift_left accu bits_per_int + of_int x)
              else Ok (Z.shift_left accu padding)
            in
            let rec find_element accu =
              if check accu then accu else find_element (accu + one)
            in
            match encode_int 0 zero with
            | Error _ as e -> e
            | Ok x -> Ok (find_element x))

    let to_ints =
      match embedding with
      | None -> fun _ -> failwith "Group_field.to_bits: missing parameters"
      | Some { padding; bits_per_int } ->
          let mask_per_int = shift_left one bits_per_int - one in
          fun n x ->
            let xs = Array.make n 0 in
            let rec decode_int i x =
              if i >= 0 then (
                xs.(i) <- to_int (logand x mask_per_int);
                decode_int (pred i) (shift_right x bits_per_int))
            in
            decode_int (pred n) (shift_right x padding);
            xs

    let hash prefix xs =
      let x = prefix ^ map_and_concat_with_commas Z.to_string xs in
      Zq.reduce_hex (sha256_hex x)

    let hash_to_int = Z.hash_to_int
    let compare = Z.compare

    let get_generator =
      let cofactor = Z.((p - one) / q) in
      fun i ->
        let s = Printf.sprintf "ggen|%d" i in
        let h = Z.of_hex (sha256_hex s) in
        let h = Z.powm h cofactor p in
        (* it is very unlikely (but theoretically possible) that one of the following assertions fails *)
        assert (Z.(compare h zero) <> 0);
        assert (Z.(compare h one) <> 0);
        assert (Z.(compare h g) <> 0);
        h

    let description = description
  end in
  (module G : GROUP)
