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

open Belenios_platform
open Common

(** Ed25519 implementation using libsodium *)

module G = Ed25519_pure

module Make (B : LIBSODIUM_STUBS) = struct
  module Zq = G.Zq

  module E = struct
    open B

    let bytes = bytes ()
    let scalarbytes = scalarbytes ()
    let create_point () = Bytes.create bytes
    let z255 = Z.of_int 255

    let of_z_generic n z =
      let result = Bytes.create n in
      let rec loop i z =
        if i < n then (
          Bytes.set result i Z.(logand z z255 |> to_int |> char_of_int);
          loop (i + 1) Z.(shift_right z 8))
        else result
      in
      loop 0 z

    let scalar_of_number z =
      let z = Zq.to_Z z in
      of_z_generic scalarbytes z

    let number_of_scalar b =
      let rec loop i accu =
        if i >= 0 then
          loop (i - 1)
            Z.(
              logor (shift_left accu 8) (Bytes.get b i |> int_of_char |> of_int))
        else accu
      in
      loop (Bytes.length b - 1) Z.zero

    let hex_size = 64
    let () = assert (hex_size = 2 * bytes)

    let point_of_string s =
      assert (String.length s = hex_size);
      of_z_generic bytes (Z.of_hex s)

    let string_of_point p =
      let raw = number_of_scalar p in
      let r = Z.to_hex raw in
      let n = String.length r in
      assert (n <= hex_size);
      if n < hex_size then String.make (hex_size - n) '0' ^ r else r
  end

  type t = G.t

  let id = G.id
  let to_nacl p = E.point_of_string @@ G.to_string p
  let to_pure p = G.of_string @@ E.string_of_point p
  let check = G.check
  let one = G.one
  let g = G.g

  let ( *~ ) a b =
    let a' = to_nacl a and b' = to_nacl b in
    let r = E.create_point () in
    if B.add r a' b' = 0 then to_pure r else G.(a *~ b)

  let ( **~ ) p n =
    let p' = to_nacl p in
    let r = E.create_point () in
    if B.scalarmult r (E.scalar_of_number n) p' = 0 then to_pure r
    else G.(p **~ n)

  let compare = G.compare
  let ( =~ ) a b = compare a b = 0
  let invert = G.invert
  let to_string = G.to_string
  let of_string = G.of_string
  let witness = G.witness
  let max_ints = G.max_ints
  let bits_per_int = G.bits_per_int
  let to_ints = G.to_ints
  let of_ints = G.of_ints
  let get_generator = G.get_generator

  let hash ~dst prefix xs =
    let dst = dst ^ "-group_hash-Ed25519" in
    (Zq.hash ~dst 1 @@ prefix ^ map_and_concat_with_commas to_string xs).(0)

  let hash_to_int = G.hash_to_int
  let description = G.description
  let selfcheck () = (g **~ Zq.(zero - one)) *~ g =~ one && g *~ invert g =~ one
end
