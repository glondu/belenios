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

(** Ed25519 implementation using libsodium *)

module G = Ed25519_pure

let l = G.q

module Make (B : Belenios_platform.Signatures.LIBSODIUM_STUBS) = struct
  module E = struct
    open B

    let bytes = bytes ()
    let scalarbytes = scalarbytes ()
    let create_point () = Bytes.create bytes
    let compare_points = Bytes.compare
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
      let z = Z.erem z l in
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

  type t = { mutable pure : G.t option; mutable nacl : B.point option }

  let get_as_pure p =
    match p.pure with
    | Some a -> a
    | None -> (
        match p.nacl with
        | Some a ->
            let s = E.string_of_point a in
            let b = G.of_string s in
            p.pure <- Some b;
            b
        | None -> failwith "inconsistency in Ed25519_libsodium.get_as_pure")

  let make_from_pure p = { pure = Some p; nacl = None }
  let make_from_nacl p = { pure = None; nacl = Some p }

  let get_as_nacl p =
    match p.nacl with
    | Some a -> a
    | None -> (
        match p.pure with
        | Some a ->
            let s = G.to_string a in
            let b = E.point_of_string s in
            p.nacl <- Some b;
            b
        | None -> failwith "inconsistency in Ed25519_libsodium.get_as_nacl")

  let check p =
    match (p.nacl, p.pure) with
    | Some a, _ -> B.is_valid_point a = 1 || G.check (get_as_pure p)
    | _, Some a -> G.check a
    | None, None -> failwith "inconsistency in Ed25519_libsodium.check"

  let one = make_from_pure G.one
  let g = make_from_pure G.g

  let ( *~ ) a b =
    let r = E.create_point () in
    if B.add r (get_as_nacl a) (get_as_nacl b) = 0 then make_from_nacl r
    else make_from_pure G.(get_as_pure a *~ get_as_pure b)

  let ( **~ ) p n =
    let r = E.create_point () in
    if B.scalarmult r (E.scalar_of_number n) (get_as_nacl p) = 0 then
      make_from_nacl r
    else make_from_pure G.(get_as_pure p **~ n)

  let compare a b =
    match (a.pure, b.pure, a.nacl, b.nacl) with
    | Some c, Some d, _, _ -> G.compare c d
    | _, _, Some c, Some d -> E.compare_points c d
    | _, _, Some c, _ -> E.compare_points c (get_as_nacl b)
    | _, _, _, Some d -> E.compare_points (get_as_nacl a) d
    | _, _, None, None -> G.compare (get_as_pure a) (get_as_pure b)

  let ( =~ ) a b = compare a b = 0
  let invert p = make_from_pure G.(invert (get_as_pure p))
  let to_string p = E.string_of_point (get_as_nacl p)
  let of_string s = make_from_nacl (E.point_of_string s)
  let to_ints n p = G.to_ints n (get_as_pure p)
  let of_ints xs = make_from_pure (G.of_ints xs)
  let get_generator i = make_from_pure (G.get_generator i)

  let hash prefix xs =
    let x = prefix ^ map_and_concat_with_commas to_string xs in
    let z = Z.of_hex (sha256_hex x) in
    Z.(z mod l)

  let hash_to_int p = G.hash_to_int (get_as_pure p)
  let description = "Ed25519"
  let q = l

  let selfcheck () =
    check one && check g
    && G.compare (get_as_pure g) G.g = 0
    && (g **~ Z.(l - one)) *~ g =~ one
    && g *~ invert g =~ one
end
