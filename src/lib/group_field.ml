(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Serializable_j
open Common

(** Helper functions *)

let check_modulo p x = Z.(geq x zero && lt x p)

let map_and_concat_with_commas f xs =
  let n = Array.length xs in
  let res = Buffer.create (n * 1024) in
  for i = 0 to n-1 do
    Buffer.add_string res (f xs.(i));
    Buffer.add_char res ',';
  done;
  let size = Buffer.length res - 1 in
  if size > 0 then Buffer.sub res 0 size else ""

(** Finite field arithmetic *)

let check_params {p; q; g} =
  Z.probab_prime p 20 > 0 &&
  Z.probab_prime q 20 > 0 &&
  check_modulo p g &&
  check_modulo p q &&
  Z.(powm g q p =% one)

module type GROUP = Signatures.GROUP
  with type t = Z.t
  and type group = ff_params

let unsafe_make group =
  let {p; q; g} = group in
  let module G = struct
    open Z
    type t = Z.t
    let p = p
    let q = q
    let one = Z.one
    let g = g
    let ( *~ ) a b = a * b mod p
    let ( **~ ) a b = powm a b p
    let invert x = Z.invert x p
    let ( =~ ) = Z.( =% )
    let check x = check_modulo p x && x **~ q =~ one
    let to_string = Z.to_string
    let of_string = Z.of_string

    let read state buf =
      match Yojson.Safe.from_lexbuf ~stream:true state buf with
      | `String s -> Z.of_string s
      | _ -> invalid_arg "Group_field.read: a string was expected"

    let write buf x =
      Bi_outbuf.add_char buf '"';
      Bi_outbuf.add_string buf (Z.to_string x);
      Bi_outbuf.add_char buf '"'

    let hash prefix xs =
      let x = prefix ^ (map_and_concat_with_commas Z.to_string xs) in
      let z = Z.of_string_base 16 (sha256_hex x) in
      Z.(z mod q)
    let compare = Z.compare
    type group = ff_params
    let group = group
    let write_group = write_ff_params

  end in (module G : GROUP)

let make group =
  if check_params group then unsafe_make group
  else invalid_arg "incorrect finite field parameters"
