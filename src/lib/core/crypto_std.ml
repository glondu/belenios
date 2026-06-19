(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026 VCAST                                                *)
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

let strxor a b =
  let n = String.length a in
  if String.length b <> n then invalid_arg "strxor: different lengths";
  String.init n (fun i ->
      char_of_int (int_of_char a.[i] lxor int_of_char b.[i]))

(** I2OSP from RFC8017 *)
let i2osp x xLen =
  if x < 0 then invalid_arg "I2OSP: x negative";
  if
    let bitLen = 8 * xLen in
    bitLen <= Sys.int_size - 2 && x >= 1 lsl bitLen
  then invalid_arg "I2OSP: x too large";
  let result = Bytes.make xLen '\000' in
  let rec loop i x =
    if x > 0 then (
      Bytes.set result i (char_of_int (x land 0xFF));
      loop (i - 1) (x lsr 8))
  in
  loop (xLen - 1) x;
  Bytes.to_string result

(** OS2IP from RFC8017 *)
let os2ip x =
  let n = String.length x in
  let rec loop i accu =
    if i < n then
      loop (i + 1) Z.(logor (shift_left accu 8) (of_int @@ int_of_char x.[i]))
    else accu
  in
  loop 0 Z.zero

module type HASH_FUNCTION = sig
  val h : string -> string
  val b_in_bytes : int
  val s_in_bytes : int
end

module SHA256 : HASH_FUNCTION = struct
  let h x = Digestif.SHA256.(to_raw_string @@ digest_string x)
  let b_in_bytes = 32
  let s_in_bytes = 64
end

module Expand_message (H : HASH_FUNCTION) = struct
  open H

  (** expand_message_xmd from RFC9380 *)
  let expand_message_xmd ~dst msg len_in_bytes =
    let ell =
      Float.(to_int @@ ceil (of_int len_in_bytes /. of_int b_in_bytes))
    in
    if ell > 255 || len_in_bytes > 65535 then invalid_arg __FUNCTION__;
    let dst =
      if String.length dst > 255 then h ("H2C-OVERSIZE-DST-" ^ dst) else dst
    in
    let dst_prime = dst ^ i2osp (String.length dst) 1 in
    let z_pad = i2osp 0 s_in_bytes in
    let l_i_b_str = i2osp len_in_bytes 2 in
    let msg_prime = z_pad ^ msg ^ l_i_b_str ^ i2osp 0 1 ^ dst_prime in
    let uniform_bytes = Buffer.create len_in_bytes in
    let b_0 = h msg_prime in
    let b_1 = h (b_0 ^ i2osp 1 1 ^ dst_prime) in
    Buffer.add_string uniform_bytes b_1;
    let rec loop b_iprev i =
      if i <= ell then (
        let b_i = h (strxor b_0 b_iprev ^ i2osp i 1 ^ dst_prime) in
        Buffer.add_string uniform_bytes b_i;
        loop b_i (i + 1))
    in
    loop b_1 2;
    Buffer.sub uniform_bytes 0 len_in_bytes
end

module type HASH_TO_FIELD_PARAMS = sig
  val k : int
  val p : Z.t
  val m : int
  val expand_message : dst:string -> string -> int -> string
end

module Hash_to_field (F : HASH_TO_FIELD_PARAMS) = struct
  open F

  let l = Float.(to_int @@ ceil (of_int (Z.bit_length p + k) /. 8.))

  (** hash_to_field from RFC9380 *)
  let hash_to_field ~dst msg count =
    let len_in_bytes = count * m * l in
    let uniform_bytes = expand_message ~dst msg len_in_bytes in
    Array.init count (fun i ->
        Array.init m (fun j ->
            let elm_offset = l * (j + (i * m)) in
            let tv = String.sub uniform_bytes elm_offset l in
            Z.(os2ip tv mod p)))
end

module type PSEUDO_RANDOM_FUNCTION = sig
  val hLen : int
  val h : key:string -> string -> string
end

module HMAC_SHA1 : PSEUDO_RANDOM_FUNCTION = struct
  let hLen = 20
  let h ~key x = Digestif.SHA1.(to_raw_string @@ hmac_string ~key x)
end

module HMAC_SHA256 : PSEUDO_RANDOM_FUNCTION = struct
  let hLen = 32
  let h ~key x = Digestif.SHA256.(to_raw_string @@ hmac_string ~key x)
end

module Pbkdf2 (H : PSEUDO_RANDOM_FUNCTION) = struct
  open H

  (** PBKDF2 from RFC8018 *)
  let pbkdf2 ~iterations ~salt key dkLen =
    if Float.(of_int dkLen > 4294967295. *. of_int hLen) then
      Printf.ksprintf failwith "%s: derived key too long" __FUNCTION__;
    let l = Float.(to_int @@ ceil @@ (of_int dkLen /. of_int hLen)) in
    let c = iterations - 1 in
    let result = Bytes.create (hLen * l) in
    let one_iteration i =
      let u = h ~key (salt ^ i2osp i 4) in
      let rec loop c u accu =
        if c > 0 then
          let u' = h ~key u in
          loop (c - 1) u' (strxor accu u')
        else accu
      in
      loop c u u
    in
    for i = 1 to l do
      let offset = (i - 1) * hLen in
      String.blit (one_iteration i) 0 result offset hLen
    done;
    Bytes.sub_string result 0 dkLen
end
