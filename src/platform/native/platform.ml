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

let debug x = prerr_endline x

let sha256_hex x =
  Cryptokit.(
    x |> hash_string (Hash.sha256 ()) |> transform_string (Hexa.encode ()))

let int_msb i =
  let result = Bytes.create 4 in
  Bytes.set result 0 (char_of_int (i lsr 24));
  Bytes.set result 1 (char_of_int ((i lsr 16) land 0xff));
  Bytes.set result 2 (char_of_int ((i lsr 8) land 0xff));
  Bytes.set result 3 (char_of_int (i land 0xff));
  Bytes.to_string result

let xor a b =
  let n = String.length a in
  assert (n = String.length b);
  String.init n (fun i ->
      char_of_int (int_of_char a.[i] lxor int_of_char b.[i]))

let pbkdf2 ~prf ~salt ~iterations ~size password =
  let c = iterations - 1 in
  let hLen = (prf password)#hash_size in
  let result = Bytes.create (hLen * size) in
  let one_iteration i =
    let u = Cryptokit.hash_string (prf password) (salt ^ int_msb i) in
    let rec loop c u accu =
      if c > 0 then
        let u' = Cryptokit.hash_string (prf password) u in
        loop (c - 1) u' (xor accu u')
      else accu
    in
    loop c u u
  in
  for i = 1 to size do
    let offset = (i - 1) * hLen in
    String.blit (one_iteration i) 0 result offset hLen
  done;
  Bytes.to_string result

let pbkdf2_generic toBits ~iterations ~salt x =
  let open Cryptokit in
  let salt = toBits salt in
  pbkdf2 ~prf:MAC.hmac_sha256 ~iterations ~size:1 ~salt x
  |> transform_string (Hexa.encode ())

let pbkdf2_utf8 = pbkdf2_generic (fun x -> x)

let aes_raw ~key ~data =
  begin
    let open Cryptokit in
    transform_string Cipher.(aes ~mode:ECB key Encrypt) data
  end
  [@alert "-crypto"]
(* OK for a single block *)

let aes_hex ~key ~data =
  let open Cryptokit in
  let key = transform_string (Hexa.decode ()) key in
  let data = transform_string (Hexa.decode ()) data in
  let output = aes_raw ~key ~data in
  transform_string (Hexa.encode ()) output

let read_i32 str i =
  let open Int32 in
  let ( ! ) x = of_int (int_of_char str.[i + x]) in
  logor (shift_left !0 24)
    (logor (shift_left !1 16) (logor (shift_left !2 8) !3))

let export_i32 x =
  let open Int32 in
  let ( ! ) i =
    String.make 1
      (char_of_int (to_int (logand 0xffl (shift_right_logical x i))))
  in
  !24 ^ !16 ^ !8 ^ !0

let xor128 x y =
  let r = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set r i (char_of_int (int_of_char x.[i] lxor int_of_char y.[i]))
  done;
  Bytes.to_string r

(********** Functions directly translated from SJCL **********)

let ccm_computeTag prf plaintext iv adata tlen ll =
  let l = String.length plaintext in
  let plaintext =
    plaintext
    ^ "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
  in
  let tlen = tlen / 8 in
  if tlen mod 2 <> 0 || tlen < 4 || tlen > 16 then
    invalid_arg "ccm: invalid tag length";
  let flags =
    (if String.length adata <> 0 then 1 lsl 6 else 0)
    lor ((tlen - 2) lsl 2)
    lor (ll - 1)
  in
  let mac =
    String.make 1 (char_of_int flags)
    ^ iv ^ "\000\000\000\000\000\000\000\000\000\000\000\000"
  in
  (* works only for "small enough" plaintext (length < 31 bits) *)
  let a = read_i32 mac 12 in
  let a = Int32.(logor a (of_int l)) in
  let mac = String.sub mac 0 12 ^ export_i32 a in
  let mac = ref (prf mac) in
  if String.length adata <> 0 then invalid_arg "ccm: adata not supported";
  let i = ref 0 in
  while !i < l do
    mac := prf (xor128 !mac (String.sub plaintext !i 16));
    i := !i + 16
  done;
  String.sub !mac 0 tlen

let ccm_ctrMode prf data iv tag tlen ll =
  let l = String.length data in
  let data =
    data ^ "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
  in
  let ctr =
    String.make 1 (char_of_int (ll - 1))
    ^ iv ^ "\000\000\000\000\000\000\000\000\000\000\000\000"
  in
  let ctr = ref (String.sub ctr 0 16) in
  let tag =
    tag ^ "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
  in
  let tag = String.sub (xor128 (prf !ctr) tag) 0 (tlen / 8) in
  let i = ref 0 in
  let res = ref "" in
  while !i < l do
    (* works only for "small enough" plaintexts (length < 31 bits) *)
    let c = Int32.succ (read_i32 !ctr 12) in
    ctr := String.sub !ctr 0 12 ^ export_i32 c;
    res := !res ^ xor128 (prf !ctr) (String.sub data !i 16);
    i := !i + 16
  done;
  (String.sub !res 0 l, tag)

let ccm_encrypt prf plaintext iv adata tlen =
  let ivl = String.length iv in
  let ol = String.length plaintext in
  if ivl < 7 then invalid_arg "ccm: iv must be at least 7 bytes";
  let l =
    let l = ref 2 in
    while !l < 4 && ol asr (8 * !l) <> 0 do
      incr l
    done;
    if !l < 15 - ivl then l := 15 - ivl;
    !l
  in
  let iv = String.sub iv 0 (15 - l) in
  let tag = ccm_computeTag prf plaintext iv adata tlen l in
  let out, tag = ccm_ctrMode prf plaintext iv tag tlen l in
  out ^ tag

let ccm_decrypt prf ciphertext iv adata tlen =
  let ivl = String.length iv in
  let ol = String.length ciphertext - (tlen / 8) in
  let out = String.sub ciphertext 0 ol in
  let tag = String.sub ciphertext ol (String.length ciphertext - ol) in
  if ivl < 7 then invalid_arg "ccm: iv must be at least 7 bytes";
  let l =
    let l = ref 2 in
    while !l < 4 && ol asr (8 * !l) <> 0 do
      incr l
    done;
    if !l < 15 - ivl then l := 15 - ivl;
    !l
  in
  let iv = String.sub iv 0 (15 - l) in
  let out, tag = ccm_ctrMode prf out iv tag tlen l in
  let tag2 = ccm_computeTag prf out iv adata tlen l in
  if tag <> tag2 then invalid_arg "ccm: tag doesn't match";
  out

(********** End of SJCL functions **********)

let encrypt ~key ~iv ~plaintext =
  let open Cryptokit in
  let key = transform_string (Hexa.decode ()) key in
  let iv = transform_string (Hexa.decode ()) iv in
  let prf data = aes_raw ~key ~data in
  let ciphertext = ccm_encrypt prf plaintext iv "" 64 in
  transform_string (Hexa.encode ()) ciphertext

let decrypt ~key ~iv ~ciphertext =
  let open Cryptokit in
  let key = transform_string (Hexa.decode ()) key in
  let iv = transform_string (Hexa.decode ()) iv in
  let ciphertext = transform_string (Hexa.decode ()) ciphertext in
  let prf data = aes_raw ~key ~data in
  let plaintext = ccm_decrypt prf ciphertext iv "" 64 in
  plaintext

type rng = Cryptokit.Random.rng

let secure_rng =
  if Version.debug && Sys.getenv_opt "BELENIOS_USE_URANDOM" <> None then
    Cryptokit.Random.device_rng "/dev/urandom"
  else Cryptokit.Random.secure_rng

let pseudo_rng = Cryptokit.Random.pseudo_rng
let random_string = Cryptokit.Random.string

module Z = struct
  include Z

  let of_hex x = of_string_base 16 x
  let to_hex x = format "%x" x
  let ( =% ) = equal
  let bit_length x = Stdlib.(String.length (to_bits x) * 8)
  let powm x a m = if Z.compare a Z.zero = 0 then Z.one else powm_sec x a m
  (* Warning: no efforts have been made to be constant time in the
     rest of the code. *)

  let hash_to_int = Z.hash
end

module Libsodium_stubs = struct
  type scalar = bytes
  type point = bytes

  external bytes : unit -> int = "belenios_libsodium_ed25519_bytes" [@@noalloc]

  external scalarbytes : unit -> int = "belenios_libsodium_ed25519_scalarbytes"
  [@@noalloc]

  external is_valid_point : point -> int
    = "belenios_libsodium_ed25519_is_valid_point"
  [@@noalloc]

  external scalarmult : point -> scalar -> point -> int
    = "belenios_libsodium_ed25519_scalarmult"
  [@@noalloc]

  external add : point -> point -> point -> int
    = "belenios_libsodium_ed25519_add"
  [@@noalloc]
end

let libsodium_stubs () =
  Some (module Libsodium_stubs : Signatures.LIBSODIUM_STUBS)
