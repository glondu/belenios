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

module Debug = struct
  let debug x = prerr_endline x
end

module Crypto_primitives = struct
  module type ENDECRYPT = sig
    val encrypt : key:string -> iv:string -> plaintext:string -> string Lwt.t

    val decrypt :
      key:string -> iv:string -> ciphertext:string -> string option Lwt.t
  end

  module AES_GCM : ENDECRYPT = struct
    let encrypt ~key ~iv ~plaintext =
      let open Cryptokit in
      let key = transform_string (Hexa.decode ()) key in
      let iv = transform_string (Hexa.decode ()) iv in
      let transform = AEAD.aes_gcm ~iv key Encrypt in
      let ciphertext = auth_transform_string transform plaintext in
      Lwt.return @@ transform_string (Hexa.encode ()) ciphertext

    let decrypt ~key ~iv ~ciphertext =
      let open Cryptokit in
      let key = transform_string (Hexa.decode ()) key in
      let iv = transform_string (Hexa.decode ()) iv in
      let transform = AEAD.aes_gcm ~iv key Decrypt in
      let ciphertext = transform_string (Hexa.decode ()) ciphertext in
      match auth_check_transform_string transform ciphertext with
      | x -> Lwt.return x
      | exception Error _ -> Lwt.return_none
  end

  let get_endecrypt = function
    | "AES-GCM" -> (module AES_GCM : ENDECRYPT)
    | x -> Printf.ksprintf failwith "unknown algorithm: %s" x

  type rng = Cryptokit.Random.rng

  let secure_rng =
    if Version.debug && Sys.getenv_opt "BELENIOS_USE_URANDOM" <> None then
      Cryptokit.Random.device_rng "/dev/urandom"
    else Cryptokit.Random.secure_rng

  let pseudo_rng = Cryptokit.Random.pseudo_rng
  let random_string = Cryptokit.Random.string
end

module Z = struct
  include Z

  let of_hex x = of_string_base 16 x
  let to_hex x = format "%x" x
  let ( =% ) = equal
  let bit_length = Z.log2up
  let powm x a m = if Z.compare a Z.zero = 0 then Z.one else powm_sec x a m
  (* Warning: no efforts have been made to be constant time in the
     rest of the code. *)

  let hash_to_int = Z.hash
end

module Libsodium_stubs_mod = struct
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

module Libsodium_stubs = struct
  let make () = Some (module Libsodium_stubs_mod : Signatures.LIBSODIUM_STUBS)
end
