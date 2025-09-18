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

module Debug : sig
  val debug : string -> unit
end

module Crypto_primitives : sig
  module type ENDECRYPT = sig
    val encrypt : key:string -> iv:string -> plaintext:string -> string Lwt.t
    (** [key] and [iv] in hex, [plaintext] UTF8 string, [ciphertext] in hex *)

    val decrypt :
      key:string -> iv:string -> ciphertext:string -> string option Lwt.t
  end

  val get_endecrypt : string -> (module ENDECRYPT)

  type rng

  val secure_rng : rng
  val pseudo_rng : string -> rng
  val random_string : rng -> int -> string
end

module Z : sig
  type t

  val zero : t
  val one : t
  val of_int : int -> t
  val of_string : string -> t
  val of_hex : string -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val erem : t -> t -> t
  val to_int : t -> int
  val to_string : t -> string
  val to_hex : t -> string
  val compare : t -> t -> int
  val ( =% ) : t -> t -> bool
  val powm : t -> t -> t -> t
  val invert : t -> t -> t
  val bit_length : t -> int
  val of_bits : string -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val hash_to_int : t -> int
end

module Libsodium_stubs : sig
  val make : unit -> (module Signatures.LIBSODIUM_STUBS) option
end
