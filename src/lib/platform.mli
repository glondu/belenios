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

val sha256_hex : string -> string
val sha256_b64 : string -> string
val pbkdf2_hex : iterations:int -> salt:string -> string -> string

type rng
val secure_rng : rng
val pseudo_rng : string -> rng
val random_string : rng -> int -> string

module Z : sig
  type t
  val zero : t
  val one : t
  val of_int : int -> t
  val of_string : string -> t
  val of_string_base : int -> string -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val erem : t -> t -> t
  val to_int : t -> int
  val to_string : t -> string
  val compare : t -> t -> int
  val ( =% ) : t -> t -> bool
  val geq : t -> t -> bool
  val lt : t -> t -> bool
  val powm : t -> t -> t -> t
  val invert : t -> t -> t
  val probab_prime : t -> int -> int
  val bit_length : t -> int
  val of_bits : string -> t
end
