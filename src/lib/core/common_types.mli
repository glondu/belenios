(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

val b58_digits : string

type number = Z.t [@@deriving yojson]

module Json : sig
  type t = Yojson.Safe.t [@@deriving yojson]

  val id : t Type.Id.t
  val to_string : t -> string
  val of_string : string -> t
end

type json = Json.t [@@deriving yojson]

module Uuid : sig
  type t [@@deriving yojson]

  val to_string : t -> string
  val of_string : string -> t
  val min_length : int
  val dummy : t
end

type uuid = Uuid.t [@@deriving yojson]

module Hash : sig
  type t [@@deriving yojson]

  val compare : t -> t -> int
  val of_hex : string -> t
  val to_hex : t -> string
  val to_b64 : t -> string
  val of_b64 : string -> t
  val hash_string : string -> t
  val hash_yojson : json -> t
end

type hash = Hash.t [@@deriving yojson]

module Weight : sig
  type t [@@deriving yojson]

  val zero : t
  val one : t
  val is_int : t -> int -> bool
  val ( + ) : t -> t -> t
  val expand : total:t -> t -> Z.t
  val reduce : total:t -> Z.t -> t
  val max_expanded_weight : Z.t
  val min : t -> t -> t
  val max : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
end

type weight = Weight.t [@@deriving yojson]

module Array : sig
  include module type of Stdlib.Array

  val mapi2 : (int -> 'a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

  val for_all3 :
    ('a -> 'b -> 'c -> bool) -> 'a array -> 'b array -> 'c array -> bool

  val map3 :
    ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array

  val findi : (int -> 'a -> 'b option) -> 'a array -> 'b option
  val for_alli : (int -> 'a -> bool) -> 'a array -> bool
  val init_lwt : int -> (int -> 'a Lwt.t) -> 'a array Lwt.t
end

module Shape : sig
  type 'a t = [ `Atomic of 'a | `Array of 'a t array ] [@@deriving yojson]

  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array
  val to_shape_array : 'a t -> 'a t array
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val flatten : 'a t -> 'a list
  val split : ('a * 'b) t -> 'a t * 'b t
  val forall : ('a -> bool) -> 'a t -> bool
  val forall2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val forall3 : ('a -> 'b -> 'c -> bool) -> 'a t -> 'b t -> 'c t -> bool
end

type 'a shape = 'a Shape.t [@@deriving yojson]

(** {2 Basic cryptographic datastructures} *)

type 'a ciphertext = { alpha : 'a; beta : 'a } [@@deriving yojson]
(** An ElGamal ciphertext. *)

type 'a proof = { challenge : 'a; response : 'a } [@@deriving yojson]
(** A Fiat-Shamir non-interactive zero-knowledge proof of knowledge (ZKP). *)

type 'a disjunctive_proof = 'a proof array [@@deriving yojson]
(** A disjunctive ZKP. The size of the array is the number of disjuncts. *)

(** {2 Misc} *)

type voter = {
  address : string option; [@yojson.option]
  login : string option; [@yojson.option]
  weight : weight option; [@yojson.option]
}
[@@deriving yojson]

type voter_list = voter list [@@deriving yojson]
type recipient = { name : string; address : string } [@@deriving yojson]

type 'a public_credential = {
  credential : 'a;
  weight : weight option; [@yojson.option]
  username : string option; [@yojson.option]
}
[@@deriving yojson]
