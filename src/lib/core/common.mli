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

open Belenios_platform
open Platform
open Signatures_core

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( ^^^ ) : string -> string -> string
val ( let@ ) : ('a -> 'b) -> 'a -> 'b
val ( let& ) : 'a option -> ('a -> 'b option) -> 'b option
val ( // ) : string -> string -> string
val ( ++ ) : 'a reader -> string -> 'a
val ( -- ) : 'a writer -> 'a -> string

type (_, _) eq = Refl : ('a, 'a) eq

val cast : ('a, 'b) eq -> 'a -> 'b

module Uuid = Common_types.Uuid
module Hash = Common_types.Hash
module Weight = Common_types.Weight
module Array = Common_types.Array
module Shape = Common_types.Shape

val sha256_hex : string -> string
val sha256_b64 : string -> string
val b58_digits : string
val encode_data_uri : ?charset:string -> mime_type:string -> string -> string

module String : sig
  include module type of String

  val drop_prefix : prefix:string -> string -> string option
end

module List : sig
  include module type of List

  val join : 'a -> 'a list -> 'a list
  val findi : (int -> 'a -> 'b option) -> 'a list -> 'b option
end

module Option : sig
  include module type of Option

  val wrap : ('a -> 'b) -> 'a -> 'b option
  val unwrap : 'b -> 'a option -> ('a -> 'b) -> 'b
end

module MakeField (_ : sig
  val q : Z.t
end) : FIELD

val sread : (string -> 'a) -> 'a reader
val swrite : ('a -> string) -> 'a writer
val save_to : string -> (Bi_outbuf.t -> 'a -> unit) -> 'a -> unit
val compare_b64 : string -> string -> int

module SSet : Set.S with type elt = string
module SMap : Map.S with type key = string
module IMap : Map.S with type key = int

val random_modulo : Z.t -> Crypto_primitives.rng -> Z.t
val check_modulo : Z.t -> Z.t -> bool

module MakeGenerateToken (_ : RANDOM) : sig
  val generate_token : ?length:int -> unit -> string
  val generate_numeric : ?length:int -> unit -> string
end

val generate_b58_token : rng:Crypto_primitives.rng -> length:int -> string
val sqrt : Z.t -> Z.t

module BabyStepGiantStep (G : GROUP) : sig
  val log : generator:G.t -> max:Z.t -> G.t -> G.Zq.t option
end

val split_on_br : string -> string list
val split_lines : string -> string list
val join_lines : string list -> string

val parse_public_credential :
  (string -> 'a) -> string -> 'a Serializable_core_t.public_credential

val strip_public_credential : string -> string
val extract_salt : string -> string option
val re_exec_opt : rex:Re.re -> string -> Re.Group.t option
val is_username : string -> bool
val is_email : ?blacklist:SSet.t -> string -> bool
val map_and_concat_with_commas : ('a -> string) -> 'a array -> string
val remove_special_characters : string -> string

module Voter : sig
  type t = [ `Plain | `Json ] * Serializable_core_t.voter

  val wrap : Yojson.Safe.t -> t
  val unwrap : t -> Yojson.Safe.t
  val to_string : t -> string
  val of_string : string -> t
  val list_to_string : t list -> string
  val list_of_string : string -> t list
  val get : t -> string * string * Weight.t
  val validate : t -> bool
  val generate : int -> t list
  val has_explicit_weights : t list -> bool
end
