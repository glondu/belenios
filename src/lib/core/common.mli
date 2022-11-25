(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

val ( let@ ) : ('a -> 'b) -> 'a -> 'b
val ( let& ) : 'a option -> ('a -> 'b option) -> 'b option
val ( // ) : string -> string -> string

module Array : sig
  include module type of Array
  val for_all3 : ('a -> 'b -> 'c -> bool) -> 'a array -> 'b array -> 'c array -> bool
  val map3 : ('a -> 'b -> 'c -> 'd) ->
             'a array -> 'b array -> 'c array -> 'd array
  val findi : (int -> 'a -> 'b option) -> 'a array -> 'b option
end

module String : sig
  include module type of String
  val drop_prefix : prefix:string -> string -> string option
end

module List : sig
  include module type of List
  val join : 'a -> 'a list -> 'a list
end

module Option : sig
  include module type of Option
  val wrap : ('a -> 'b) -> 'a -> 'b option
  val unwrap : 'b -> 'a option -> ('a -> 'b) -> 'b
end

module Shape : sig
  type 'a t =
    | SAtomic of 'a
    | SArray of 'a t array
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

val save_to : string -> (Bi_outbuf.t -> 'a -> unit) -> 'a -> unit

val compare_b64 : string -> string -> int

module SSet : Set.S with type elt = string
module SMap : Map.S with type key = string

module IMap : Map.S with type key = int

val bytes_to_sample : Z.t -> int
val check_modulo : Z.t -> Z.t -> bool

module MakeGenerateToken (R : RANDOM) : sig
  val generate_token : ?length:int -> unit -> string R.t
  val generate_numeric : ?length:int -> unit -> string R.t
end

val sqrt : Z.t -> Z.t

module BabyStepGiantStep (G : GROUP) : sig
  val log : generator:G.t -> max:Z.t -> G.t -> Z.t option
end

val split_on_br : string -> string list
val split_lines : string -> string list
val strip_cred : string -> string

val supported_crypto_versions : int list
