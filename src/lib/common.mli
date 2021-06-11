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
open Signatures_core

module Array : sig
  include module type of Array
  val exists : ('a -> bool) -> 'a array -> bool
  val forall : ('a -> bool) -> 'a array -> bool
  val forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  val forall3 : ('a -> 'b -> 'c -> bool) -> 'a array -> 'b array -> 'c array -> bool
  val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  val map3 : ('a -> 'b -> 'c -> 'd) ->
    'a array -> 'b array -> 'c array -> 'd array
  val findi : (int -> 'a -> 'b option) -> 'a array -> 'b option
  val split : ('a * 'b) array -> 'a array * 'b array
end

module String : sig
  include module type of String
  val startswith : string -> string -> bool
end

module List : sig
  include module type of List
  val join : 'a -> 'a list -> 'a list
  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
end

module Option : sig
  val iter : ('a -> unit) -> 'a option -> unit
  val get : 'a option -> 'a -> 'a
  val map : ('a -> 'b) -> 'a option -> 'b option
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

module Weight : sig
  type t
  val zero : t
  val one : t
  val two : t
  val max_weight : t
  val compare : t -> t -> int
  val ( + ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val of_string : string -> t
  val to_string : t -> string
  val of_int : int -> t
  val to_int : t -> int (* FIXME: weights can be bigger than int *)
end

val save_to : string -> (Bi_outbuf.t -> 'a -> unit) -> 'a -> unit

val compare_b64 : string -> string -> int

module SSet : Set.S with type elt = string
module SMap : Map.S with type key = string

val bytes_to_sample : Platform.Z.t -> int
val check_modulo : Platform.Z.t -> Platform.Z.t -> bool

module DirectRandom : RANDOM with type 'a t = 'a

module MakeGenerateToken (R : RANDOM) : sig
  val generate_token : ?length:int -> unit -> string R.t
  val generate_numeric : ?length:int -> unit -> string R.t
end

(** Input: [str = "something[,weight]"]
    Output:
    - if [weight] is an integer > 0, return [(something, weight)]
    - else, return [(str, 1)] *)
val extract_weight : string -> string * Weight.t

val split_identity : string -> string * string * Weight.t
val split_identity_opt : string -> string * string option * Weight.t option
