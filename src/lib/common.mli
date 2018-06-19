(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Signatures

module Array : sig
  include module type of Array
  val exists : ('a -> bool) -> 'a array -> bool
  val forall : ('a -> bool) -> 'a array -> bool
  val forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  val fforall : ('a -> bool) -> 'a array array -> bool
  val fforall2 : ('a -> 'b -> bool) ->
    'a array array -> 'b array array -> bool
  val fforall3 : ('a -> 'b -> 'c -> bool) ->
    'a array array -> 'b array array -> 'c array array -> bool
  val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  val map3 : ('a -> 'b -> 'c -> 'd) ->
    'a array -> 'b array -> 'c array -> 'd array
  val mmap : ('a -> 'b) -> 'a array array -> 'b array array
  val mmap2 : ('a -> 'b -> 'c) ->
    'a array array -> 'b array array -> 'c array array
  val mmap3 : ('a -> 'b -> 'c -> 'd) ->
    'a array array -> 'b array array -> 'c array array -> 'd array array
  val ssplit : ('a * 'b) array array -> 'a array array * 'b array array
  val findi : (int -> 'a -> 'b option) -> 'a array -> 'b option
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
  val get : 'a option -> 'a -> 'a
  val map : ('a -> 'b) -> 'a option -> 'b option
end

val save_to : string -> (Bi_outbuf.t -> 'a -> unit) -> 'a -> unit

val compare_b64 : string -> string -> int

module SMap : Map.S with type key = string

val bytes_to_sample : Platform.Z.t -> int

module DirectRandom : RANDOM with type 'a t = 'a
