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
open Common_types
open Signatures_core

val dst_prefix : string
val default_algorithm : string
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( ^^^ ) : string -> string -> string
val ( let@ ) : ('a -> 'b) -> 'a -> 'b
val ( let& ) : 'a option -> ('a -> 'b option) -> 'b option
val ( // ) : string -> string -> string
val datetime_now : unit -> int64
val add_days : int64 -> int -> int64
val finally : 'a -> (unit -> unit) -> 'a
val cast : ('a, 'b) Type.eq -> 'a -> 'b

type 'a smart_ref = { get : unit -> 'a; set : 'a -> unit }

val smart_ref : 'a -> 'a smart_ref
val sha256_hex : string -> string
val sha256_b64 : string -> string
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
end) : FIELD with type t = Z.t

val ( !$ ) : (string -> 'a) -> json -> 'a
val ( !& ) : ('a -> string) -> 'a -> json
val ( !* ) : (json -> 'a) -> string -> 'a
val ( !+ ) : ('a -> json) -> 'a -> string
val compare_b64 : string -> string -> int

module SSet : Set.S with type elt = string
module SMap : Map.S with type key = string
module IMap : Map.S with type key = int

val random_modulo : Z.t -> Crypto_primitives.rng -> Z.t
val check_modulo : Z.t -> Z.t -> bool
val generate_token : int -> string
val generate_numeric : int -> string
val sqrt : Z.t -> Z.t

module BabyStepGiantStep (G : GROUP) : sig
  val log : generator:G.t -> max:Z.t -> G.t -> G.Zq.t option
end

val split_on_br : string -> string list
val split_lines : string -> string list
val join_lines : string list -> string
val re_exec_opt : rex:Re.re -> string -> Re.Group.t option
val is_username : string -> bool
val is_email : ?blacklist:SSet.t -> string -> bool
val map_and_concat_with_commas : ('a -> string) -> 'a array -> string
val remove_special_characters : string -> string
val uniq_first : ?compare:('a -> 'a -> int) -> 'a list -> 'a list

module Voter : sig
  type t = voter [@@deriving yojson]

  val get : t -> string
  val get_weight : t -> Weight.t
  val get_recipient : t -> recipient
  val validate : t -> bool
  val generate : int -> t list
  val has_explicit_weights : t list -> bool
end
