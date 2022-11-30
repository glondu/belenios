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

open Belenios_platform.Platform

module Number : sig
  type t = Z.t
  val wrap : string -> t
  val unwrap : t -> string
end

module Uuid : sig
  type t
  val min_length : int
  val wrap : string -> t
  val unwrap : t -> string
end

module Hash : sig
  type t

  val wrap : string -> t
  val unwrap : t -> string

  val of_hex : string -> t
  val to_hex : t -> string

  val to_b64 : t -> string
  val of_b64 : string -> t

  val hash_string : string -> t
end

module Weight : sig
  type t
  val wrap : Yojson.Safe.t -> t
  val unwrap : t -> Yojson.Safe.t
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
