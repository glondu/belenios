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

module Datetime : sig
  type t
  val now : unit -> t
  val wrap : string -> t
  val unwrap : t -> string
  val compare : t -> t -> int
  val format : ?fmt:string -> t -> string
  val to_unixfloat : t -> float
  val from_unixfloat : float -> t
end

module Period : sig
  type t
  val day : int -> t
  val second : int -> t
  val add : Datetime.t -> t -> Datetime.t
  val sub : Datetime.t -> Datetime.t -> t
  val ymds : t -> int * int * int * int
end
