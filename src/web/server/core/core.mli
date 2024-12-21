(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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

open Belenios
open Serializable_t

exception Race_condition
exception Election_not_found of uuid * string

val ( let&* ) : 'a option -> ('a -> 'b option Lwt.t) -> 'b option Lwt.t
val ( let*& ) : 'a option Lwt.t -> ('a -> 'b option Lwt.t) -> 'b option Lwt.t
val sleep : float -> unit Lwt.t

module Datetime = Types.Datetime
module Period = Types.Period
module Random : RANDOM

val generate_numeric : ?length:int -> unit -> string
val generate_token : ?length:int -> unit -> string

type draft_election =
  | Draft :
      'a Belenios.Election.version * 'a raw_draft_election
      -> draft_election

val draft_election_of_string : string -> draft_election
val string_of_draft_election : draft_election -> string
val csv_of_string : string -> string list list
val string_of_csv : string list list -> string
