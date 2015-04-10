(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2015 Inria                                           *)
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

type election_state =
  [ `Open
  | `Closed
  | `EncryptedTally of int * int * string
  ]
val get_election_state : string -> election_state Lwt.t
val set_election_state : string -> election_state -> unit Lwt.t

val get_main_election : unit -> string option Lwt.t
val set_main_election : string -> unit Lwt.t
val unset_main_election : unit -> unit Lwt.t

val add_featured_election : string -> unit Lwt.t
val remove_featured_election : string -> unit Lwt.t
val is_featured_election : string -> bool Lwt.t
val get_featured_elections : unit -> string list Lwt.t

val get_partial_decryptions : string -> (int * string) list Lwt.t
val set_partial_decryptions : string -> (int * string) list -> unit Lwt.t
