(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

open Serializable_t
open Common
open Web_serializable_t

type election_state =
  [ `Open
  | `Closed
  | `EncryptedTally of int * int * string
  | `Tallied of plaintext
  | `Archived
  ]
val get_election_state : string -> election_state Lwt.t
val set_election_state : string -> election_state -> unit Lwt.t

val get_election_date : string -> datetime Lwt.t
val set_election_date : string -> datetime -> unit Lwt.t

val get_partial_decryptions : string -> (int * string) list Lwt.t
val set_partial_decryptions : string -> (int * string) list -> unit Lwt.t

val get_auth_config : string -> (string * (string * string list)) list Lwt.t
val set_auth_config : string -> (string * (string * string list)) list -> unit Lwt.t

val get_raw_election : string -> string option Lwt.t
val get_election_metadata : string -> metadata Lwt.t
val get_election_result : string -> Yojson.Safe.json result option Lwt.t

val get_elections_by_owner : user -> string list Lwt.t

val get_voters : string -> string list option Lwt.t
val get_passwords : string -> (string * string) SMap.t option Lwt.t

val get_ballot_hashes : uuid:string -> string list Lwt.t
val get_ballot_by_hash : uuid:string -> hash:string -> string option Lwt.t
