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

open Belenios

module type PARAMS = sig
  val file : string
end

module type GETTERS = sig
  val fsck : unit -> unit Lwt.t
  val setup_data : setup_data Lwt.t
  val raw_election : string Lwt.t
  val get_trustees : unit -> string option Lwt.t
  val get_public_creds : unit -> string list option Lwt.t
  val get_ballots : unit -> string list option Lwt.t

  val get_encrypted_tally :
    unit -> (string * hash sized_encrypted_tally) option Lwt.t

  val get_shuffles : unit -> (hash * hash owned * string) list option Lwt.t
  val get_pds : unit -> (hash * hash owned * string) list option Lwt.t
  val get_result : unit -> string option Lwt.t
end

module MakeGetters (_ : PARAMS) : GETTERS

module type ELECTION_DATA = sig
  type s
  type t
  type r

  module Cred :
    Credential.S
      with type private_key := s
       and type public_key := t
       and type 'a m := 'a

  val trustees_as_string : string option Lwt.t
  val trustees : (t, s) trustees option Lwt.t
  val pks : t array Lwt.t Lazy.t
  val raw_public_creds : string list option Lwt.t Lazy.t
  val public_creds_weights : (bool * weight SMap.t) option Lwt.t Lazy.t
  val raw_ballots : string list option Lwt.t Lazy.t
  val verified_ballots : (hash * string * weight * string) list Lwt.t Lazy.t
  val unverified_ballots : (hash * string * weight * string) list Lwt.t Lazy.t

  val pre_cast :
    ?skip_ballot_check:bool ->
    SSet.t ->
    string ->
    (hash * (string * weight * string), cast_error) result Lwt.t

  val raw_encrypted_tally :
    (t encrypted_tally * hash sized_encrypted_tally) Lwt.t Lazy.t

  val raw_shuffles : (hash * hash owned * string) list option Lwt.t Lazy.t
  val shuffles : (t, s) shuffle list option Lwt.t Lazy.t
  val shuffles_hash : string list option Lwt.t Lazy.t

  val encrypted_tally :
    (t encrypted_tally * hash sized_encrypted_tally) Lwt.t Lazy.t

  val pds : (hash * hash owned * string) list option Lwt.t Lazy.t
  val result : r election_result option Lwt.t Lazy.t
  val fsck : unit -> unit Lwt.t
  val election_hash : hash Lwt.t
end

module Make (_ : GETTERS) (Election : ELECTION) :
  ELECTION_DATA
    with type s := Election.G.Zq.t
     and type t := Election.G.t
     and type r := Election.result
