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

open Belenios_core.Signatures
open Belenios_core.Serializable_t
open Web_serializable_t

type 'a t
type 'a list

val get : uuid:uuid -> 'a t -> 'a option Lwt.t
val get_default : default:'a -> uuid:uuid -> 'a t -> 'a Lwt.t
val get_raw_list : uuid:uuid -> string list -> string List.t option Lwt.t
val get_fold_s : uuid:uuid -> 'a list -> ('a -> 'b -> 'b Lwt.t) -> 'b -> 'b option Lwt.t
val get_fold_s_default : uuid:uuid -> 'a list -> ('a -> 'b -> 'b Lwt.t) -> 'b -> 'b Lwt.t
val set : uuid:uuid -> 'a t -> 'a -> unit Lwt.t
val set_list : uuid:uuid -> 'a list -> 'a List.t -> unit Lwt.t
val del : uuid:uuid -> 'a t -> unit Lwt.t

val draft : draft_election t
val result : string t
val hide_result : datetime t
val dates : election_dates t
val state : election_state t
val partial_decryptions : partial_decryptions t
val decryption_tokens : decryption_tokens t
val election : string t
val metadata : metadata t
val private_key : number t
val private_keys : string list
val trustees : string t
val public_creds : public_credentials t
val ballots : string list
val ballots_index : Yojson.Safe.t t
val encrypted_tally : (module GROUP with type t = 'a) -> 'a encrypted_tally t
val shuffle_token : shuffle_token t
val shuffle_hashes : shuffle_hash list
val shuffles : string list
val extended_records : extended_record list
val records : string list
val credential_mappings : credential_mapping list
val audit_cache : audit_cache t
val voters : string list
