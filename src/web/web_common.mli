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
open Web_serializable_t

val spool_dir : string ref

val make_rng : unit -> Platform.rng Lwt.t
(** Create a pseudo random number generator initialized by a 128-bit
    secure random seed. *)

module type LWT_RANDOM = Signatures.RANDOM with type 'a t = 'a Lwt.t

module type LWT_RNG = sig
  val rng : Platform.rng Lwt.t
end

module MakeLwtRandom (X : LWT_RNG) : LWT_RANDOM
(** Lwt-compatible random number generation. *)

type error =
  | Serialization of exn
  | ProofCheck
  | ElectionClosed
  | MissingCredential
  | InvalidCredential
  | RevoteNotAllowed
  | ReusedCredential
  | WrongCredential
  | UsedCredential
  | CredentialNotFound
  | UnauthorizedVoter

exception Error of error

val fail : error -> 'a Lwt.t

val explain_error : error -> string

val open_security_log : string -> unit Lwt.t
(** Set the path to the security logger. *)

val security_log : (unit -> string) -> unit Lwt.t
(** Add an entry to the security log. *)

val fail_http : int -> 'a Lwt.t
val forbidden : unit -> 'a Lwt.t

val rewrite_prefix : string -> string
val set_rewrite_prefix : src:string -> dst:string -> unit

type election_file =
  | ESRaw
  | ESKeys
  | ESCreds
  | ESBallots
  | ESVoters
  | ESRecords
  | ESETally
  | ESResult

val election_file_of_string : string -> election_file
val string_of_election_file : election_file -> string

val election_file :
  string ->
  (election_file, [ `WithoutSuffix ],
   [ `One of election_file ] Eliom_parameter.param_name)
  Eliom_parameter.params_type

val uuid_of_string : string -> Uuidm.t

val uuid :
  string ->
  (Uuidm.t, [ `WithoutSuffix ],
   [ `One of Uuidm.t ] Eliom_parameter.param_name)
  Eliom_parameter.params_type

type setup_voter = {
  sv_id : string;
  mutable sv_password : (string * string) option;
}

type setup_trustee = {
  st_id : string;
  st_token : string;
  mutable st_public_key : string;
}

type setup_election = {
  mutable se_owner : user;
  mutable se_group : string;
  mutable se_voters : setup_voter list;
  mutable se_questions : template;
  mutable se_public_keys : setup_trustee list;
  mutable se_metadata : metadata;
  mutable se_public_creds : string;
  mutable se_public_creds_received : bool;
}

val generate_token : unit -> string Lwt.t

val string_of_user : user -> string

val underscorize : string -> string

val send_email : string -> string -> string -> unit Lwt.t

val split_identity : string -> string * string
