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

open Signatures
open Web_serializable_t

val spool_dir : string ref

module LwtRandom : RANDOM with type 'a t = 'a Lwt.t
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

val generate_token : unit -> string Lwt.t

val string_of_user : user -> string

val underscorize : string -> string

val send_email : string -> string -> string -> unit Lwt.t

val split_identity : string -> string * string

val available_languages : string list
val get_languages : string list option -> string list
val string_of_languages : string list option -> string
val languages_of_string : string -> string list option
