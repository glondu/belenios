open Serializable_builtin_t
open Serializable_t

type user_type = Dummy | CAS | Admin

type user = {
  user_name : string;
  user_type : user_type;
}

val string_of_user : user -> string
val is_admin : user option -> bool

type acl =
  | Any
  | Restricted of (user -> bool Lwt.t)

module SSet : Set.S with type elt = string

type election_web = {
  params_fname : string;
  public_keys_fname : string;
  featured_p : bool;
  can_read : acl;
  can_vote : acl;
}

val make_rng : unit -> Cryptokit.Random.rng Lwt.t
(** Create a pseudo random number generator initialized by a 128-bit
    secure random seed. *)

module type LWT_RANDOM = Signatures.RANDOM with type 'a t = 'a Lwt.t

module type LWT_RNG = sig
  val rng : Cryptokit.Random.rng Lwt.t
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

exception Error of error

val explain_error : error -> string

module type WEB_BALLOT_BOX = sig
  include Signatures.BALLOT_BOX
  with type 'a m = 'a Lwt.t
  and type ballot = string
  and type record = string * datetime

  val inject_creds : SSet.t -> unit Lwt.t
  val extract_creds : unit -> SSet.t Lwt.t
  val update_cred : old:string -> new_:string -> unit Lwt.t
end

module type WEB_ELECTION_BUNDLE =
  Signatures.ELECTION_BUNDLE with type 'a E.m = 'a Lwt.t

module type WEB_BALLOT_BOX_BUNDLE = sig
  include WEB_ELECTION_BUNDLE
  module B : WEB_BALLOT_BOX
end

type 'a web_election = private {
  modules : (module WEB_BALLOT_BOX_BUNDLE with type elt = 'a);
  election : 'a Signatures.election;
  election_web : election_web;
}

val make_web_election :
  string ->
  Serializable_t.metadata option ->
  election_web ->
  Z.t web_election

val open_security_log : string -> unit Lwt.t
(** Set the path to the security logger. *)

val security_log : (unit -> string) -> unit Lwt.t
(** Add an entry to the security log. *)
