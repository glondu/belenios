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
  fingerprint : string;
  params : ff_pubkey params;
  public_keys_fname : string;
  public_creds : SSet.t;
  featured_p : bool;
  can_read : acl;
  can_vote : acl;
}

module MakeLwtRandom (G : Signatures.GROUP) : sig

  (** {2 Monadic definitions} *)

  include Signatures.MONAD with type 'a t = 'a Lwt.t

  (** {2 Random number generation} *)

  val random : Z.t -> Z.t t
  (** [random q] returns a random number modulo [q]. It uses a secure
      random number generator initialized by a 128-bit seed. *)
end
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

module type LWT_ELECTION = Signatures.ELECTION
  with type elt = Z.t
  and type 'a m = 'a Lwt.t

module type WEB_BBOX = sig
  include Signatures.BALLOT_BOX
  with type 'a m := 'a Lwt.t
  and type ballot = string
  and type record = string * datetime

  val inject_creds : SSet.t -> unit Lwt.t
  val extract_creds : unit -> SSet.t Lwt.t
  val update_cred : old:string -> new_:string -> unit Lwt.t
end

module MakeBallotBox (P : Signatures.ELECTION_PARAMS) (E : LWT_ELECTION) : WEB_BBOX

module type WEB_ELECTION = sig
  module G : Signatures.GROUP
  module P : Signatures.ELECTION_PARAMS
  module E : LWT_ELECTION
  module B : WEB_BBOX
  val election_web : election_web
end

val open_security_log : string -> unit Lwt.t
(** Set the path to the security logger. *)

val security_log : (unit -> string) -> unit Lwt.t
(** Add an entry to the security log. *)
