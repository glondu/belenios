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

(** Election primitives *)

open Platform
open Serializable_t
open Signatures

module MakeSimpleMonad (G : GROUP) : sig

  (** {2 Monadic definitions} *)

  include Signatures.MONAD with type 'a t = unit -> 'a

  (** {2 Random number generation} *)

  val random : Z.t -> Z.t t
  (** [random q] returns a random number modulo [q]. It uses a secure
      random number generator lazily initialized by a 128-bit seed
      shared by all instances. *)

  (** {2 Ballot box management} *)

  include Signatures.MONADIC_MAP_RO
  with type 'a m := 'a t
  and type elt = G.t ballot
  and type key := unit

  val cast : elt -> unit t
end
(** Simple election monad that keeps all ballots in memory. *)

module MakeSimpleDistKeyGen (G : GROUP) (M : RANDOM) : sig

  (** This module implements a simple distributed key generation. Each
      share is a number modulo q, and the secret key is their sum. All
      shares are needed to decrypt, but the decryptions can be done in
      a distributed fashion. *)

  val generate_and_prove :
    unit -> (Z.t * G.t trustee_public_key) M.t
  (** [generate_and_prove ()] returns a new keypair [(x, y)]. [x] is
      the secret exponent, [y] contains the public key and a
      zero-knowledge proof of knowledge of [x]. *)

  val check : G.t trustee_public_key -> bool
  (** Check a public key and its proof. *)

  val combine : G.t trustee_public_key array -> G.t
  (** Combine all public key shares into an election public key. *)

end
(** Simple distributed generation of an election public key. *)

module MakeElection (G : GROUP) (M : RANDOM) :
  ELECTION with type elt = G.t and type 'a m = 'a M.t
(** Implementation of {!Signatures.ELECTION}. *)
