(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2017 Inria                                           *)
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

open Platform
open Serializable_t
open Signatures

module MakeSimpleDistKeyGen (G : GROUP) (M : RANDOM) : sig

  (** This module implements a simple distributed key generation. Each
      share is a number modulo q, and the secret key is their sum. All
      shares are needed to decrypt, but the decryptions can be done in
      a distributed fashion. *)

  val generate : unit -> Z.t M.t
  (** [generate ()] generates a new private key. *)

  val prove : Z.t -> G.t trustee_public_key M.t
  (** [prove x] returns the public key associated to [x] and a zero-
      knowledge proof of its knowledge. *)

  val check : G.t trustee_public_key -> bool
  (** Check a public key and its proof. *)

  val combine : G.t trustee_public_key array -> G.t
  (** Combine all public key shares into an election public key. *)

end
(** Simple distributed generation of an election public key. *)
