(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

module MakeSimple (G : GROUP) (M : RANDOM) : sig

  (** This module implements a simple distributed key generation. Each
      share is a number modulo q, and the secret key is their sum. All
      shares are needed to decrypt, but the decryptions can be done in
      a distributed fashion. *)

  val generate : unit -> Z.t M.t
  (** [generate ()] generates a new private key. *)

  val prove : Z.t -> G.t trustee_public_key M.t
  (** [prove x] returns the public key associated to [x] and a zero-
      knowledge proof of its knowledge. *)

end
(** Simple distributed generation of an election public key. *)

module MakePKI (G : GROUP) (M : RANDOM) :
  PKI with type 'a m = 'a M.t
     and type private_key = Z.t
     and type public_key = G.t

module MakeChannels (G : GROUP) (M : RANDOM)
         (P : PKI with type 'a m = 'a M.t
                   and type private_key = Z.t
                   and type public_key = G.t) :
  CHANNELS with type 'a m = 'a P.m
     and type private_key = P.private_key
     and type public_key = P.public_key

exception PedersenFailure of string

module MakePedersen (G : GROUP) (M : RANDOM)
         (P : PKI with type 'a m = 'a M.t
                   and type private_key = Z.t
                   and type public_key = G.t)
         (C : CHANNELS with type 'a m = 'a M.t
                        and type private_key = Z.t
                        and type public_key = G.t) :
  PEDERSEN with type 'a m = 'a M.t
            and type elt = G.t

module MakeCombinator (G : GROUP) : sig

  val check : G.t trustees -> bool
  (** Check consistency of a set of trustees. *)

  val combine_keys : G.t trustees -> G.t
  (** Compute the public key associated to a set of trustees. *)

  val combine_factors :
    G.t trustees ->
    (G.t -> G.t partial_decryption -> bool) ->
    G.t partial_decryption list -> G.t shape
  (** Compute synthetic decryption factors. *)

end
