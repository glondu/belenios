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

open Serializable_t
open Signatures

module type S = sig
  (** Simple distributed generation of an election public key. *)
  module MakeSimple (G : GROUP) (_ : RANDOM) : sig
    (** This module implements a simple distributed key generation. Each share
        is a number modulo q, and the secret key is their sum. All shares are
        needed to decrypt, but the decryptions can be done in a distributed
        fashion. *)

    val generate : unit -> G.Zq.t
    (** [generate ()] generates a new private key. *)

    val prove : G.Zq.t -> (G.t, G.Zq.t) trustee_public_key
    (** [prove x] returns the public key associated to [x] and a zero- knowledge
        proof of its knowledge. *)
  end

  module MakePKI (G : GROUP) (R : RANDOM) :
    PKI with module Group = G and module Random = R

  module MakeChannels (P : PKI) : CHANNELS with module Pki = P

  exception PedersenFailure of string

  module MakePedersen (C : CHANNELS) :
    PEDERSEN
      with type element = C.Pki.Group.t
       and type scalar = C.Pki.Group.Zq.t

  module MakeCombinator (G : GROUP) : sig
    val check : (G.t, G.Zq.t) trustees -> bool
    (** Check consistency of a set of trustees. *)

    val combine_keys : (G.t, G.Zq.t) trustees -> G.t
    (** Compute the public key associated to a set of trustees. *)

    val combine_factors :
      (G.t, G.Zq.t) trustees ->
      (G.t -> (G.t, G.Zq.t) partial_decryption -> bool) ->
      (G.t, G.Zq.t) partial_decryption owned list ->
      (G.t shape, combination_error) result
    (** Compute synthetic decryption factors. *)
  end
end
