(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2013 Inria                                           *)
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

(** Compatibility layer with the Helios reference implementation *)

open Serializable_compat_t

val params : 'a params -> 'a Serializable_t.params
val ballot : 'a ballot -> 'a Serializable_t.ballot
val partial_decryption :
  'a partial_decryption -> 'a Serializable_t.partial_decryption

module MakeCompat (G : Signatures.GROUP) : sig
  type election = G.t Signatures.election
  val ballot : election -> G.t Serializable_t.ballot -> G.t ballot
  val partial_decryption : election ->
    G.t Serializable_t.ciphertext array array ->
    G.t Serializable_t.partial_decryption -> G.t partial_decryption
end
