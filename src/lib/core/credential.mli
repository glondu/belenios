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

open Signatures
open Serializable_t
open Common

type batch = {
  private_creds : private_credentials;
  public_creds : public_credentials;
  public_with_ids_and_salts : string list;
}

module type ELECTION = sig
  type 'a t
  type public_key

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val pause : unit -> unit t
  val uuid : Uuid.t
  val get_salt : int -> public_key salt option t
end

module type S = sig
  type 'a m
  type private_key
  type public_key

  val generate : Voter.t list -> batch m
  val generate_sub : int -> sub_batch m * (unit -> int)
  val merge_sub : Voter.t list -> sub_batch -> batch

  val derive :
    string ->
    (private_key, [ `Wrong | `Invalid | `MaybePassword | `MissingSalt ]) result
    m

  val parse_public_credential : string -> (Weight.t * public_key) option
end

module Make (G : GROUP) (E : ELECTION with type public_key := G.t) :
  S
    with type public_key := G.t
     and type private_key := G.Zq.t
     and type 'a m := 'a E.t
