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
open Common

val parse : string -> [ `Valid | `Invalid | `MaybePassword ]
val check : string -> bool

type 'a t = { private_cred : string; private_key : 'a }

module type ELECTION = sig
  val uuid : Uuid.t
end

module type S = sig
  type private_key
  type public_key

  val generate : unit -> private_key t
  val derive : string -> private_key
  val parse_public_credential : string -> (Weight.t * public_key) option
end

module Make (R : RANDOM) (G : GROUP) (E : ELECTION) :
  S with type public_key := G.t and type private_key := G.Zq.t
