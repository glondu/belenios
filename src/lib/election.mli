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

val question_length : question -> int

module DirectRandom : sig

  (** {2 Monadic definitions} *)

  include Signatures.MONAD with type 'a t = 'a

  (** {2 Random number generation} *)

  val random : Z.t -> Z.t t
  (** [random q] returns a random number modulo [q]. It uses a secure
      random number generator lazily initialized by a 128-bit seed
      shared by all instances. *)
end
(** Direct random monad *)

module MakeElection (G : GROUP) (M : RANDOM) :
  ELECTION with type elt = G.t and type 'a m = 'a M.t
(** Implementation of {!Signatures.ELECTION}. *)
