(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Belenios_platform
open Platform

(** Helpers for interacting with atd stuff *)

module Json = Atdgen_runtime.Util.Json

type 'a reader = 'a Json.reader
type 'a writer = 'a Json.writer

(** A group suitable for discrete logarithm-based cryptography. *)
module type GROUP = sig
  (** The following interface is redundant: it is assumed, but not
      checked, that usual mathematical relations hold. *)

  type t
  (** The type of elements. Note that it may be larger than the group
      itself, hence the [check] function below. *)

  val check : t -> bool
  (** Check group membership. *)

  val one : t
  (** The neutral element of the group. *)

  val g : t
  (** A generator of the group. *)

  val q : Z.t
  (** The order of [g]. *)

  val ( *~ ) : t -> t -> t
  (** Multiplication. *)

  val ( **~ ) : t -> Z.t -> t
  (** Exponentiation. *)

  val ( =~ ) : t -> t -> bool
  (** Equality test. *)

  val invert : t -> t
  (** Inversion. *)

  val to_string : t -> string
  (** Conversion to string. *)

  val of_string : string -> t
  (** Conversion from string. *)

  val of_ints : int array -> t
  (** Convert an int array to a group element. *)

  val to_ints : int -> t -> int array
  (** Convert a group element to an int array. The first argument is
     the size of the array. *)

  val hash : string -> t array -> Z.t
  (** Hash an array of elements into an integer mod [q]. The string
      argument is a string that is prepended before computing the hash. *)

  val hash_to_int : t -> int
  (** Hash an element to a small integer. *)

  val compare : t -> t -> int
  (** A total ordering over the elements of the group. *)

  val get_generator : int -> t
                               (** [get_generator i] computes generator #[i] of the group. *)

  val description : string
end

(** Monad signature. *)
module type MONAD = sig
  type 'a t
  val yield : unit -> unit t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
end

(** Random number generation. *)
module type RANDOM = sig
  val random : Z.t -> Z.t
                          (** [random q] returns a random number modulo [q]. *)
end

module type QUESTION_SIGNATURE_PACK = sig
  type t
  val x : t Common_types.Question_signature.t
end
