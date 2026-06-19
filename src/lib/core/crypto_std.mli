(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026 VCAST                                                *)
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

val strxor : string -> string -> string

val i2osp : int -> int -> string
(** I2OSP from RFC8017 *)

val os2ip : string -> Z.t
(** OS2IP from RFC8017 *)

module type HASH_FUNCTION = sig
  val h : string -> string
  val b_in_bytes : int
  val s_in_bytes : int
end

module SHA256 : HASH_FUNCTION

module Expand_message (_ : HASH_FUNCTION) : sig
  val expand_message_xmd : dst:string -> string -> int -> string
  (** expand_message_xmd from RFC9380 *)
end

module type HASH_TO_FIELD_PARAMS = sig
  val k : int
  val p : Z.t
  val m : int
  val expand_message : dst:string -> string -> int -> string
end

module Hash_to_field (_ : HASH_TO_FIELD_PARAMS) : sig
  val hash_to_field : dst:string -> string -> int -> Z.t array array
  (** hash_to_field from RFC9380 *)
end

module type PSEUDO_RANDOM_FUNCTION = sig
  val hLen : int
  val h : key:string -> string -> string
end

module HMAC_SHA1 : PSEUDO_RANDOM_FUNCTION
module HMAC_SHA256 : PSEUDO_RANDOM_FUNCTION

module Pbkdf2 (_ : PSEUDO_RANDOM_FUNCTION) : sig
  val pbkdf2 : iterations:int -> salt:string -> string -> int -> string
  (** PBKDF2 from RFC8018 *)
end
