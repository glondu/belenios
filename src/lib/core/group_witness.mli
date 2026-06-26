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

type 'a serializers = { of_string : string -> 'a; to_string : 'a -> string }

module type SERIALIZERS = sig
  type element
  type scalar

  val element : element serializers
  val scalar : scalar serializers
end

type (!'a, !'b) t

val make :
  (module SERIALIZERS with type element = 'a and type scalar = 'b) -> ('a, 'b) t

val get :
  ('a, 'b) t -> (module SERIALIZERS with type element = 'a and type scalar = 'b)

val provably_equal_opt :
  ('a1, 'b1) t -> ('a2, 'b2) t -> ('a1 * 'b1, 'a2 * 'b2) Type.eq option

exception Witness_mismatch of string

val provably_equal :
  string -> ('a1, 'b1) t -> ('a2, 'b2) t -> ('a1 * 'b1, 'a2 * 'b2) Type.eq
