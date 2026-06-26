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

type (_, _) id = ..

module type ID = sig
  type element
  type scalar
  type (_, _) id += Id : (element, scalar) id
end

type (!'a, !'b) t = {
  m : (module ID with type element = 'a and type scalar = 'b);
  serializers : (module SERIALIZERS with type element = 'a and type scalar = 'b);
}

let make (type a b) serializers : (a, b) t =
  let module M : ID with type element = a and type scalar = b = struct
    type element = a
    type scalar = b
    type (_, _) id += Id : (element, scalar) id
  end
  in
  { m = (module M); serializers }

let get t = t.serializers

let provably_equal_opt (type a1 b1) (t1 : (a1, b1) t) (type a2 b2)
    (t2 : (a2, b2) t) : (a1 * b1, a2 * b2) Type.eq option =
  let module T1 = (val t1.m) in
  let module T2 = (val t2.m) in
  match T1.Id with T2.Id -> Some Equal | _ -> None

exception Witness_mismatch of string

let provably_equal msg a b =
  match provably_equal_opt a b with
  | Some x -> x
  | None -> raise (Witness_mismatch msg)
