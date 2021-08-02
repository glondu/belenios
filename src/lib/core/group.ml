(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Serializable_j
open Signatures

(** Generic group parsing *)

(* For now, only finite fields are supported... *)

let of_string x =
  let group = ff_params_of_string x in
  let module G = (val Group_field.make group : Group_field.GROUP) in
  (module G : GROUP)

let read state buf =
  let group = read_ff_params state buf in
  let module G = (val Group_field.make group : Group_field.GROUP) in
  (module G : GROUP)

let wrapped_pubkey_of_string x =
  let x = wrapped_pubkey_of_string read Yojson.Safe.read_json x in
  let {wpk_group=group; wpk_y=y} = x in
  let module X =
    struct
      module G = (val group)
      let y = G.of_string (Yojson.Safe.Util.to_string y)
    end
  in
  (module X : WRAPPED_PUBKEY)
