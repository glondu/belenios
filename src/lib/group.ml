(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Platform
open Serializable_j
open Signatures
open Common

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

let election_params_of_string x =
  let params = params_of_string read_ff_pubkey x in
  let {ffpk_g=g; ffpk_p=p; ffpk_q=q; ffpk_y=y} = params.e_public_key in
  let group = {g; p; q} in
  let module X = struct
    module G = (val Group_field.make group : Group_field.GROUP)
    let params = {params with e_public_key = y}
    let fingerprint = sha256_b64 x
  end in
  (module X : ELECTION_PARAMS)
