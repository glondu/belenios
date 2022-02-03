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

open Belenios_core
open Belenios
open Serializable_builtin_t
open Serializable_j
open Signatures

module type PARAMS = sig
  val version : int
  val uuid : string
  val group : string
  val template : string
  val get_trustees : unit -> string
end

module type S = sig
  val mkelection : unit -> string
end

module type PARSED_PARAMS = sig
  val version : int
  val uuid : uuid
  val template : template
  val group : string
  module G : GROUP
  val get_trustees : unit -> G.t trustees
  module Trustees : Trustees_sig.S
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module T = (val Trustees.get_by_version P.version) in
  let module R = struct
    let version = P.version
    let uuid = uuid_of_raw_string P.uuid
    let template = template_of_string P.template
    let group = P.group
    module G = (val Group.of_string ~version P.group : GROUP)
    let get_trustees () =
      P.get_trustees () |> trustees_of_string G.read
    module Trustees = T
  end
  in (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct
  open P

  (* Setup trustees *)

  module K = Trustees.MakeCombinator (G)

  let trustees = get_trustees ()
  let y = K.combine_keys trustees
  let public_key = G.to_string y

  (* Setup election *)

  let params = {
    e_version = version;
    e_description = template.t_description;
    e_name = template.t_name;
    e_questions = template.t_questions;
    e_uuid = uuid;
    e_administrator = template.t_administrator;
    e_credential_authority = template.t_credential_authority;
  }

  (* Generate and serialize election.json *)

  let mkelection () =
    Election.make_raw_election params ~group ~public_key

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)
