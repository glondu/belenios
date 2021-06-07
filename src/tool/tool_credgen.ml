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

open Belenios
open Serializable_builtin_t
open Signatures
open Common

module type PARAMS = sig
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : string list -> string list * string list
end

module type PARSED_PARAMS = sig
  val uuid : uuid
  module G : GROUP
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module R = struct
    let uuid = uuid_of_raw_string P.uuid
    module G = (val Group.of_string P.group : GROUP)
  end
  in (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct
  open P

  module CG = Credential.MakeGenerate (DirectRandom)
  module CD = Credential.MakeDerive (G)

  module CredSet = Map.Make (G)

  let derive_in_group x =
    if Credential.check x then (
      let x = CD.derive uuid x in
      G.(g **~ x)
    ) else (
      Printf.ksprintf failwith "invalid secret credential: %s" x
    )

  let derive x =
    G.to_string (derive_in_group x)

  let generate ids =
    let implicit_weights = ref true in
    let privs, pubs =
      List.fold_left
        (fun (privs, pubs) id ->
          let _, _, weight = split_identity id in
          if weight <> 1 then implicit_weights := false;
          let priv = CG.generate () in
          priv :: privs,
          CredSet.add (derive_in_group priv) weight pubs
        ) ([], CredSet.empty) ids
    in
    let serialize (e, w) =
      G.to_string e
      ^ (if !implicit_weights then "" else Printf.sprintf ",%d" w)
    in
    List.rev privs, (CredSet.bindings pubs |> List.map serialize)

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)

let int_length n =
  string_of_int n |> String.length

let rec find_first n first =
  if int_length first = int_length (first + n) then first
  else find_first n (10 * first)

let generate_ids n =
  (* choose the first id so that they all have the same length *)
  let first = find_first n 1 in
  let last = first + n - 1 in
  let rec loop last accu =
    if last < first then accu
    else loop (last-1) (string_of_int last :: accu)
  in loop last []
