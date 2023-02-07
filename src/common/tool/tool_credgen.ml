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

open Belenios_core
open Signatures
open Serializable_t
open Common

module type PARAMS = sig
  val version : int
  val uuid : string
  val group : string
end

type credentials =
  {
    priv : private_credentials;
    public : string list;
    public_with_ids : string list;
  }

module type S = sig
  val derive : string -> string
  val generate : Voter.t list -> credentials
end

module Make (P : PARAMS) (M : RANDOM) () = struct

  let uuid = Uuid.wrap P.uuid
  module G = (val Belenios.Group.of_string ~version:P.version P.group : GROUP)

  module CG = Credential.MakeGenerate (M)
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
    let implicit_weights = not (has_explicit_weights ids) in
    let privs, pubs =
      List.fold_left
        (fun (privs, pubs) id ->
          let _, username, weight = Voter.get id in
          let priv = CG.generate () in
          (
            (username, priv) :: privs,
            CredSet.add (derive_in_group priv) (weight, username) pubs
          )
        ) ([], CredSet.empty) ids
    in
    let serialize (e, (w, id)) =
      G.to_string e
      ^ (if implicit_weights then "," else Printf.sprintf ",%s" (Weight.to_string w))
      ^ Printf.sprintf ",%s" id
    in
    let serialize_public (e, (w, _)) =
      G.to_string e
      ^ (if implicit_weights then "" else Printf.sprintf ",%s" (Weight.to_string w))
    in
    let bindings = CredSet.bindings pubs in
    {
      priv = List.rev privs;
      public = List.map serialize_public bindings;
      public_with_ids = List.map serialize bindings;
    }

end

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
    else
      let address = string_of_int last in
      let x : Voter.t = `Plain, {address; login = None; weight = None} in
      loop (last-1) (x :: accu)
  in loop last []
