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
open Common

module type PARAMS = sig
  val version : int
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : Voter.t list -> Credential.batch
end

module Make (P : PARAMS) (R : RANDOM) () = struct
  module G = (val Belenios.Group.of_string ~version:P.version P.group : GROUP)

  module Cred =
    Credential.Make (R) (G)
      (struct
        let uuid = Uuid.wrap P.uuid
      end)

  let derive_in_group x =
    match Cred.derive x with
    | Ok x -> G.(g **~ x)
    | Error _ -> Printf.ksprintf failwith "invalid secret credential: %s" x

  let derive x = G.to_string (derive_in_group x)
  let generate = Cred.generate
end

let int_length n = string_of_int n |> String.length

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
      let x : Voter.t = (`Plain, { address; login = None; weight = None }) in
      loop (last - 1) (x :: accu)
  in
  loop last []
