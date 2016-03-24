(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Signatures
open Common

module type PARAMS = sig
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : unit -> string * string * string
end

module type PARSED_PARAMS = sig
  val uuid : Uuidm.t
  module G : GROUP
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module R = struct
    let uuid =
      match Uuidm.of_string P.uuid with
      | Some u -> u
      | None -> Printf.ksprintf failwith "%s is not a valid UUID" P.uuid
    module G = (val Group.of_string P.group : GROUP)
  end
  in (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct
  open P

  module CG = Credential.MakeGenerate (Election.MakeSimpleMonad (G))
  module CD = Credential.MakeDerive (G)

  let derive x =
    let x = CD.derive uuid x in
    let y = G.(g **~ x) in
    G.to_string y

  let compute_pub_and_hash priv =
    let pub = derive priv in
    let hashed = sha256_hex pub in
    priv, pub, hashed

  let generate () =
    CG.generate () () |> compute_pub_and_hash

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
