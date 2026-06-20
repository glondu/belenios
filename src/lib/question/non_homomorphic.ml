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
module Syntax = Question_nh

type t = Syntax.question
type Types.raw_question += Q of t

let extract = function Q x -> Some x | _ -> None
let type_ = "NonHomomorphic"
let make ~value ~extra = Types.{ type_; value = Q value; extra }

let wrap ~value ~extra =
  let value = Q (value |> Syntax.question_of_yojson) in
  Types.{ type_; value; extra }

let unwrap (q : Types.question) =
  match q.value with
  | Q x ->
      let value = x |> Syntax.yojson_of_question in
      let o = match q.extra with None -> [] | Some x -> [ ("extra", x) ] in
      Some
        (`Assoc (("type", `String "NonHomomorphic") :: ("value", value) :: o))
  | _ -> None

let erase (q : t) : t =
  { answers = Array.map (fun _ -> "") q.answers; question = "" }

type counting_method =
  [ `None
  | `MajorityJudgment of Question_nh.mj_extra
  | `Schulze of Question_nh.schulze_extra
  | `STV of Question_nh.stv_extra ]

let get_counting_method extra =
  let open Question_nh in
  match extra with
  | Some (`Assoc o as extra) -> (
      match List.assoc_opt "method" o with
      | Some (`String "MajorityJudgment") -> (
          match mj_extra_of_yojson extra with
          | x -> `MajorityJudgment x
          | exception _ -> `None)
      | Some (`String "Schulze") -> (
          match schulze_extra_of_yojson extra with
          | x -> `Schulze x
          | exception _ -> `None)
      | Some (`String "STV") -> (
          match stv_extra_of_yojson extra with
          | x -> `STV x
          | exception _ -> `None)
      | _ -> `None)
  | _ -> `None

let check group (q : t Types.generic_question) =
  let module G = (val Lazy.force group : GROUP) in
  let n = Array.length q.value.answers in
  if n > G.max_ints then Error `Vector_size
  else
    match get_counting_method q.extra with
    | `None -> Ok ()
    | `Schulze _ | `STV _ ->
        if n < 1 lsl G.bits_per_int then Ok () else Error `Int_size
    | `MajorityJudgment { grades = g; _ } ->
        if Array.length g < 1 lsl G.bits_per_int then Ok () else Error `Int_size
