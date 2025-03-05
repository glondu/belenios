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

module Syntax = Question_h_j

type t = Syntax.question
type Types.raw_question += Q of t

let extract = function Q x -> Some x | _ -> None
let type_ = "Homomorphic"
let make ~value ~extra = Types.{ type_; value = Q value; extra }

let wrap ~value ~extra =
  let value = Q (value |> Yojson.Safe.to_string |> Syntax.question_of_string) in
  Types.{ type_; value; extra }

let unwrap (q : Types.question) =
  match q.value with
  | Q x -> (
      let value = x |> Syntax.string_of_question |> Yojson.Safe.from_string in
      match q.extra with
      | None -> Some value
      | Some x ->
          let o = [ ("extra", x) ] in
          Some
            (`Assoc (("type", `String "Homomorphic") :: ("value", value) :: o)))
  | _ -> None

let erase (q : t) : t =
  {
    q_answers = Array.map (fun _ -> "") q.q_answers;
    q_blank = q.q_blank;
    q_min = q.q_min;
    q_max = q.q_max;
    q_question = "";
  }

let check _ (q : t Types.generic_question) =
  let q = q.value in
  if q.q_min <= q.q_max then Ok () else Error `Min_max
