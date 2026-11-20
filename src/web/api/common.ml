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

open Types

type draft = Draft : 'a Belenios.Election.version * raw_draft -> draft

let draft_d =
  Desc.Core.map raw_draft_d
    (fun x ->
      match Belenios.Election.version_of_int x.version with
      | Version v -> Draft (v, x))
    (fun (Draft (_, x)) -> x)

let draft_of_yojson = Desc.Json.of_json draft_d
let yojson_of_draft = Desc.Json.to_json draft_d

let remaining_credits (xs : credentials_credits) =
  xs
  |> List.filter (fun x -> x.success)
  |> List.fold_left (fun accu x -> accu + x.credits) 0
