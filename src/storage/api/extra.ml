(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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
open Types

type ('a, 'b) draft_election =
  | Draft :
      'q Belenios.Election.version * ('a, 'b) raw_draft_election
      -> ('a, 'b) draft_election

let draft_election_of_yojson of_yojson1 of_yojson2 x =
  let abstract = raw_draft_election_of_yojson Fun.id Fun.id x in
  let open Belenios.Election in
  match version_of_int abstract.version with
  | Version v ->
      let x = raw_draft_election_of_yojson of_yojson1 of_yojson2 x in
      Draft (v, x)

let yojson_of_draft_election to_yojson1 to_yojson2 (Draft (_, x)) =
  yojson_of_raw_draft_election to_yojson1 to_yojson2 x

type wrapped_draft_election =
  | W : ('a, 'b) group * ('a, 'b) draft_election -> wrapped_draft_election

let wrapped_draft_election_of_yojson (x : Json.t) : wrapped_draft_election =
  let abstract = raw_draft_election_of_yojson Fun.id Fun.id x in
  let version = abstract.version in
  let module G = (val Group.of_string ~version abstract.group) in
  let (Version v) = Election.version_of_int version in
  let x = [%group_of_yojson: _ raw_draft_election] x in
  W ((module G), Draft (v, x))

let yojson_of_wrapped_draft_election
    (W (w, Draft (_, x)) : wrapped_draft_election) : Json.t =
  let module G = (val w) in
  [%yojson_of_group: _ raw_draft_election] x

let csv_of_string = split_lines >> List.map (String.split_on_char ',')
let string_of_csv = List.map (String.concat ",") >> join_lines
