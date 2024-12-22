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
open Serializable_j

type draft_election =
  | Draft :
      'a Belenios.Election.version * 'a raw_draft_election
      -> draft_election

let draft_election_of_string x =
  let abstract = raw_draft_election_of_string Yojson.Safe.read_json x in
  let open Belenios.Election in
  match version_of_int abstract.se_version with
  | Version v ->
      let open (val get_serializers v) in
      let x = raw_draft_election_of_string read_question x in
      Draft (v, x)

let string_of_draft_election (Draft (v, x)) =
  let open (val Belenios.Election.get_serializers v) in
  string_of_raw_draft_election write_question x

let csv_of_string = Csv.(of_string >> input_all)

let string_of_csv csv =
  let b = Buffer.create 1024 in
  Csv.(output_all (to_buffer b) csv);
  Buffer.contents b
