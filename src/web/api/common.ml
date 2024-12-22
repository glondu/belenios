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

type draft =
  | Draft : 'a Belenios.Election.version * 'a Serializable_t.raw_draft -> draft

let draft_of_string x =
  let abstract = Serializable_j.raw_draft_of_string Yojson.Safe.read_json x in
  let open Belenios.Election in
  match version_of_int abstract.draft_version with
  | Version v ->
      let open (val get_serializers v) in
      let x = Serializable_j.raw_draft_of_string read_question x in
      Draft (v, x)

let string_of_draft (Draft (v, x)) =
  let open (val Belenios.Election.get_serializers v) in
  Serializable_j.string_of_raw_draft write_question x
