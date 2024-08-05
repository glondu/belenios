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

open Serializable_j

let version =
  let open Belenios.Election in
  let (Version v) = List.hd supported_crypto_versions in
  int_of_version v

let name = ""
let description = ""
let contact = ""

let questions =
  [|
    Belenios_question.Homomorphic.make
      ~value:
        {
          q_answers = [| "Answer 1"; "Answer 2"; "Answer 3" |];
          q_blank = None;
          q_min = 1;
          q_max = 1;
          q_question = "Question 1?";
        }
      ~extra:None;
  |]

let creation_date = datetime_of_string "\"2018-11-26 00:00:00.000000\""
let validation_date = datetime_of_string "\"2015-10-01 00:00:00.000000\""
let tally_date = datetime_of_string "\"2018-11-26 00:00:00.000000\""
let archive_date = datetime_of_string "\"2018-11-26 00:00:00.000000\""
let days_to_archive = 7
let days_to_delete = 365
let days_to_mail = 30
let days_between_mails = 7
let max_election_name_size = 80
let max_total_weight = 100_000
let supported_booth_versions = [ 2 ]
