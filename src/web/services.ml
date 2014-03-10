(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Util
open Serializable_t
open Eliom_service
open Eliom_parameter

let ballot = Eliom_reference.eref
  ~scope:Eliom_common.default_session_scope
  (None : string option)

let uuid = Eliom_parameter.user_type
  (fun x -> match Uuidm.of_string x with
    | Some x -> x
    | None -> invalid_arg "uuid")
  Uuidm.to_string
  "uuid"

type election_file =
  | ESIndex
  | ESRaw
  | ESKeys
  | ESCreds
  | ESBallots
  | ESRecords

let election_file_of_string = function
  | "" -> ESIndex
  | "election.json" -> ESRaw
  | "public_keys.jsons" -> ESKeys
  | "public_creds.txt" -> ESCreds
  | "ballots.jsons" -> ESBallots
  | "records" -> ESRecords
  | x -> invalid_arg ("election_dir_item: " ^ x)

let string_of_election_file = function
  | ESIndex -> ""
  | ESRaw -> "election.json"
  | ESKeys -> "public_keys.jsons"
  | ESCreds -> "public_creds.txt"
  | ESBallots -> "ballots.jsons"
  | ESRecords -> "records"

let election_update_credential_form = service
  ~path:["election"; "update-cred"]
  ~get_params:uuid
  ()

let get_randomness = service
  ~path:["get-randomness"]
  ~get_params:unit
  ()

let preapply_uuid s e = Eliom_service.preapply s e.e_uuid

type savable_service =
  | Home
  | Cast of Uuidm.t
  | Election of Uuidm.t

let saved_service = Eliom_reference.eref
  ~scope:Eliom_common.default_session_scope
  Home
