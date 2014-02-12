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

let home = service
  ~path:[]
  ~get_params:unit
  ()

let source_code = service
  ~path:["belenios.tar.gz"]
  ~get_params:unit
  ()

let logout = service
  ~path:["logout"]
  ~get_params:unit
  ()

let create_string_login ~fallback ~post_params =
  Eliom_service.post_coservice
    ~csrf_safe:true
    ~csrf_scope:Eliom_common.default_session_scope
    ~fallback ~post_params ()

let user = Eliom_reference.eref
  ~scope:Eliom_common.default_session_scope
  (None : Web_common.user option)

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
  | ESRaw
  | ESKeys
  | ESCreds
  | ESBallots
  | ESRecords

let election_file_of_string = function
  | "election.json" -> ESRaw
  | "public_keys.jsons" -> ESKeys
  | "public_creds.txt" -> ESCreds
  | "ballots.jsons" -> ESBallots
  | "records" -> ESRecords
  | x -> invalid_arg ("election_dir_item: " ^ x)

let string_of_election_file = function
  | ESRaw -> "election.json"
  | ESKeys -> "public_keys.jsons"
  | ESCreds -> "public_creds.txt"
  | ESBallots -> "ballots.jsons"
  | ESRecords -> "records"

let election_file = Eliom_parameter.user_type
  election_file_of_string
  string_of_election_file
  "file"

let election_dir = service
  ~path:["elections"]
  ~get_params:(suffix (uuid ** election_file))
  ()

let election_index = service
  ~path:["election"; ""]
  ~get_params:uuid
  ()

let election_vote = service
  ~path:["election"; "vote"]
  ~get_params:uuid
  ()

let election_cast = service
  ~path:["election"; "cast"]
  ~get_params:uuid
  ()

let election_cast_post = post_service
  ~fallback:election_cast
  ~post_params:(opt (string "encrypted_vote") ** opt (file "encrypted_vote_file"))
  ()

let election_update_credential_form = service
  ~path:["election"; "update-cred"]
  ~get_params:uuid
  ()

let election_update_credential = post_service
  ~fallback:election_update_credential_form
  ~post_params:(string "old_credential" ** string "new_credential")
  ()

let create_confirm () =
  Eliom_service.post_coservice
    ~csrf_safe:true
    ~csrf_scope:Eliom_common.default_session_scope
    ~fallback:election_cast
    ~post_params:Eliom_parameter.unit
    ()

let get_randomness = service
  ~path:["get-randomness"]
  ~get_params:unit
  ()

let election_booth = static_dir_with_params
  ~get_params:(string "election_url")
  ()

let make_booth uuid =
  let service = Eliom_service.preapply election_dir (uuid, ESRaw) in
  Eliom_service.preapply election_booth (
    ["booth"; "vote.html"],
    Eliom_uri.make_string_uri ~absolute_path:true ~service ()
  )

let preapply_uuid s e = Eliom_service.preapply s e.e_uuid
let election_file e f = Eliom_service.preapply election_dir (e.e_uuid, f)

type savable_service =
  | Home
  | Cast of Uuidm.t
  | Election of Uuidm.t

let saved_service = Eliom_reference.eref
  ~scope:Eliom_common.default_session_scope
  Home

let to_service = function
  | Home -> home
  | Cast u -> Eliom_service.preapply election_cast u
  | Election u -> Eliom_service.preapply election_index u

open Lwt

let get () =
  Eliom_reference.get saved_service >>= wrap1 to_service

let set s =
  Eliom_reference.set saved_service s
