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

open Eliom_service
open Eliom_service.Http
open Eliom_parameter
open Web_common
open Web_signatures

let home = service ~path:[""] ~get_params:unit ()
let admin = service ~path:["admin"] ~get_params:unit ()
let site_login = service ~path:["login"] ~get_params:(opt (string "service")) ()
let site_logout = service ~path:["logout"] ~get_params:unit ()

let source_code = service ~path:["belenios.tar.gz"] ~get_params:unit ()
let get_randomness = service ~path:["get-randomness"] ~get_params:unit ()

let new_election = service ~path:["new-election"] ~get_params:unit ()
let new_election_post = post_service ~fallback:new_election ~post_params:(file "election" ** file "metadata" ** file "public_keys" ** file "public_creds") ()

let tool = preapply (static_dir ()) ["static"; "belenios-tool.html"]

let election_setup_index = service ~path:["setup"; ""] ~get_params:unit ()
let election_setup_new = post_coservice ~csrf_safe:true ~fallback:election_setup_index ~post_params:unit ()
let election_setup = service ~path:["setup"; "election"] ~get_params:(uuid "uuid") ()
let election_setup_group = post_coservice ~fallback:election_setup ~post_params:(string "group") ()
let election_setup_metadata = post_coservice ~fallback:election_setup ~post_params:(string "metadata") ()
let election_setup_questions = service ~path:["setup"; "questions"] ~get_params:(uuid "uuid") ()
let election_setup_questions_post = post_coservice ~fallback:election_setup_questions ~post_params:(string "questions") ()
let election_setup_trustee_add = post_coservice ~fallback:election_setup ~post_params:unit ()
let election_setup_credentials = service ~path:["setup"; "credentials"] ~get_params:(string "token") ()
let election_setup_credentials_download = service ~path:["setup"; "public_creds.txt"] ~get_params:(string "token") ()
let election_setup_credentials_post = post_coservice ~fallback:election_setup_credentials ~post_params:(string "public_creds") ()
let election_setup_credentials_post_file = post_coservice ~fallback:election_setup_credentials ~post_params:(file "public_creds") ()
let election_setup_trustee = service ~path:["setup"; "trustee"] ~get_params:(string "token") ()
let election_setup_trustee_post = post_coservice ~fallback:election_setup_trustee ~post_params:(string "public_key") ()
let election_setup_create = post_coservice ~csrf_safe:true ~fallback:election_setup ~post_params:unit ()

let election_home = service ~path:["elections"] ~get_params:(suffix (uuid "uuid" ** suffix_const "")) ()
let election_admin = service ~path:["elections"] ~get_params:(suffix (uuid "uuid" ** suffix_const "admin")) ()
let election_login = service ~path:["elections"] ~get_params:(suffix_prod (uuid "uuid" ** suffix_const "login") (opt (string "service"))) ()
let election_logout = service ~path:["elections"] ~get_params:(suffix (uuid "uuid" ** suffix_const "logout")) ()
let election_set_featured = post_coservice ~fallback:election_admin ~post_params:(bool "featured") ()
let election_set_state = post_coservice ~fallback:election_admin ~post_params:(bool "state") ()
let election_update_credential = service ~path:["elections"] ~get_params:(suffix (uuid "uuid" ** suffix_const "update-cred")) ()
let election_update_credential_post = post_service ~fallback:election_update_credential ~post_params:(string "old_credential" ** string "new_credential") ()
let election_vote = service ~path:["elections"] ~get_params:(suffix (uuid "uuid" ** suffix_const "vote")) ()
let election_cast = service ~path:["elections"] ~get_params:(suffix (uuid "uuid" ** suffix_const "cast")) ()
let election_cast_post = post_service ~fallback:election_cast ~post_params:(opt (string "encrypted_vote") ** opt (file "encrypted_vote_file")) ()
let election_cast_confirm = post_coservice ~csrf_safe:true ~fallback:election_cast ~post_params:unit ()
let election_pretty_ballots = service ~path:["elections"] ~get_params:(suffix_prod (uuid "uuid" ** suffix_const "ballots") (int "start")) ()
let election_pretty_ballot = service ~path:["elections"] ~get_params:(suffix_prod (uuid "uuid" ** suffix_const "ballot") (string "hash")) ()
let election_dir = service ~path:["elections"] ~get_params:(suffix (uuid "uuid" ** election_file "file")) ()

let scope = Eliom_common.default_session_scope

let cont : (unit -> service_handler) Eliom_reference.eref =
  Eliom_reference.eref ~scope (fun () () -> Eliom_registration.Redirection.send home)

let ballot : string option Eliom_reference.eref =
  Eliom_reference.eref ~scope None

let cast_confirmed : [ `Error of Web_common.error | `Valid of string ] option Eliom_reference.eref =
  Eliom_reference.eref ~scope None
