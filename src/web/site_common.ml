(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

open Lwt
open Belenios
open Serializable_builtin_t
open Common
open Web_serializable_j
open Web_common
open Web_services

let ( / ) = Filename.concat

module PString = String

open Eliom_service
open Eliom_registration

let find_election uuid =
  match%lwt Web_persist.get_raw_election uuid with
  | Some raw_election -> return_some (Election.of_string raw_election)
  | _ -> return_none

let election_not_found () =
  let%lwt l = Web_i18n.get_preferred_gettext () in
  let open (val l) in
  Pages_common.generic_page ~title:(s_ "Not found") (s_ "This election does not exist. This may happen for elections that have not yet been open or have been deleted.") ()
  >>= Html.send ~code:404

let () = File.register ~service:source_code
  ~content_type:"application/x-gzip"
  (fun () () -> return !Web_config.source_file)

let redir_preapply s u () = Redirection.send (Redirection (preapply ~service:s u))

let wrap_handler f =
  try%lwt f () with
  | e -> Pages_common.generic_page ~title:"Error" (Printexc.to_string e) () >>= Html.send

let get_cont_state cont =
  let redir = match cont with
    | ContSiteHome -> Redirection home
    | ContSiteAdmin -> Redirection admin
    | ContSiteElection uuid -> Redirection (preapply ~service:election_home (uuid, ()))
  in
  fun () -> Redirection.send redir

let () =
  Any.register ~service:set_cookie_disclaimer
    (fun cont () ->
      let%lwt () = Eliom_reference.set Web_state.show_cookie_disclaimer false in
      get_cont_state cont ()
    )

let content_type_of_file = function
  | ESRaw -> "application/json; charset=utf-8"
  | ESTrustees | ESETally | ESResult -> "application/json"
  | ESBallots | ESShuffles -> "text/plain" (* should be "application/json-seq", but we don't use RS *)
  | ESCreds | ESRecords | ESVoters -> "text/plain"

let handle_pseudo_file uuid f site_user =
  let%lwt confidential =
    match f with
    | ESRaw | ESTrustees | ESBallots | ESETally | ESCreds | ESShuffles -> return false
    | ESRecords | ESVoters -> return true
    | ESResult ->
       match%lwt Web_persist.get_election_result_hidden uuid with
       | None -> return false
       | Some _ -> return true
  in
  let%lwt () =
    if confidential then (
      let%lwt metadata = Web_persist.get_election_metadata uuid in
      match site_user with
      | Some u when metadata.e_owner = Some u -> return ()
      | _ -> forbidden ()
    ) else return ()
  in
  let content_type = content_type_of_file f in
  File.send ~content_type (!Web_config.spool_dir / raw_string_of_uuid uuid / string_of_election_file f)

let () =
  Any.register ~service:election_dir
    (fun (uuid, f) () ->
     let%lwt site_user = Eliom_reference.get Web_state.site_user in
     handle_pseudo_file uuid f site_user)

let () =
  Any.register ~service:election_nh_ciphertexts
    (fun uuid () ->
      let%lwt x = Web_persist.get_nh_ciphertexts uuid in
      String.send (x, "application/json")
    )

let () =
  Any.register ~service:set_language
    (fun (lang, cont) () ->
      let%lwt () = Eliom_reference.set Web_state.language (Some lang) in
      get_cont_state cont ()
    )
