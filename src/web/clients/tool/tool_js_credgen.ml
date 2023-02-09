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

open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
open Belenios_core.Common
open Belenios_tool_common
open Belenios_js.Common
open Tool_credgen
open Belenios_api.Serializable_j

let generate uuid draft =
  let raw = get_textarea "voters" in
  let ids = Voter.list_of_string raw in
  let module P : PARAMS = struct
    let version = draft.draft_version
    let uuid = uuid
    let group = draft.draft_group
  end in
  let module X = Make (P) (Random) () in
  let c = X.generate ids in
  set_textarea "pks" (string_of_public_credentials c.public_with_ids);
  let hash = sha256_b64 (string_of_public_credentials c.public) in
  set_content "public_creds_fp" hash;
  let text_creds = string_of_private_credentials c.priv in
  set_download "creds" "text/plain" "creds.txt" text_creds;
  set_download "voters_txt" "text/plain" "voters.txt" raw;
  set_element_display "submit_form" "inline";
  Lwt.return_unit

let fill_interactivity () =
  let open (val !Belenios_js.I18n.gettext) in
  let@ uuid, token = fun cont ->
    let hash = Dom_html.window##.location##.hash |> Js.to_string in
    match extract_uuid_and_token hash with
    | Some (uuid, token) -> cont (uuid, token)
    | None ->
       alert "Unable to extract UUID and token from URL";
       Lwt.return_unit
  in
  let@ () = redirect_if_admin "credentials" uuid token in
  set_form_target "submit_form" "submit-credentials" uuid token;
  set_form_target "submit_form_file" "submit-credentials-file" uuid token;
  let href = Dom_html.window##.location##.href |> Js.to_string in
  set_content "election_url" (build_election_url href uuid);
  let@ draft = fun cont ->
    let url = Printf.sprintf "../api/drafts/%s" uuid in
    let* x = get draft_of_string url in
    match x with
    | Some x -> cont x
    | None ->
       alert "Unable to get draft";
       Lwt.return_unit
  in
  let@ voters = fun cont ->
    let url = Printf.sprintf "../api/drafts/%s/voters" uuid in
    let* x = get ~token voter_list_of_string url in
    match x with
    | Some x -> cont x
    | None ->
       alert "Unable to get voters";
       Lwt.return_unit
  in
  let raw = Voter.list_to_string voters in
  set_textarea "voters" raw;
  set_content "voters_hash" (sha256_b64 raw);
  let&&* e = document##getElementById (Js.string "interactivity") in
  let x = Dom_html.createDiv document in
  Dom.appendChild e x;
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string (s_ "Generate")) in
  Lwt_js_events.async (fun () ->
      let* _ = Lwt_js_events.click b in
      generate uuid draft
    );
  Dom.appendChild b t;
  Dom.appendChild x b;
  Lwt.return_unit

let () =
  Lwt.async (fun () ->
      let* _ = Lwt_js_events.onload () in
      let* () = Belenios_js.I18n.auto_init "admin" in
      fill_interactivity ()
    )
