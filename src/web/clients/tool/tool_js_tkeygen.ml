(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Belenios_core
open Common
open Belenios_tool_common
open Serializable_j
open Belenios_js.Common
open Tool_tkeygen
open Belenios_api.Serializable_j

let tkeygen draft =
  let open (val !Belenios_js.I18n.gettext) in
  let module P : PARAMS = struct
    let group = draft.draft_group
    let version = draft.draft_version
  end in
  let module X = Make (P) (LwtJsRandom) () in
  let open X in
  let* {id=_; priv; pub} = trustee_keygen () in
  let hash =
    let pub = trustee_public_key_of_string Yojson.Safe.read_json pub in
    sha256_b64 (Yojson.Safe.to_string pub.trustee_public_key)
  in
  set_textarea "pk" pub;
  set_content "public_key_fp" hash;
  set_download "private_key" "application/json" "private_key.json" priv;
  set_element_display "submit_form" "inline";
  let downloaded = ref false in
  let () =
    match Dom_html.getElementById_coerce "private_key" Dom_html.CoerceTo.a with
    | None -> ()
    | Some x -> x##.onclick := Dom_html.handler (fun _ -> downloaded := true; Js._true)
  in
  let () =
    let handler =
      let@ _ = Dom_html.handler in
      if not !downloaded then (
        alert @@ s_ "The private key must be downloaded first!";
        Js._false
      ) else Js._true
    in
    let xs = document##getElementsByTagName (Js.string "form") in
    for i = 0 to xs##.length - 1 do
      let$ x = xs##item i in
      let$ x = Dom_html.CoerceTo.form x in
      x##.onsubmit := handler
    done
  in
  Lwt.return_unit

let fail msg =
  set_content "election_url" msg;
  Lwt.return_unit

let fill_interactivity () =
  let open (val !Belenios_js.I18n.gettext) in
  let&&* e = document##getElementById (Js.string "interactivity") in
  let hash = Dom_html.window##.location##.hash |> Js.to_string in
  match extract_uuid_and_token hash with
  | Some (uuid, token) ->
     let@ () = redirect_if_admin "trustee" uuid token in
     let href = Dom_html.window##.location##.href |> Js.to_string in
     set_content "election_url" (build_election_url href uuid);
     set_form_target "data_form" "submit-trustee" uuid token;
     begin
       let url = Printf.sprintf "../../api/drafts/%s" uuid in
       let* x = get draft_of_string url in
       match x with
       | Some draft ->
          let b = Dom_html.createButton document in
          let t = document##createTextNode (Js.string (s_ "Generate a key")) in
          Lwt_js_events.async (fun () ->
              let* _ = Lwt_js_events.click b in
              tkeygen draft
            );
          Dom.appendChild b t;
          Dom.appendChild e b;
          Lwt.return_unit
       | None -> fail "(token error)"
     end
  | None -> fail "(uuid error)"

let () =
  Lwt.async (fun () ->
      let* _ = Lwt_js_events.onload () in
      let* () = Belenios_js.I18n.auto_init "admin" in
      fill_interactivity ()
    )
