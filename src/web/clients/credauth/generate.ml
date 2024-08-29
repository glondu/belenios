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

open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios_api.Serializable_j
open Belenios
open Belenios_api
open Belenios_js.Common

let do_generate uuid (Draft (_, draft)) ~voters =
  let voters = Voter.list_of_string voters in
  let version = draft.draft_version in
  let group = draft.draft_group in
  let module G = (val Belenios.Group.of_string ~version group) in
  let module Cred =
    Credential.Make
      (G)
      (struct
        type 'a t = 'a Lwt.t

        let return = Lwt.return
        let bind = Lwt.bind
        let pause = Lwt_js.yield
        let uuid = uuid
        let get_salt _ = Lwt.return_none
      end)
  in
  Cred.generate voters

let make_submit_credentials_div ~uuid ~token ~voters (c : Credential.batch) =
  let open (val !Belenios_js.I18n.gettext) in
  let container = Dom_html.createDiv document in
  let public_credentials = string_of_public_credentials c.public_creds in
  let private_credentials = string_of_private_credentials c.private_creds in
  let submit_div = Dom_html.createDiv document in
  let submit_btn =
    let@ () =
      button ~a:[ a_id "submit"; a_disabled () ]
      @@ s_ "Submit public credentials"
    in
    Dom.removeChild container submit_div;
    let contents = `String (string_of_public_credentials c.public_with_ids) in
    let url = !/(Printf.sprintf "drafts/%s/credentials/public" uuid) in
    let* x = http_perform ~token ~override_method:`POST ~contents url in
    let msg =
      match x.code with
      | 200 -> s_ "Credentials have been received and checked!"
      | _ -> s_ "An error occurred while submitting credentials!"
    in
    let element = div ~a:[ a_id "success" ] [ txt msg ] in
    Dom.appendChild container (Tyxml_js.To_dom.of_div element);
    Lwt.return_unit
  in
  Dom.appendChild submit_div (Tyxml_js.To_dom.of_node submit_btn);
  let download_private_creds =
    let handler _ =
      let r = Tyxml_js.To_dom.of_button submit_btn in
      r##.disabled := Js._false;
      true
    in
    a
      ~a:[ a_id "creds"; a_download @@ Some "creds.txt"; a_onclick handler ]
      ~href:(encode_data_uri ~mime_type:"text/plain" private_credentials)
      (s_ "private credentials")
  in
  List.iter
    (fun x -> Dom.appendChild container @@ Tyxml_js.To_dom.of_node x)
    [
      div [ txt @@ s_ "Public credentials:" ];
      div
        [
          Tyxml_js.Html.textarea
            ~a:[ a_id "pks"; a_rows 5; a_cols 40 ]
            (txt public_credentials);
        ];
      div
        [
          txt @@ s_ "Fingerprint of public credentials:";
          txt " ";
          span
            ~a:[ a_id "public_creds_fp" ]
            [ txt @@ sha256_b64 public_credentials ];
        ];
      div
        [
          b [ txt @@ s_ "Instructions:" ];
          ol
            [
              li
                [
                  txt @@ s_ "Download ";
                  download_private_creds;
                  txt @@ s_ " and save the file to a secure location.";
                  br ();
                  txt @@ s_ "You will use it to send credentials to voters.";
                ];
              li
                [
                  txt @@ s_ "Download ";
                  a
                    ~a:[ a_id "voters_txt"; a_download @@ Some "voters.txt" ]
                    ~href:(encode_data_uri ~mime_type:"text/plain" voters)
                    (s_ "voter list");
                  txt ".";
                  br ();
                  txt
                  @@ s_
                       "This list must be the one approved by the election \
                        commission.";
                ];
              li
                [
                  txt @@ s_ "Save the two fingerprints above.";
                  br ();
                  txt
                  @@ s_
                       "Once the election is open, you must check that they \
                        match with what is published by the server.";
                ];
              li
                [
                  txt @@ s_ "Submit public credentials using the button below.";
                ];
            ];
        ];
    ];
  Dom.appendChild container submit_div;
  Tyxml_js.Of_dom.of_div container

let error () =
  let open (val !Belenios_js.I18n.gettext) in
  Lwt.return [ txt @@ s_ "Error" ]

let generate configuration ~uuid ~token =
  let open (val !Belenios_js.I18n.gettext) in
  let@ draft cont =
    let url = !/(Printf.sprintf "drafts/%s" uuid) in
    let* x = get draft_of_string url in
    match x with None -> error () | Some x -> cont x
  in
  let@ voters cont =
    let url = !/(Printf.sprintf "drafts/%s/voters" uuid) in
    let* x = get ~token voter_list_of_string url in
    match x with None -> error () | Some x -> cont x
  in
  let voters = Voter.list_to_string voters in
  let header = h3 [ txt @@ s_ "Credential generation" ] in
  let link_div =
    let url = Printf.sprintf "%selection#%s" configuration.uris.home uuid in
    div
      [
        txt @@ s_ "The link to the election will be:";
        ul [ li [ span ~a:[ a_id "election_url" ] [ txt url ] ] ];
      ]
  in
  let container = Dom_html.createDiv document in
  let () =
    Dom.appendChild container @@ Tyxml_js.To_dom.of_div
    @@
    let hash = span ~a:[ a_id "voters_hash" ] [ txt @@ sha256_b64 voters ] in
    div
      [
        div [ txt @@ s_ "Voter list:" ];
        div
          [
            Tyxml_js.Html.textarea
              ~a:[ a_rows 5; a_cols 40; a_id "voters" ]
              (txt voters);
          ];
        div [ txt @@ s_ "Fingerprint of voters:"; txt " "; hash ];
      ]
  in
  let generate_div = Dom_html.createDiv document in
  let generate_btn =
    let@ () = button ~a:[ a_id "generate" ] @@ s_ "Generate" in
    Dom.removeChild container generate_div;
    let* c = do_generate (Uuid.wrap uuid) draft ~voters in
    Dom.appendChild container @@ Tyxml_js.To_dom.of_div
    @@ make_submit_credentials_div ~uuid ~token ~voters c;
    Lwt.return_unit
  in
  Dom.appendChild generate_div (Tyxml_js.To_dom.of_button generate_btn);
  Dom.appendChild container generate_div;
  Lwt.return
  @@ [ header; hr (); link_div; hr (); Tyxml_js.Of_dom.of_div container ]
