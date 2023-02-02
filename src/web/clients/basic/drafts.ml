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
open Js_of_ocaml_tyxml
open Belenios_core.Signatures
open Belenios_core.Common
open Belenios_api.Serializable_j
open Tyxml_js.Html5
open Belenios_js.Common
open Common

let show_draft_main show_all uuid draft container =
  let@ () = show_in container in
  let draft_str = string_of_draft draft in
  let t, tget = textarea draft_str in
  let ifmatch = sha256_b64 draft_str in
  let button_save =
    let@ () = button "Save changes" in
    let* x = put_with_token ~ifmatch (tget ()) "drafts/%s" uuid in
    let@ () = show_in container in
    generic_proceed x show_all
  in
  let button_delete =
    let@ () = button "Delete draft" in
    if confirm "Are you sure?" then (
      let* x = delete_with_token ~ifmatch "drafts/%s" uuid in
      let@ () = show_in container in
      let@ () = generic_proceed x in
      Dom_html.window##.location##.hash := Js.string "";
      Lwt.return_unit
    ) else (
      Lwt.return_unit
    )
  in
  Lwt.return [
      div [t];
      div [button_save];
      div [button_delete];
    ]

let rec show_draft_voters uuid draft container =
  let@ () = show_in container in
  let* x = get voter_list_of_string "drafts/%s/voters" uuid in
  let@ voters = with_ok "voters" x in
  let voters_str = string_of_voter_list voters in
  let t, tget = textarea voters_str in
  let ifmatch = sha256_b64 voters_str in
  let b =
    let@ () = button "Save changes" in
    let* x = put_with_token ~ifmatch (tget ()) "drafts/%s/voters" uuid in
    let@ () = show_in container in
    generic_proceed x (fun () -> show_draft_voters uuid draft container)
  in
  let import =
    let i, iget = input "" in
    let b =
      let@ () = button "Import voters" in
      let r = `Import (Uuid.wrap (iget ())) in
      let* x = post_with_token ~ifmatch (string_of_voters_request r) "drafts/%s/voters" uuid in
      let@ () = show_in container in
      generic_proceed x (fun () -> show_draft_voters uuid draft container)
    in
    div [i; b]
  in
  Lwt.return [div [t]; div [b]; import]

let rec show_draft_passwords uuid container =
  let@ () = show_in container in
  let* x = get voter_list_of_string "drafts/%s/voters" uuid in
  let@ voters = with_ok "voters" x in
  let* x = get string_list_of_string "drafts/%s/passwords" uuid in
  let ifmatch = compute_ifmatch string_of_string_list x in
  let@ x = with_ok "passwords" x in
  let missing =
    let x = List.fold_left (fun accu v -> SSet.add (String.lowercase_ascii v) accu) SSet.empty x in
    List.filter_map
      (fun v ->
        let _, login, _ = Voter.get v in
        if SSet.mem (String.lowercase_ascii login) x then None else Some login
      ) voters
  in
  let t1, _ = textarea (string_of_string_list x) in
  let t2, t2get = textarea (string_of_string_list missing) in
  let b =
    let@ () = button "Generate and send passwords" in
    let* x = post_with_token ?ifmatch (t2get ()) "drafts/%s/passwords" uuid in
    let@ () = show_in container in
    generic_proceed x (fun () -> show_draft_passwords uuid container)
  in
  Lwt.return [div [t1]; div [t2]; div [b]]

let rec show_draft_credentials uuid container =
  let@ () = show_in container in
  let* x = get public_credentials_of_string "drafts/%s/credentials/public" uuid in
  let@ x = with_ok_not_found "credentials" x in
  match x with
  | None ->
     let* x = get (fun x -> x) "drafts/%s/credentials/token" uuid in
     let@ x = with_ok_not_found "token" x in
     begin
       match x with
       | None ->
          let b =
            let@ () = button "Generate on server" in
            let op = string_of_public_credentials [] in
            let* x = post_with_token op "drafts/%s/credentials" uuid in
            let@ () = show_in container in
            generic_proceed x (fun () -> show_draft_credentials uuid container)
          in
          Lwt.return [b]
       | Some token ->
          let link = Js.to_string Dom_html.window##.location##.href ^ "@" ^ token in
          let module X = Belenios_ui.Mails_admin.Make (Belenios_js.I18n) in
          let subject, body = X.mail_credential_authority !Belenios_js.I18n.gettext link in
          Lwt.return [
              a_mailto ~recipient:"" ~subject ~body "Send an e-mail to the credential authority";
              txt " ";
              txt "or send the following link manually:";
              txt " ";
              txt link;
            ]
     end
  | Some x ->
     let t, _ = textarea (string_of_public_credentials x) in
     Lwt.return [t]

type trustee_with_writer =
  TWW : 'a trustee list * 'a writer -> trustee_with_writer

let rec show_draft_trustees uuid container =
  let@ () = show_in container in
  let* x = get draft_trustees_of_string "drafts/%s/trustees" uuid in
  let ifmatch = compute_ifmatch string_of_draft_trustees x in
  let@ trustees = with_ok "trustees" x in
  let mode =
    match trustees with
    | `Basic _ -> "basic"
    | `Threshold t ->
       let threshold =
         match t.tt_threshold with
         | None -> "not set"
         | Some i -> Printf.sprintf "%d out of %d" i (List.length t.tt_trustees)
       in
       Printf.sprintf "threshold (%s)" threshold
  in
  let mode = div [txt "Mode:"; txt " "; txt mode] in
  let mode_set =
    let t, tget = textarea ~rows:1 ~cols:60 "" in
    let b =
      let@ () = button "Set mode" in
      let@ request = fun cont ->
        match Yojson.Safe.from_string (tget ()) with
        | `String "Basic" -> cont `SetBasic
        | `List [`String "Threshold"; `Int i] -> cont (`SetThreshold i)
        | _ -> alert "Unrecognized mode"; Lwt.return_unit
      in
      let* x = post_with_token ?ifmatch (string_of_trustees_request request) "drafts/%s/trustees" uuid in
      let@ () = show_in container in
      generic_proceed x (fun () -> show_draft_trustees uuid container)
    in
    div [t; txt " "; b]
  in
  let TWW (trustees, write) =
    match trustees with
    | `Basic x -> TWW (x.bt_trustees, write_trustee_public_key Yojson.Safe.write_json)
    | `Threshold x -> TWW (x.tt_trustees, write_cert)
  in
  let all_trustees =
    List.map
      (fun t ->
        let encoded_trustee =
          Option.value ~default:"@" t.trustee_address
          |> Js.string
          |> Js.encodeURIComponent
          |> Js.to_string
        in
        let content =
          let b =
            let@ () = button "Delete" in
            let* x = delete_with_token "drafts/%s/trustees/%s" uuid encoded_trustee in
            let@ () = show_in container in
            generic_proceed x (fun () -> show_draft_trustees uuid container)
          in
          [txt (string_of_trustee write t); txt " "; b]
        in
        li content
      ) trustees
  in
  let all_trustees = ul all_trustees in
  let t2, t2get = textarea "" in
  let b =
    let@ () = button "Add trustee" in
    let r = `Add (trustee_of_string Yojson.Safe.read_json (t2get ())) in
    let* x = post_with_token ?ifmatch (string_of_trustees_request r) "drafts/%s/trustees" uuid in
    let@ () = show_in container in
    generic_proceed x (fun () -> show_draft_trustees uuid container)
  in
  let import =
    let i, iget = input "" in
    let b =
      let@ () = button "Import trustees" in
      let r = `Import (Uuid.wrap (iget ())) in
      let* x = post_with_token ?ifmatch (string_of_trustees_request r) "drafts/%s/trustees" uuid in
      let@ () = show_in container in
      generic_proceed x (fun () -> show_draft_trustees uuid container)
    in
    div [i; b]
  in
  Lwt.return [
      mode;
      mode_set;
      div [all_trustees];
      div [t2];
      div [b];
      import;
    ]

let rec show_draft_status uuid container =
  let@ () = show_in container in
  let* x = get draft_status_of_string "drafts/%s/status" uuid in
  let@ status = with_ok "status" x in
  let t, _ = textarea (string_of_draft_status status) in
  let b label r =
    let@ () = button label in
    let* x = post_with_token (string_of_draft_request r) "drafts/%s" uuid in
    let@ () = show_in container in
    let@ () = generic_proceed x in
    match r, x.code with
    | `ValidateElection, 200 ->
       let new_hash = Printf.sprintf "#elections/%s" uuid in
       Dom_html.window##.location##.hash := Js.string new_hash;
       Lwt.return_unit
    | _ -> show_draft_status uuid container
  in
  let buttons =
    div [
        b "Set downloaded" `SetDownloaded;
        b "Validate election" `ValidateElection;
      ]
  in
  Lwt.return [div [t]; buttons]

let suffix_and_label_of_draft_tab = function
  | `Draft -> "", "Draft"
  | `Voters -> "/voters", "Voters"
  | `Passwords -> "/passwords", "Passwords"
  | `Credentials -> "/credentials", "Credentials"
  | `Trustees -> "/trustees", "Trustees"
  | `Status -> "/status", "Status"

let show_draft show_all uuid draft title container tab =
  container##.innerHTML := Js.string "Loading...";
  let _, label = suffix_and_label_of_draft_tab tab in
  let* () =
    let@ () = show_in title in
    Lwt.return [txt label]
  in
  match tab with
  | `Draft -> show_draft_main show_all uuid draft container
  | `Voters -> show_draft_voters uuid draft container
  | `Passwords -> show_draft_passwords uuid container
  | `Credentials -> show_draft_credentials uuid container
  | `Trustees -> show_draft_trustees uuid container
  | `Status -> show_draft_status uuid container

let a_draft_tab uuid tab =
  let suffix, label = suffix_and_label_of_draft_tab tab in
  let href = Printf.sprintf "#drafts/%s%s" uuid suffix in
  a ~href label

let show main uuid tab context =
  let rec show_all () =
    let* x = get draft_of_string "drafts/%s" uuid in
    match x with
    | Error e ->
       let@ () = show_in main in
       let msg =
         Printf.sprintf "An error occurred while retrieving draft %s: %s"
           uuid (string_of_error e)
       in
       Lwt.return [
           h1 [txt "Error"];
           div [txt msg];
         ]
    | Ok draft ->
       let title = h2 [] in
       let container = div [] in
       let* () =
         let@ () = show_in main in
         let tabs =
           ul [
               li [a_draft_tab uuid `Draft];
               li [a_draft_tab uuid `Voters];
               li [a_draft_tab uuid `Passwords];
               li [a_draft_tab uuid `Credentials];
               li [a_draft_tab uuid `Trustees];
               li [a_draft_tab uuid `Status];
             ]
         in
         Lwt.return [
             div [a ~href:"#" "Home"];
             h1 [txt draft.draft_questions.t_name];
             tabs;
             title;
             container;
           ]
       in
       let title = Tyxml_js.To_dom.of_h2 title in
       let container = Tyxml_js.To_dom.of_div container in
       context := `Draft (draft, title, container);
       show_draft show_all uuid draft title container tab
  in
  match !context with
  | `Draft (draft, title, container) ->
     show_draft show_all uuid draft title container tab
  | _ -> show_all ()
