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
open Belenios_core.Serializable_builtin_t
open Belenios_core.Common
open Belenios_api.Serializable_j
open Belenios_tool_js_common
open Tool_js_common
open Tool_js_html
open Common

let show_draft_main show_all uuid draft container =
  let@ () = show_in container in
  let t = textarea () in
  t##.value := Js.string (string_of_draft draft);
  let button_save =
    let@ () = button "Save changes" in
    let* x = put_with_token (Js.to_string t##.value) "drafts/%s" uuid in
    let@ () = show_in container in
    generic_proceed x show_all
  in
  let button_delete =
    let@ () = button "Delete draft" in
    if confirm "Are you sure?" then (
      let* x = delete_with_token "drafts/%s" uuid in
      let@ () = show_in container in
      let@ () = generic_proceed x in
      Dom_html.window##.location##.hash := Js.string "";
      Lwt.return_unit
    ) else (
      Lwt.return_unit
    )
  in
  Lwt.return [
      node @@ div [node @@ t];
      node @@ div [node button_save];
      node @@ div [node button_delete];
    ]

let rec show_draft_voters uuid draft container =
  let@ () = show_in container in
  let* x = get voter_list_of_string "drafts/%s/voters" uuid in
  let@ voters = with_ok "voters" x in
  let t = textarea () in
  t##.value := Js.string (string_of_voter_list voters);
  let b =
    let@ () = button "Save changes" in
    let* x = put_with_token (Js.to_string t##.value) "drafts/%s/voters" uuid in
    let@ () = show_in container in
    generic_proceed x (fun () -> show_draft_voters uuid draft container)
  in
  let import =
    let i = input [] in
    let b =
      let@ () = button "Import voters" in
      let r = `ImportVoters (uuid_of_raw_string (Js.to_string i##.value)) in
      let* x = post_with_token (string_of_draft_request r) "drafts/%s" uuid in
      let@ () = show_in container in
      generic_proceed x (fun () -> show_draft_voters uuid draft container)
    in
    div [node i; node b]
  in
  Lwt.return [node @@ div [node @@ t]; node @@ div [node b]; node import]

let rec show_draft_passwords uuid container =
  let@ () = show_in container in
  let* x = get voter_list_of_string "drafts/%s/voters" uuid in
  let@ voters = with_ok "voters" x in
  let* x = get voter_list_of_string "drafts/%s/passwords" uuid in
  let@ x = with_ok "passwords" x in
  let missing =
    let x = List.fold_left (fun accu v -> SSet.add v accu) SSet.empty x in
    List.filter (fun v -> not @@ SSet.mem v x) voters
  in
  let t1 = textarea () in
  t1##.value := Js.string (string_of_voter_list x);
  let t2 = textarea () in
  t2##.value := Js.string (string_of_voter_list missing);
  let b =
    let@ () = button "Generate and send passwords" in
    let* x = post_with_token (Js.to_string t2##.value) "drafts/%s/passwords" uuid in
    let@ () = show_in container in
    generic_proceed x (fun () -> show_draft_passwords uuid container)
  in
  Lwt.return [node @@ div [node @@ t1]; node @@ div [node @@ t2]; node @@ div [node b]]

let rec show_draft_credentials uuid container =
  let@ () = show_in container in
  let* x = get credentials_of_string "drafts/%s/credentials" uuid in
  let@ x = with_ok "credentials" x in
  match x.credentials_public, x.credentials_token with
  | None, None ->
     let b =
       let@ () = button "Generate on server" in
       let op = string_of_credential_list [] in
       let* x = post_with_token op "drafts/%s/credentials" uuid in
       let@ () = show_in container in
       generic_proceed x (fun () -> show_draft_credentials uuid container)
     in
     Lwt.return [node @@ b]
  | None, Some token ->
     let link = Js.to_string Dom_html.window##.location##.href ^ "@" ^ token in
     let module X = Belenios_ui.Mails_admin.Make (I18n) in
     let subject, body = X.mail_credential_authority !gettext link in
     Lwt.return [
         node @@ a_mailto ~recipient:"" ~subject ~body "Send an e-mail to the credential authority";
         txt " ";
         txt "or send the following link manually:";
         txt " ";
         txt link;
       ]
  | Some _, _ ->
     let t = textarea () in
     t##.value := Js.string (string_of_credentials x);
     Lwt.return [node @@ t]

let rec show_draft_trustees uuid container =
  let@ () = show_in container in
  let* x = get trustees_of_string "drafts/%s/trustees" uuid in
  let@ trustees = with_ok "trustees" x in
  let* mode =
    let* x = get trustees_mode_of_string "drafts/%s/trustees-mode" uuid in
    match x with
    | Error e -> Lwt.return @@ Printf.sprintf "error (%s)" (string_of_error e)
    | Ok `Basic -> Lwt.return "basic"
    | Ok (`Threshold threshold) ->
       let threshold =
         match threshold with
         | 0 -> "not set"
         | i -> Printf.sprintf "%d out of %d" i (List.length trustees)
       in
       Lwt.return @@ Printf.sprintf "threshold (%s)" threshold
  in
  let mode = div [txt "Mode:"; txt " "; txt mode] in
  let mode_set =
    let t = textarea ~rows:1 ~cols:60 () in
    let b =
      let@ () = button "Set mode" in
      let* x = put_with_token (Js.to_string t##.value) "drafts/%s/trustees-mode" uuid in
      let@ () = show_in container in
      generic_proceed x (fun () -> show_draft_trustees uuid container)
    in
    div [node t; txt " "; node b]
  in
  let all_trustees =
    List.map
      (fun t ->
        let encoded_trustee = t.trustee_address |> Js.string |> Js.encodeURIComponent |> Js.to_string in
        let content =
          let b =
            let@ () = button "Delete" in
            let* x = delete_with_token "drafts/%s/trustees/%s" uuid encoded_trustee in
            let@ () = show_in container in
            generic_proceed x (fun () -> show_draft_trustees uuid container)
          in
          [txt (string_of_trustee t); txt " "; node @@ b]
        in
        node @@ li content
      ) trustees
  in
  let all_trustees = ul all_trustees in
  let t2 = textarea () in
  let b =
    let@ () = button "Add trustee" in
    let* x = post_with_token (Js.to_string t2##.value) "drafts/%s/trustees" uuid in
    let@ () = show_in container in
    generic_proceed x (fun () -> show_draft_trustees uuid container)
  in
  let import =
    let i = input [] in
    let b =
      let@ () = button "Import trustees" in
      let r = `ImportTrustees (uuid_of_raw_string (Js.to_string i##.value)) in
      let* x = post_with_token (string_of_draft_request r) "drafts/%s" uuid in
      let@ () = show_in container in
      generic_proceed x (fun () -> show_draft_trustees uuid container)
    in
    div [node i; node b]
  in
  Lwt.return [
      node @@ mode;
      node @@ mode_set;
      node @@ div [node all_trustees];
      node @@ div [node t2];
      node @@ div [node b];
      node @@ import;
    ]

let rec show_draft_status uuid container =
  let@ () = show_in container in
  let* x = get status_of_string "drafts/%s/status" uuid in
  let@ status = with_ok "status" x in
  let t = textarea () in
  t##.value := Js.string (string_of_status status);
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
        node @@ b "Set downloaded" `SetDownloaded;
        node @@ b "Validate election" `ValidateElection;
      ]
  in
  Lwt.return [node @@ div [node t]; node buttons]

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
           node @@ h1 [txt "Error"];
           node @@ div [txt msg];
         ]
    | Ok draft ->
       let title = h2 [] in
       let container = div [] in
       let* () =
         let@ () = show_in main in
         let tabs =
           ul [
               node @@ li [node @@ a_draft_tab uuid `Draft];
               node @@ li [node @@ a_draft_tab uuid `Voters];
               node @@ li [node @@ a_draft_tab uuid `Passwords];
               node @@ li [node @@ a_draft_tab uuid `Credentials];
               node @@ li [node @@ a_draft_tab uuid `Trustees];
               node @@ li [node @@ a_draft_tab uuid `Status];
             ]
         in
         Lwt.return [
             node @@ div [node @@ a ~href:"#" "Home"];
             node @@ h1 [txt draft.draft_questions.t_name];
             node @@ tabs;
             node @@ title;
             node @@ container;
           ]
       in
       context := `Draft (draft, title, container);
       show_draft show_all uuid draft title container tab
  in
  match !context with
  | `Draft (draft, title, container) ->
     show_draft show_all uuid draft title container tab
  | _ -> show_all ()
