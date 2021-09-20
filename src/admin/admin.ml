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
open Belenios_platform.Platform
open Belenios_core.Signatures
open Belenios_core.Serializable_builtin_t
open Belenios_core.Common
open Belenios_api.Serializable_j
open Belenios_tool_js_common
open Tool_js_common
open Tool_js_html

let ( let& ) = Js.Opt.bind

let api_token = ref ""
let api_root = "../api/"

let context = ref `None

let lwt_handler f =
  Dom_html.handler (fun _ -> Lwt.async f; Js._true)

let show_in container f =
  let* content = f () in
  container##.innerHTML := Js.string "";
  List.iter (Dom.appendChild container) content;
  Lwt.return_unit

let textarea ?(cols = 80) ?(rows = 10) () =
  let r = textarea [] in
  r##.cols := cols;
  r##.rows := rows;
  r

let button label handler =
  let r = button [txt label] in
  r##.onclick := lwt_handler handler;
  r

let a ~href label =
  let r = a [txt label] in
  r##.href := Js.string href;
  r

let get_api_token () =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = get "../api-token" in
  if x.code = 200 then (
    api_token := x.content;
    Lwt.return_true
  ) else (
    Lwt.return_false
  )

let get_with_token url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  Printf.ksprintf (fun x -> perform_raw_url ~headers (api_root ^ x)) url

let delete_with_token url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  Printf.ksprintf (fun x -> perform_raw_url ~headers ~override_method:`DELETE (api_root ^ x)) url

let put_with_token x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  let contents = `String x in
  Printf.ksprintf (fun x -> perform_raw_url ~headers ~contents ~override_method:`PUT (api_root ^ x)) url

let post_with_token x url =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let headers = ["Authorization", "Bearer " ^ !api_token] in
  let contents = `String x in
  Printf.ksprintf (fun x -> perform_raw_url ~headers ~contents ~override_method:`POST (api_root ^ x)) url

let bad_result = Lwt.return (Error `BadResult)

let string_of_error = function
  | `BadResult -> "bad result"
  | `BadStatus (code, content) -> Printf.sprintf "bad status %d: %s" code content

let wrap of_string x =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* x = x in
  match x.code with
  | 200 ->
     let@ x = Option.unwrap bad_result (Option.wrap of_string x.content) in
     Lwt.return @@ Ok x
  | code -> Lwt.return @@ Error (`BadStatus (code, x.content))

let get_account () =
  get_with_token "account" |> wrap api_account_of_string

let get_drafts () =
  get_with_token "drafts" |> wrap summary_list_of_string

let post_drafts draft =
  post_with_token draft "drafts" |> wrap uuid_of_string

let get_draft uuid =
  get_with_token "drafts/%s" uuid |> wrap draft_of_string

let get_voters uuid =
  get_with_token "drafts/%s/voters" uuid |> wrap voter_list_of_string

let get_passwords uuid =
  get_with_token "drafts/%s/passwords" uuid |> wrap voter_list_of_string

let get_credentials uuid =
  get_with_token "drafts/%s/credentials" uuid |> wrap credentials_of_string

let get_trustees uuid =
  get_with_token "drafts/%s/trustees" uuid |> wrap trustees_of_string

let get_trustees_mode uuid =
  get_with_token "drafts/%s/trustees-mode" uuid |> wrap trustees_mode_of_string

let get_status uuid =
  get_with_token "drafts/%s/status" uuid |> wrap status_of_string

module CG = Belenios_core.Credential.MakeGenerate (LwtJsRandom)

let credentials_ui main uuid =
  let@ () = show_in main in
  let* x = get_draft uuid in
  match x with
  | Error e ->
     let msg =
       Printf.sprintf
         "An error occurred while retrieving draft %s: %s"
         uuid (string_of_error e)
     in
     Lwt.return [
         node @@ h1 [txt "Error"];
         node @@ div [txt msg];
       ]
  | Ok draft ->
     let* voters =
       let* x = get_voters uuid in
       match x with
       | Error e ->
          let msg =
            Printf.sprintf
              "An error occurred while retrieving voters of draft %s: %s"
              uuid (string_of_error e)
          in
          Lwt.return @@ div [txt msg]
       | Ok xs ->
          let fingerprint =
            String.concat "\n" xs ^ "\n"
            |> sha256_b64
          in
          let fingerprint =
            Printf.sprintf
              "The voter list has %d voter(s) and its fingerprint is %s."
              (List.length xs) fingerprint
          in
          let container = div [] in
          let b =
            let@ () = button "Generate credentials" in
            let uuid_ = uuid_of_raw_string uuid in
            let show_weight =
              List.exists
                (fun v ->
                  let _, _, weight = split_identity_opt v in
                  weight <> None
                ) xs
            in
            let version = draft.draft_version in
            let module G = (val Belenios.Group.of_string ~version draft.draft_group : GROUP) in
            let module CMap = Map.Make (G) in
            let module CD = Belenios_core.Credential.MakeDerive (G) in
            let* public_creds, private_creds =
              Lwt_list.fold_left_s (fun (public_creds, private_creds) v ->
                  let _, _, weight = split_identity v in
                  let* cred = CG.generate () in
                  let pub_cred =
                    let x = CD.derive uuid_ cred in
                    G.(g **~ x)
                  in
                  Lwt.return (CMap.add pub_cred weight public_creds, (v, cred) :: private_creds)
                ) (CMap.empty, []) xs
            in
            let private_creds = List.rev_map (fun (id, c) -> id ^ " " ^ c) private_creds in
            let public_creds =
              CMap.bindings public_creds
              |> List.map
                   (fun (cred, weight) ->
                     let cred = G.to_string cred in
                     if show_weight then
                       Printf.sprintf "%s,%s" cred (Weight.to_string weight)
                     else cred
                   )
            in
            let op = string_of_credential_list public_creds in
            let t = textarea () in
            t##.value := Js.string (String.concat "\n" private_creds ^ "\n");
            let button_container = div [] in
            let b =
              let@ () = button "Send public credentials to server" in
              let* x = post_with_token op "drafts/%s/credentials" uuid in
              let@ () = show_in button_container in
              let msg =
                match x.code with
                | 200 -> "Public credentials successfully uploaded!"
                | code -> Printf.sprintf "Error %d: %s" code x.content
              in
              Lwt.return [txt msg]
            in
            Dom.appendChild button_container b;
            let@ () = show_in container in
            Lwt.return [
                node @@ div [node @@ t];
                node @@ button_container;
              ]
          in
          Dom.appendChild container b;
          Lwt.return
          @@ div [
                 node @@ div [txt fingerprint];
                 node @@ container;
               ]
     in
     Lwt.return [
         node @@ h1 [txt (Printf.sprintf "Credentials for %s" draft.draft_questions.t_name)];
         node @@ voters;
       ]

let draft_a x =
  let uuid = raw_string_of_uuid x.summary_uuid in
  a ~href:("#drafts/" ^ uuid) (if x.summary_name = "" then "(no title)" else x.summary_name)

let regexps =
  [
    Regexp.regexp "^#?$", (fun _ -> `Root);
    Regexp.regexp "^#drafts/([0-9A-Za-z]+)(/(voters|passwords|credentials|trustees|status))?$",
    begin
      fun r ->
      match Regexp.matched_group r 1, Regexp.matched_group r 3 with
      | Some uuid, None -> `Draft (uuid, `Draft)
      | Some uuid, Some "voters" -> `Draft (uuid, `Voters)
      | Some uuid, Some "passwords" -> `Draft (uuid, `Passwords)
      | Some uuid, Some "credentials" -> `Draft (uuid, `Credentials)
      | Some uuid, Some "trustees" -> `Draft (uuid, `Trustees)
      | Some uuid, Some "status" -> `Draft (uuid, `Status)
      | _ -> `Error
    end;
    Regexp.regexp "^#drafts/([0-9A-Za-z]+)/credentials@([0-9A-Za-z]+)$",
    begin
      fun r ->
      match Regexp.matched_group r 1, Regexp.matched_group r 2 with
      | Some uuid, Some token -> `Credentials (uuid, token)
      | _ -> `Error
    end;
  ]

let parse_hash () =
  let x = Js.to_string Dom_html.window##.location##.hash in
  let r = List.find_map (fun (r, f) -> Option.map f @@ Regexp.string_match r x 0) regexps in
  Option.get r `Error

let show_error main =
  main##.innerHTML := Js.string "Error";
  Lwt.return_unit

let rec show_root main =
  main##.innerHTML := Js.string "Loading...";
  let@ () = show_in main in
  let* account =
    let* x = get_account () in
    match x with
    | Error error ->
       error
       |> string_of_error
       |> (fun x -> Printf.sprintf "An error occurred while retrieving account: %s" x)
       |> (fun x -> Lwt.return @@ div [txt x])
    | Ok account ->
       let t = textarea ~rows:2 () in
       t##.value := Js.string (string_of_api_account account);
       let b =
         let@ () = button "Save changes" in
         let* x = put_with_token (Js.to_string t##.value) "account" in
         let@ () = show_in main in
         let msg =
           match x.code with
           | 200 -> "Changes successfully applied!"
           | code -> Printf.sprintf "Error %d: %s" code x.content
         in
         let b = button "Proceed" (fun () -> show_root main) in
         Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
       in
       Lwt.return @@ div [node @@ div [node t]; node @@ div [node b]]
  in
  let* drafts =
    let* x = get_drafts () in
    match x with
    | Error error ->
       error
       |> string_of_error
       |> (fun x -> Printf.sprintf "An error occurred while retrieving drafts: %s" x)
       |> (fun x -> Lwt.return @@ div [txt x])
    | Ok drafts ->
       drafts
       |> List.sort (fun a b -> compare a.summary_date b.summary_date)
       |> List.map (fun x -> node @@ li [node @@ draft_a x])
       |> (fun xs -> Lwt.return @@ ul xs)
  in
  let create =
    let t = textarea () in
    let b =
      let@ () = button "Create new draft" in
      let* x = post_drafts (Js.to_string t##.value) in
      match x with
      | Ok uuid ->
         Dom_html.window##.location##.hash := Js.string ("#drafts/" ^ raw_string_of_uuid uuid);
         Lwt.return_unit
      | Error e ->
         let@ () = show_in main in
         let msg =
           Printf.ksprintf txt
             "An error occurred while creating the draft: %s"
             (string_of_error e)
         in
         let b = button "Proceed" (fun () -> show_root main) in
         Lwt.return [node @@ div [msg]; node @@ div [node @@ b]]
    in
    div [
        node @@ div [node @@ t];
        node @@ div [node @@ b];
      ]
  in
  Lwt.return [
      node @@ h1 [txt "My account"];
      node @@ account;
      node @@ h1 [txt "My draft elections"];
      node @@ drafts;
      node @@ h1 [txt "Create new draft"];
      node @@ create;
    ]

let show_draft_main show_root show_all uuid draft container =
  let@ () = show_in container in
  let t = textarea () in
  t##.value := Js.string (string_of_draft draft);
  let button_save =
    let@ () = button "Save changes" in
    let* x = put_with_token (Js.to_string t##.value) "drafts/%s" uuid in
    let@ () = show_in container in
    let msg =
      match x.code with
      | 200 -> "Changes successfully applied!"
      | code -> Printf.sprintf "Error %d: %s" code x.content
    in
    let b = button "Proceed" show_all in
    Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
  in
  let button_delete =
    let@ () = button "Delete draft" in
    if confirm "Are you sure?" then (
      let* x = delete_with_token "drafts/%s" uuid in
      let@ () = show_in container in
      let msg =
        match x.code with
        | 200 -> "Draft successfully deleted!"
        | code -> Printf.sprintf "Error %d: %s" code x.content
      in
      let b = button "Proceed" show_root in
      Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
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
  let* x = get_voters uuid in
  match x with
  | Error e ->
     let msg = Printf.sprintf "Error: %s" (string_of_error e) in
     Lwt.return [txt msg]
  | Ok voters ->
     let t = textarea () in
     t##.value := Js.string (string_of_voter_list voters);
     let b =
       let@ () = button "Save changes" in
       let* x = put_with_token (Js.to_string t##.value) "drafts/%s/voters" uuid in
       let@ () = show_in container in
       let msg =
         match x.code with
         | 200 -> "Changes successfully applied!"
         | code -> Printf.sprintf "Error %d: %s" code x.content
       in
       let b = button "Proceed" (fun () -> show_draft_voters uuid draft container) in
       Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
     in
     Lwt.return [node @@ div [node @@ t]; node @@ div [node b]]

let rec show_draft_passwords uuid container =
  let@ () = show_in container in
  let* x = get_voters uuid in
  match x with
  | Error e ->
     let msg = Printf.sprintf "Error while retrieving voters: %s" (string_of_error e) in
     Lwt.return [txt msg]
  | Ok voters ->
     let* x = get_passwords uuid in
     match x with
     | Error e ->
        let msg = Printf.sprintf "Error while retrieving passwords: %s" (string_of_error e) in
        Lwt.return [txt msg]
     | Ok x ->
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
          let msg =
            match x.code with
            | 200 -> "Passwords successfully generated and sent!"
            | code -> Printf.sprintf "Error %d: %s" code x.content
          in
          let b = button "Proceed" (fun () -> show_draft_passwords uuid container) in
          Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
        in
        Lwt.return [node @@ div [node @@ t1]; node @@ div [node @@ t2]; node @@ div [node b]]

let rec show_draft_credentials uuid container =
  let@ () = show_in container in
  let* x = get_credentials uuid in
  match x with
  | Error e ->
     let msg = Printf.sprintf "Error: %s" (string_of_error e) in
     Lwt.return [txt msg]
  | Ok x ->
     match x.credentials_public, x.credentials_token with
     | None, None ->
        let b =
          let@ () = button "Generate on server" in
          let op = string_of_credential_list [] in
          let* x = post_with_token op "drafts/%s/credentials" uuid in
          let@ () = show_in container in
          let msg =
            match x.code with
            | 200 -> "Credentials successfully generated and sent!"
            | code -> Printf.sprintf "Error %d: %s" code x.content
          in
          let b = button "Proceed" (fun () -> show_draft_credentials uuid container) in
          Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
        in
        Lwt.return [node @@ b]
     | None, Some token ->
        let link = Js.to_string Dom_html.window##.location##.href ^ "@" ^ token in
        Lwt.return [
            txt "Send the following link to the credential authority:";
            txt " ";
            txt link;
          ]
     | Some _, _ ->
        let t = textarea () in
        t##.value := Js.string (string_of_credentials x);
        Lwt.return [node @@ t]

let rec show_draft_trustees uuid container =
  let@ () = show_in container in
  let* x = get_trustees uuid in
  match x with
  | Error e ->
     let msg = Printf.sprintf "Error: %s" (string_of_error e) in
     Lwt.return [txt msg]
  | Ok trustees ->
     let* mode =
       let* x = get_trustees_mode uuid in
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
         let msg =
           match x.code with
           | 200 -> "Success"
           | code -> Printf.sprintf "Error %d: %s" code x.content
         in
         let b = button "Proceed" (fun () -> show_draft_trustees uuid container) in
         Lwt.return [node @@ div [txt msg]; node @@ div [node b]]
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
               let msg =
                 match x.code with
                 | 200 -> "Success"
                 | code -> Printf.sprintf "Error %d: %s" code x.content
               in
               let b = button "Proceed" (fun () -> show_draft_trustees uuid container) in
               Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
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
       let msg =
         match x.code with
         | 200 -> "Success"
         | code -> Printf.sprintf "Error %d: %s" code x.content
       in
       let b = button "Proceed" (fun () -> show_draft_trustees uuid container) in
       Lwt.return [node @@ div [txt msg]; node @@ div [node @@ b]]
     in
     Lwt.return [
         node @@ mode;
         node @@ mode_set;
         node @@ div [node all_trustees];
         node @@ div [node t2];
         node @@ div [node b];
       ]

let rec show_draft_status uuid container =
  let@ () = show_in container in
  let* x = get_status uuid in
  match x with
  | Error e ->
     let msg = Printf.sprintf "Error: %s" (string_of_error e) in
     Lwt.return [txt msg]
  | Ok status ->
     let t = textarea () in
     t##.value := Js.string (string_of_status status);
     let b label r =
       let@ () = button label in
       let* x = post_with_token (string_of_status_request r) "drafts/%s/status" uuid in
       let@ () = show_in container in
       let msg =
         match x.code with
         | 200 -> "Success"
         | code -> Printf.sprintf "Error %d: %s" code x.content
       in
       let b = button "Proceed" (fun () -> show_draft_status uuid container) in
       Lwt.return [node @@ div [txt msg]; node @@ div [node b]]
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

let show_draft show_root show_all uuid draft title container tab =
  container##.innerHTML := Js.string "Loading...";
  let _, label = suffix_and_label_of_draft_tab tab in
  let* () =
    let@ () = show_in title in
    Lwt.return [txt label]
  in
  match tab with
  | `Draft -> show_draft_main show_root show_all uuid draft container
  | `Voters -> show_draft_voters uuid draft container
  | `Passwords -> show_draft_passwords uuid container
  | `Credentials -> show_draft_credentials uuid container
  | `Trustees -> show_draft_trustees uuid container
  | `Status -> show_draft_status uuid container

let a_draft_tab uuid tab =
  let suffix, label = suffix_and_label_of_draft_tab tab in
  let href = Printf.sprintf "#drafts/%s%s" uuid suffix in
  a ~href label

let show hash main =
  let show_root () =
    context := `None;
    show_root main
  in
  match hash with
  | `Error -> context := `None; show_error main
  | `Root -> show_root ()
  | `Draft (uuid, tab) ->
     begin
       let rec show_all () =
         let* x = get_draft uuid in
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
                  node @@ div [node @@ a ~href:"#" "All drafts"];
                  node @@ h1 [txt draft.draft_questions.t_name];
                  node @@ tabs;
                  node @@ title;
                  node @@ container;
                ]
            in
            context := `Draft (draft, title, container);
            show_draft show_root show_all uuid draft title container tab
       in
       match !context with
       | `Draft (draft, title, container) ->
          show_draft show_root show_all uuid draft title container tab
       | _ -> show_all ()
     end
  | `Credentials (uuid, _) -> context := `None; credentials_ui main uuid

let onhashchange () =
  let& main = document##getElementById (Js.string "main") in
  Lwt.async (fun () -> show (parse_hash ()) main);
  Js.Opt.return ()

let onload () =
  let& main = document##getElementById (Js.string "main") in
  Lwt.async
    (fun () ->
      let hash = parse_hash () in
      let* b =
        match hash with
        | `Root | `Draft _ -> get_api_token ()
        | `Credentials (_, token) -> api_token := token; Lwt.return_true
        | `Error -> Lwt.return_true
      in
      if b then (
        show hash main
      ) else (
        alert "Unable to retrieve API token. Please log in again.";
        Lwt.return_unit
      )
    );
  Js.Opt.return ()

let belenios =
  object%js
    method onload = onload ()
    method onhashchange = onhashchange ()
  end

let () = Js.export "BeleniosAdmin" belenios
