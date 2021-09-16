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

let lwt_handler f =
  Dom_html.handler (fun _ -> Lwt.async f; Js._true)

let replace_content container content =
  container##.innerHTML := Js.string "";
  Dom.appendChild container content

let textarea () =
  let r = textarea [] in
  r##.cols := 80;
  r##.rows := 10;
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

let get_drafts () =
  get_with_token "drafts" |> wrap summary_list_of_string

let post_drafts draft =
  post_with_token draft "drafts" |> wrap uuid_of_string

let get_draft uuid =
  get_with_token "drafts/%s" uuid |> wrap draft_of_string

let get_voters uuid =
  get_with_token "drafts/%s/voters" uuid |> wrap voter_list_of_string

let put_voters uuid voters =
  put_with_token voters "drafts/%s/voters" uuid

let get_passwords uuid =
  get_with_token "drafts/%s/passwords" uuid |> wrap voter_list_of_string

let get_credentials uuid =
  get_with_token "drafts/%s/credentials" uuid |> wrap credentials_of_string

let perform_draft container uuid action msg handler =
  let* x = action (format_of_string "drafts/%s") uuid in
  let msg =
    let open Js_of_ocaml_lwt.XmlHttpRequest in
    match x.code with
    | 200 -> div [txt msg]
    | code -> div [Printf.ksprintf txt "Error %d: %s" code x.content]
  in
  let b = button "Proceed" handler in
  let content = div [node @@ msg; node @@ b] in
  replace_content container content;
  Lwt.return_unit

let delete_draft container uuid =
  perform_draft container uuid
    delete_with_token "Draft successfully deleted!"
    (fun () -> Dom_html.window##.location##.hash := Js.string "#"; Lwt.return_unit)

let rec save_draft main container uuid x =
  perform_draft container uuid
    (fun url -> put_with_token x url) "Changes successfully applied!"
    (fun () -> load_draft main uuid)

and save_voters main container uuid voters =
  let* x = put_voters uuid voters in
  let msg =
    match x.code with
    | 200 -> "Voters successfully updated!"
    | code -> Printf.sprintf "Error %d: %s" code x.content
  in
  let b = button "Proceed" (fun () -> load_draft main uuid) in
  let content = div [node @@ div [txt msg]; node @@ b] in
  replace_content container content;
  Lwt.return_unit

and post_passwords main container uuid voters =
  let* x = post_with_token voters "drafts/%s/passwords" uuid in
  let msg =
    match x.code with
    | 200 -> "Passwords successfully generated and sent!"
    | code -> Printf.sprintf "Error %d: %s" code x.content
  in
  let b = button "Proceed" (fun () -> load_draft main uuid) in
  let content = div [node @@ div [txt msg]; node @@ b] in
  replace_content container content;
  Lwt.return_unit

and generate_credentials_on_server main container uuid =
  let op = string_of_credential_operation `GenerateOnServer in
  let* x = post_with_token op "drafts/%s/credentials" uuid in
  let msg =
    match x.code with
    | 200 -> "Credentials successfully generated!"
    | code -> Printf.sprintf "Error %d: %s" code x.content
  in
  let b = button "Proceed" (fun () -> load_draft main uuid) in
  let content = div [node @@ div [txt msg]; node @@ b] in
  replace_content container content;
  Lwt.return_unit

and load_draft main uuid =
  let* content =
    let container = div [] in
    let* title, draft =
      let* x = get_draft uuid in
      match x with
      | Error e ->
         let msg =
           Printf.sprintf "An error occurred while retrieving draft %s: %s"
             uuid (string_of_error e)
         in
         Lwt.return ("Error", div [txt msg])
      | Ok x ->
         let t = textarea () in
         t##.value := Js.string (string_of_draft x);
         let save_button =
           let@ () = button "Save changes" in
           save_draft main container uuid (Js.to_string t##.value)
         in
         let delete_button =
           let@ () = button "Delete draft" in
           if confirm "Are you sure?" then
             delete_draft main uuid
           else
             Lwt.return_unit
         in
         let index_link = a ~href:"#" "All drafts" in
         Lwt.return
           (
             x.draft_questions.t_name,
             div [
                 node @@ h2 [txt "Draft"];
                 node @@ div [node @@ t];
                 node @@ div [node @@ save_button; txt " "; node @@ index_link];
                 node @@ div [node @@ delete_button];
               ]
           )
    in
    let* voters, all_voters =
      let* x = get_voters uuid in
      match x with
      | Error e ->
         let msg =
           Printf.sprintf
             "An error occurred while retrieving voters of draft %s: %s"
             uuid (string_of_error e)
         in
         Lwt.return (div [txt msg], [])
      | Ok x ->
         let t = textarea () in
         t##.value := Js.string (string_of_voter_list x);
         let b =
           let@ () = button "Update voter list" in
           save_voters main container uuid (Js.to_string t##.value)
         in
         Lwt.return
           (div [
                node @@ h2 [txt "Voter list"];
                node @@ div [node @@ t];
                node @@ div [node @@ b];
              ],
            x)
    in
    let* passwords =
      let* x = get_passwords uuid in
      match x with
      | Error e ->
         let msg =
           Printf.sprintf
             "An error occurred while retrieving passwords of draft %s: %s"
             uuid (string_of_error e)
         in
         Lwt.return @@ div [txt msg]
      | Ok x ->
         let missing =
           let x = List.fold_left (fun accu v -> SSet.add v accu) SSet.empty x in
           List.filter (fun v -> not @@ SSet.mem v x) all_voters
         in
         let t_current = textarea () in
         t_current##.value := Js.string (string_of_voter_list x);
         let t_post = textarea () in
         t_post##.value := Js.string (string_of_voter_list missing);
         let b =
           let@ () = button "Generate and send passwords" in
           post_passwords main container uuid (Js.to_string t_post##.value)
         in
         Lwt.return
         @@ div [
                node @@ h2 [txt "Passwords"];
                node @@ div [node @@ t_current];
                node @@ div [node @@ t_post];
                node @@ div [node @@ b];
              ]
    in
    let* credentials =
      let* body =
        let* x = get_credentials uuid in
        match x with
        | Error e ->
           let msg =
             Printf.sprintf
               "An error occurred while retrieving credentials of draft %s: %s"
               uuid (string_of_error e)
           in
           Lwt.return @@ div [txt msg]
        | Ok x ->
           match x.credentials_public, x.credentials_token with
           | None, None ->
              let b =
                let@ () = button "Generate on server" in
                generate_credentials_on_server main container uuid
              in
              Lwt.return @@ div [node @@ b]
           | None, Some token ->
              let link = Js.to_string Dom_html.window##.location##.href ^ "/credentials@" ^ token in
              Lwt.return
              @@ div [
                     txt "Send the following link to the credential authority:";
                     txt " ";
                     txt link;
                   ]
           | Some _, _ ->
              let t = textarea () in
              t##.value := Js.string (string_of_credentials x);
              Lwt.return @@ div [node @@ t]
      in
      Lwt.return
      @@ div [
             node @@ h2 [txt "Credentials"];
             node @@ body;
           ]
    in
    Dom.appendChild container draft;
    Dom.appendChild container voters;
    Dom.appendChild container passwords;
    Dom.appendChild container credentials;
    Lwt.return @@ node
    @@ div [
           node @@ h1 [txt title];
           node @@ container;
         ]
  in
  replace_content main content;
  Lwt.return_unit

module CG = Belenios_core.Credential.MakeGenerate (LwtJsRandom)

let credentials_ui main uuid =
  let* content =
    let* x = get_draft uuid in
    match x with
    | Error e ->
       let msg =
         Printf.sprintf
           "An error occurred while retrieving draft %s: %s"
           uuid (string_of_error e)
       in
       Lwt.return
       @@ div [
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
              let op = `SubmitPublic public_creds |> string_of_credential_operation in
              let t = textarea () in
              t##.value := Js.string (String.concat "\n" private_creds ^ "\n");
              let b =
                let@ () = button "Send public credentials to server" in
                let* x = post_with_token op "drafts/%s/credentials" uuid in
                let msg =
                  match x.code with
                  | 200 -> "Public credentials successfully uploaded!"
                  | code -> Printf.sprintf "Error %d: %s" code x.content
                in
                let content = div [txt msg] in
                replace_content container content;
                Lwt.return_unit
              in
              let content =
                div [
                    node @@ div [node @@ t];
                    node @@ div [node @@ b];
                  ]
              in
              replace_content container content;
              Lwt.return_unit
            in
            Dom.appendChild container b;
            Lwt.return
            @@ div [
                   node @@ div [txt fingerprint];
                   node @@ container;
                 ]
       in
       Lwt.return
       @@ div [
              node @@ h1 [txt (Printf.sprintf "Credentials for %s" draft.draft_questions.t_name)];
              node @@ voters;
            ]
  in
  replace_content main content;
  Lwt.return_unit

let draft_a x =
  let uuid = raw_string_of_uuid x.summary_uuid in
  a ~href:("#drafts/" ^ uuid) x.summary_name

let draft_regexp = Regexp.regexp "^#drafts/([0-9A-Za-z]+)$"
let credentials_regexp = Regexp.regexp "^#drafts/([0-9A-Za-z]+)/credentials@([0-9A-Za-z]+)$"

let parse_hash () =
  let x = Js.to_string Dom_html.window##.location##.hash in
  if x = "" || x = "#" then
    `Root
  else
    match Regexp.string_match draft_regexp x 0 with
    | Some r ->
       begin
         match Regexp.matched_group r 1 with
         | None -> `Unknown
         | Some uuid -> `Draft uuid
       end
    | None ->
       match Regexp.string_match credentials_regexp x 0 with
       | Some r ->
          begin
            match Regexp.matched_group r 1, Regexp.matched_group r 2 with
            | Some uuid, Some token -> `Credentials (uuid, token)
            | _ -> `Unknown
          end
       | None -> `Unknown

let rec onhashchange_inner hash main =
  main##.innerHTML := Js.string "Loading...";
  match hash with
  | `Unknown ->
     let content = div [txt "Unknown hash"] in
     replace_content main content;
     Lwt.return_unit
  | `Root ->
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
     let create_textarea = textarea () in
     let create_button =
       let@ () = button "Create new draft" in
       let* x = post_drafts (Js.to_string create_textarea##.value) in
       match x with
       | Ok uuid ->
          Dom_html.window##.location##.hash := Js.string ("#drafts/" ^ raw_string_of_uuid uuid);
          Lwt.return_unit
       | Error e ->
          let msg =
            Printf.ksprintf txt
              "An error occurred while creating the draft: %s"
              (string_of_error e)
          in
          let b = button "Continue" (fun () -> onhashchange_inner (parse_hash ()) main) in
          let content =
            div [
                node @@ div [msg];
                node @@ div [node @@ b];
              ]
          in
          replace_content main content;
          Lwt.return_unit
     in
     let create =
       div [
           node @@ div [node @@ create_textarea];
           node @@ div [node @@ create_button];
         ]
     in
     let content =
       div [
           node @@ h1 [txt "My draft elections"];
           node @@ drafts;
           node @@ h1 [txt "Create new draft"];
           node @@ create;
         ]
     in
     replace_content main content;
     Lwt.return_unit
  | `Draft uuid -> load_draft main uuid
  | `Credentials (uuid, _) -> credentials_ui main uuid

let onhashchange () =
  let& main = document##getElementById (Js.string "main") in
  Lwt.async (fun () -> onhashchange_inner (parse_hash ()) main);
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
        | `Unknown -> Lwt.return_true
      in
      if b then (
        onhashchange_inner hash main
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
