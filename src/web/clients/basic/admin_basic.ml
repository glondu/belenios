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
open Belenios
open Belenios_api.Serializable_j
open Belenios_api
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session

let context = ref `None

let draft_a (x : summary) =
  let uuid = Uuid.unwrap x.uuid in
  a ~href:("#drafts/" ^ uuid) (if x.name = "" then "(no title)" else x.name)

let election_a (x : summary) =
  let uuid = Uuid.unwrap x.uuid in
  a ~href:("#elections/" ^ uuid) (if x.name = "" then "(no title)" else x.name)

let regexps =
  [
    (Regexp.regexp "^#?$", fun _ -> `Root);
    ( Regexp.regexp
        "^#drafts/([0-9A-Za-z]+)(/(voters|passwords|credentials|trustees|status))?$",
      fun r ->
        match (Regexp.matched_group r 1, Regexp.matched_group r 3) with
        | Some uuid, None -> `Draft (Uuid.wrap uuid, `Draft)
        | Some uuid, Some "voters" -> `Draft (Uuid.wrap uuid, `Voters)
        | Some uuid, Some "passwords" -> `Draft (Uuid.wrap uuid, `Passwords)
        | Some uuid, Some "credentials" -> `Draft (Uuid.wrap uuid, `Credentials)
        | Some uuid, Some "trustees" -> `Draft (Uuid.wrap uuid, `Trustees)
        | Some uuid, Some "status" -> `Draft (Uuid.wrap uuid, `Status)
        | _ -> `Error );
    ( Regexp.regexp "^#drafts/([0-9A-Za-z]+)/credentials@([0-9A-Za-z]+)$",
      fun r ->
        match (Regexp.matched_group r 1, Regexp.matched_group r 2) with
        | Some uuid, Some token -> `Credentials (Uuid.wrap uuid, token)
        | _ -> `Error );
    ( Regexp.regexp "^#elections/([0-9A-Za-z]+)$",
      fun r ->
        match Regexp.matched_group r 1 with
        | Some uuid -> `Election (Uuid.wrap uuid)
        | _ -> `Error );
  ]

let parse_hash () =
  let x = Js.to_string Dom_html.window##.location##.hash in
  let r =
    List.find_map
      (fun (r, f) -> Option.map f @@ Regexp.string_match r x 0)
      regexps
  in
  Option.value r ~default:`Error

let show_error main =
  main##.innerHTML := Js.string "Error";
  Lwt.return_unit

let rec show_root main =
  let open (val !Belenios_js.I18n.gettext) in
  main##.innerHTML := Js.string "Loading...";
  let@ () = show_in main in
  let* configuration, configuration_opt =
    let* x = Api.(get ~notoken:true configuration) in
    let@ c, _ = with_ok_opt "configuration" x in
    Lwt.return [ txt (string_of_configuration c) ]
  in
  let* account, account_opt =
    let* x = Api.(get account) in
    let@ account, ifmatch = with_ok_opt "account" x in
    let account_str = string_of_api_account account in
    let t, tget = textarea ~rows:2 account_str in
    let b =
      let@ () = button "Save changes" in
      let* x = Api.(put ~ifmatch account (api_account_of_string (tget ()))) in
      let@ () = show_in main in
      let msg =
        match x.code with
        | 200 -> "Changes successfully applied!"
        | code -> Printf.sprintf "Error %d: %s" code x.content
      in
      let b = button "Proceed" (fun () -> show_root main) in
      Lwt.return [ div [ txt msg ]; div [ b ] ]
    in
    Lwt.return [ div [ t ]; div [ b ] ]
  in
  let* x = Api.(get drafts) in
  let@ drafts, ifmatch = with_ok "drafts" x in
  let* drafts =
    drafts
    |> List.sort (fun (a : summary) b -> compare b.date a.date)
    |> List.map (fun x -> li [ draft_a x ])
    |> fun xs -> Lwt.return [ ul xs ]
  in
  let* validated, tallied, archived =
    let* x = Api.(get elections) in
    match x with
    | Error e ->
        let msg =
          Printf.sprintf "An error occurred while retrieving elections: %s"
            (string_of_error e)
        in
        let x () = [ txt msg ] in
        Lwt.return (x (), x (), x ())
    | Ok (elections, _) ->
        let make f =
          List.filter (fun (x : summary) -> f x.state) elections
          |> List.sort (fun (a : summary) b -> compare b.date a.date)
          |> List.map (fun x -> li [ election_a x ])
          |> fun xs -> [ ul xs ]
        in
        let is_validated = function
          | `Open | `Closed | `Shuffling | `EncryptedTally -> true
          | _ -> false
        in
        let is_tallied x = x = `Tallied in
        let is_archived x = x = `Archived in
        Lwt.return (make is_validated, make is_tallied, make is_archived)
  in
  let template =
    match (configuration_opt, account_opt) with
    | Some c, Some a ->
        let draft_version = List.hd c.supported_crypto_versions in
        let (Version v) = Election.version_of_int draft_version in
        let draft_questions =
          {
            t_description = "";
            t_name = "";
            t_questions = [||];
            t_administrator = Some a.name;
            t_credential_authority = Some "server";
          }
        in
        let address =
          match a.address with None -> "" | Some x -> Printf.sprintf " <%s>" x
        in
        let draft =
          {
            draft_version;
            draft_owners = [ a.id ];
            draft_questions;
            draft_languages = [ "en"; "fr" ];
            draft_contact = Some (a.name ^ address);
            draft_booth = List.hd c.supported_booth_versions;
            draft_authentication =
              (match c.authentications with
              | [] | `Password :: _ -> `Password
              | `CAS :: _ -> `CAS ""
              | `Configured x :: _ -> `Configured x.configured_instance);
            draft_group = c.default_group;
          }
        in
        Some (Draft (v, draft))
    | _ -> None
  in
  let create =
    let value =
      match template with None -> "" | Some x -> string_of_draft x
    in
    let t, tget = textarea value in
    let b =
      let@ () = button "Create new draft" in
      let* x =
        Api.(post ~ifmatch drafts (draft_of_string (tget ())))
        |> wrap uuid_of_string
      in
      match x with
      | Ok uuid ->
          Dom_html.window##.location##.hash
          := Js.string ("#drafts/" ^ Uuid.unwrap uuid);
          Lwt.return_unit
      | Error e ->
          let@ () = show_in main in
          let msg =
            Printf.ksprintf txt "An error occurred while creating the draft: %s"
              (string_of_error e)
          in
          let b = button "Proceed" (fun () -> show_root main) in
          Lwt.return [ div [ msg ]; div [ b ] ]
    in
    div [ div [ t ]; div [ b ] ]
  in
  Lwt.return
    [
      h1 [ txt "Server configuration" ];
      div configuration;
      h1 [ txt "My account" ];
      div account;
      h1 [ txt @@ s_ "Elections being prepared" ];
      div drafts;
      h1 [ txt @@ s_ "Elections you can administer" ];
      div validated;
      h1 [ txt @@ s_ "Tallied elections" ];
      div tallied;
      h1 [ txt @@ s_ "Archived elections" ];
      div archived;
      h1 [ txt "Create new draft" ];
      create;
    ]

let show hash main =
  match hash with
  | `Error ->
      context := `None;
      show_error main
  | `Root ->
      context := `None;
      show_root main
  | `Draft (uuid, tab) -> Drafts.show main uuid tab context
  | `Election uuid ->
      context := `None;
      Elections.show main uuid
  | `Credentials (uuid, _) ->
      context := `None;
      Credentials.show main uuid

let onhashchange () =
  let hash = parse_hash () in
  let* () = init_api_token ~ui:"basic" hash in
  show hash document##.body

let onload () =
  let lang =
    Js.Optdef.case
      Dom_html.window##.navigator##.language
      (fun () -> "en")
      Js.to_string
  in
  let* () = Belenios_js.I18n.init ~dir:"" ~component:"admin" ~lang in
  let hash = parse_hash () in
  let* () = init_api_token ~ui:"basic" hash in
  show hash document##.body

let () =
  relative_root := "../";
  Dom_html.window##.onload := lwt_handler onload;
  Dom_html.window##.onhashchange := lwt_handler onhashchange
