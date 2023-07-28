(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2022-2023 Inria, CNRS                                     *)
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
open Belenios_core.Common
open Belenios_api.Serializable_j
open Tyxml_js.Html5
open Belenios_js.Common
open Common

(* The hash in the url can be:
   - nothing or '#'. This is the "Home", that lists available elections
   - the uuid of an election
   - the "profile" string, for account management.
*)
let parse_hash () =
  let x = Js.to_string Dom_html.window##.location##.hash in
  if x = "" || x = "#" then `Home
  else
    let re = Regexp.regexp "^#([0-9A-Za-z]+)$" in
    let mat = Regexp.string_match re x 0 in
    match mat with
    | Some m -> (
        match Regexp.matched_group m 1 with
        | Some "profile" -> `Profile
        | Some uuid -> `Election (Uuid.wrap uuid)
        | _ -> `Home)
    | _ -> `Home

let header () =
  let open (val !Belenios_js.I18n.gettext) in
  let* title, description =
    match !where_am_i with
    | Election { uuid; status = Draft; _ } -> (
        let* x = get draft_of_string "drafts/%s" (Uuid.unwrap uuid) in
        match x with
        | Ok draft ->
            Lwt.return
              ( s_ "Setup: " ^ draft.draft_questions.t_name,
                draft.draft_questions.t_description )
        | Error _ -> Lwt.return ("Fail/Belenios administrator page", ""))
    | Election { uuid; status = Running | Tallied | Archived; _ } -> (
        let* x =
          get Yojson.Safe.from_string "elections/%s/election" (Uuid.unwrap uuid)
        in
        match x with
        | Ok election ->
            let name =
              match election with
              | `Assoc o -> (
                  match List.assoc_opt "name" o with
                  | Some (`String x) -> x
                  | _ -> "Invalid name")
              | _ -> "Fail when parsing election.json"
            in
            let description =
              match election with
              | `Assoc o -> (
                  match List.assoc_opt "description" o with
                  | Some (`String x) -> x
                  | _ -> "Invalid name")
              | _ -> "Fail when parsing election.json"
            in
            Lwt.return (name, description)
        | Error _ -> Lwt.return ("Fail/Belenios administrator page", ""))
    | _ -> Lwt.return (s_ "Belenios administrator page", "")
  in
  Lwt.return
    [
      div
        ~a:[ a_class [ "page-header__logo" ] ]
        [
          img
            ~a:
              [
                a_class [ "page-header__logo__image" ];
                a_title "Election server";
              ]
            ~alt:"Belenios logo" ~src:"../LOGO" ();
        ];
      div
        ~a:[ a_class [ "page-header__titles" ] ]
        [
          h1
            ~a:
              [
                a_class [ "page-header__titles__election-name" ];
                a_id "election_name";
              ]
            [ txt title ];
          p
            ~a:
              [
                a_class [ "page-header__titles__election-description" ];
                a_id "election_descr";
              ]
            [ txt description ];
        ];
      div ~a:[ a_class [ "page-header__right" ] ] [];
    ]

let newdraft () =
  let* x = Cache.get Cache.config in
  let* configuration_opt =
    match x with
    | Error e ->
        alert ("Failed to retrieve server config: " ^ e);
        Lwt.return None
    | Ok c -> Lwt.return @@ Some c
  in
  let* account_opt =
    let* x = get api_account_of_string "account" in
    match x with
    | Error e ->
        alert ("Failed to retrieve account info: " ^ string_of_error e);
        Lwt.return None
    | Ok c -> Lwt.return @@ Some c
  in
  Lwt.return
  @@
  match (configuration_opt, account_opt) with
  | Some c, Some a ->
      Some
        {
          draft_version = List.hd c.supported_crypto_versions;
          draft_owners = [ a.id ];
          draft_questions =
            {
              t_description = "";
              t_name = "";
              t_questions = [||];
              t_administrator = Some a.name;
              t_credential_authority = Some "server";
            };
          draft_languages = [ "en"; "fr" ];
          draft_contact = Some (Printf.sprintf "%s <%s>" a.name a.address);
          draft_booth = List.hd c.supported_booth_versions;
          draft_authentication =
            (match c.authentications with
            | [] | `Password :: _ -> `Password
            | `CAS :: _ -> `CAS ""
            | `Configured x :: _ -> `Configured x.configured_instance);
          draft_group = c.default_group;
        }
  | _ -> None

let choose_election_handler uuid status () =
  where_am_i := Election { uuid; status; tab = Title };
  Lwt.return_unit

let election_a2 x kind =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = x.summary_uuid in
  let elt =
    a
      ~href:("#" ^ Uuid.unwrap uuid)
      (if x.summary_name = "" then s_ "(no title)" else x.summary_name)
  in
  let r = Tyxml_js.To_dom.of_a elt in
  r##.onclick := lwt_handler (choose_election_handler uuid kind);
  div ~a:[ a_class [ "txt_with_a" ] ] [ elt ]

let list_draft () =
  let open (val !Belenios_js.I18n.gettext) in
  let* x = get summary_list_of_string "drafts" in
  let@ drafts = with_ok "drafts" x in
  drafts
  |> List.sort (fun a b -> compare b.summary_date a.summary_date)
  |> List.map (fun x -> li [ election_a2 x Draft ])
  |> fun xs -> Lwt.return [ h2 [ txt @@ s_ "Elections being setup:" ]; ul xs ]

let list_elec () =
  let open (val !Belenios_js.I18n.gettext) in
  let* x = get summary_list_of_string "elections" in
  match x with
  | Error e ->
      let msg =
        Printf.sprintf "An error occurred while retrieving elections: %s"
          (string_of_error e)
      in
      let x () = [ div [ txt msg ] ] in
      Lwt.return (x (), x ())
  | Ok elections ->
      let make title kind =
        List.filter
          (fun x -> match x.summary_kind with Some y -> kind y | _ -> false)
          elections
        |> List.sort (fun a b -> compare b.summary_date a.summary_date)
        |> List.map (fun x ->
               li
                 [
                   election_a2 x
                     (match x.summary_kind with
                     | Some `Validated -> Running
                     | Some `Archived -> Archived
                     | Some `Tallied -> Tallied
                     | _ -> assert false);
                 ])
        |> fun xs -> [ h2 [ txt title ]; ul xs ]
      in
      let elt1 =
        make (s_ "Running or finished elections:") (fun x ->
            x = `Validated || x = `Tallied)
      in
      let elt2 = make (s_ "Archived elections:") (fun x -> x = `Archived) in
      Lwt.return (elt1, elt2)

let rec page_body () =
  let open (val !Belenios_js.I18n.gettext) in
  let* main_zone =
    match !where_am_i with
    | List_draft -> list_draft ()
    | List_running ->
        let* a, _ = list_elec () in
        Lwt.return a
    | List_old ->
        let* _, a = list_elec () in
        Lwt.return a
    | _ -> Lwt.return [ div [ txt "Error: Should not print this" ] ]
  in
  let menus =
    [ List_draft; List_running; List_old ]
    |> List.map (fun x ->
           let active = x = !where_am_i in
           let classes = [ "main-menu__item"; "noselect" ] in
           let classes =
             if active then "active" :: classes else "clickable" :: classes
           in
           let attr = [ a_class [ String.concat " " classes ] ] in
           let title =
             div ~a:attr
               [
                 txt
                   (match x with
                   | List_draft -> s_ "My draft elections"
                   | List_running -> s_ "My running elections"
                   | List_old -> s_ "My archived elections"
                   | _ -> assert false);
               ]
           in
           let r = Tyxml_js.To_dom.of_div title in
           r##.onclick :=
             lwt_handler (fun () ->
                 where_am_i := x;
                 update_page_body ());
           let title =
             if active then
               [
                 div
                   ~a:[ a_class [ "positioned" ] ]
                   [ div ~a:[ a_class [ "main-menu__item-active" ] ] []; title ];
               ]
             else [ title ]
           in
           (div ~a:[ a_class [ "main-menu__doing" ] ] [] :: title)
           @ [ div ~a:[ a_class [ "main-menu__item-separator" ] ] [] ])
    |> List.flatten
  in
  let menus = div ~a:[ a_class [ "main-menu__item-separator" ] ] [] :: menus in
  let but_cr =
    button
      ~a:[ a_class [ "clickable" ] ]
      (s_ "Create a new election")
      (fun () ->
        let* x = get summary_list_of_string "drafts" in
        let ifmatch = compute_ifmatch string_of_summary_list x in
        let* dr = newdraft () in
        match dr with
        | None ->
            alert "Creation failed: could not get config from server";
            Lwt.return_unit
        | Some d -> (
            let dr = string_of_draft d in
            let* x =
              post_with_token ?ifmatch dr "drafts" |> wrap uuid_of_string
            in
            match x with
            | Ok uuid ->
                where_am_i := Election { uuid; status = Draft; tab = Title };
                Dom_html.window##.location##.hash
                := Js.string (Uuid.unwrap uuid);
                Lwt.return_unit
            | Error e ->
                alert ("Creation failed: " ^ string_of_error e);
                Lwt.return_unit))
  in
  let menus = div ~a:[ a_class [ "main-menu__button" ] ] [ but_cr ] :: menus in
  let main_menu = div ~a:[ a_id "main_menu"; a_class [ "main-menu" ] ] menus in
  Lwt.return
    [
      main_menu; div ~a:[ a_id "main_zone"; a_class [ "main-zone" ] ] main_zone;
    ]

and update_page_body () =
  let&&* container = document##getElementById (Js.string "main") in
  show_in container page_body

let home_handler () =
  where_am_i := List_draft;
  Dom_html.window##.location##.hash := Js.string "";
  Lwt.return_unit

let account_handler () =
  where_am_i := Profile;
  Dom_html.window##.location##.hash := Js.string "profile";
  Lwt.return_unit

let nav_menu () =
  let open (val !Belenios_js.I18n.gettext) in
  let elt1 =
    div
      ~a:[ a_class [ "nav-menu__item"; "clickable"; "noselect" ] ]
      [ txt @@ s_ "Home" ]
  in
  let r = Tyxml_js.To_dom.of_div elt1 in
  r##.onclick := lwt_handler home_handler;
  let elt2 = div ~a:[ a_class [ "nav-menu__item-blank"; "noselect" ] ] [] in
  let* ac = Cache.get Cache.account in
  let user =
    match ac with
    | Ok acc -> acc.name
    | Error msg ->
        alert msg;
        "[no_name]"
  in
  let elt3 =
    div
      ~a:[ a_class [ "nav-menu__item"; "clickable"; "noselect" ] ]
      [
        div ~a:[ a_id "nav_username" ] [ txt user ];
        img ~a:[ a_id "avatar" ] ~alt:"Avatar" ~src:"avatar.png" ();
      ]
  in
  let r = Tyxml_js.To_dom.of_div elt3 in
  r##.onclick := lwt_handler account_handler;
  Lwt.return [ elt1; elt2; elt3 ]

let footer () =
  (* TODO *)
  let open (val !Belenios_js.I18n.gettext) in
  [
    div
      ~a:[ a_class [ "page-footer" ] ]
      [ a ~href:"../admin" (s_ "Classical interface") ];
  ]

let show_root main =
  let open (val !Belenios_js.I18n.gettext) in
  main##.innerHTML := Js.string @@ s_ "Loading...";
  let@ () = show_in main in
  let* header = header () in
  let* page_body =
    match !where_am_i with Election _ -> Lwt.return [] | _ -> page_body ()
  in
  let* nav_menu = nav_menu () in
  Lwt.return
    [
      div
        ~a:[ a_class [ "page" ] ]
        [
          div ~a:[ a_class [ "page-header" ]; a_id "header" ] header;
          div ~a:[ a_class [ "nav-menu" ] ] nav_menu;
          div ~a:[ a_class [ "page-body" ]; a_id "main" ] page_body;
          div ~a:[ a_class [ "footer" ] ] (footer ());
        ];
      div
        ~a:[ a_id "popup" ]
        [
          div ~a:[ a_id "popup-background-filter" ] [];
          div ~a:[ a_id "popup-content" ] [];
        ];
    ]

let find_kind uuid =
  let* is_draft =
    let* x = get summary_list_of_string "drafts" in
    match x with
    | Error _ -> Lwt.return false
    | Ok drafts ->
        Lwt.return @@ List.exists (fun x -> x.summary_uuid = uuid) drafts
  in
  if is_draft then Lwt.return (Some Draft)
  else
    let* x = get summary_list_of_string "elections" in
    match x with
    | Error _ -> Lwt.return None
    | Ok elecs -> (
        let e = List.find_opt (fun x -> x.summary_uuid = uuid) elecs in
        match e with
        | None -> Lwt.return None
        | Some ee -> (
            match ee.summary_kind with
            | Some `Validated -> Lwt.return (Some Running)
            | Some `Tallied -> Lwt.return (Some Tallied)
            | Some `Archived -> Lwt.return (Some Archived)
            | _ ->
                Lwt.return
                  None (* should not occur, unless some race condition *)))

let onhashchange () =
  let open (val !Belenios_js.I18n.gettext) in
  let* () = Cache.sync_until_success () in
  Cache.invalidate_all ();
  let hash = parse_hash () in
  let* () = get_api_token hash in
  let* where =
    match hash with
    | `Home -> Lwt.return List_draft
    | `Profile -> Lwt.return Profile
    | `Election uuid -> (
        let* kind = find_kind uuid in
        match kind with
        | None ->
            alert @@ s_ "Unknown uuid";
            Lwt.return List_draft
        | Some status ->
            let tab = match status with Tallied -> Trustees | _ -> Title in
            Lwt.return @@ Election { uuid; status; tab })
  in
  where_am_i := where;
  let main = document##.body in
  let* () = show_root main in
  match where with
  | Election _ -> Elections.update_main ()
  | Profile -> Account.update_main ()
  | _ -> Lwt.return_unit

let extract_lang x =
  match String.index_opt x '-' with None -> x | Some i -> String.sub x 0 i

let onload () =
  let lang =
    Js.Optdef.case
      Dom_html.window##.navigator##.language
      (fun () -> "en")
      Js.to_string
    |> extract_lang
  in
  let* () = Belenios_js.I18n.init ~dir:"" ~component:"admin" ~lang in
  onhashchange ()

let () =
  Dom_html.window##.onload := lwt_handler onload;
  Dom_html.window##.onhashchange := lwt_handler onhashchange
