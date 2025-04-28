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
open Belenios
open Belenios_web_api
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session
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

let header config =
  let open (val !Belenios_js.I18n.gettext) in
  let* title, description =
    match !where_am_i with
    | Election { uuid; status = Draft; _ } -> (
        let* x = Api.(get (draft uuid) `Nobody) in
        match x with
        | Ok (Draft (_, draft), _) ->
            Lwt.return
              ( s_ "Setup: " ^ draft.draft_questions.t_name,
                draft.draft_questions.t_description )
        | Error _ ->
            Lwt.return (config.vendor ^^^ s_ "Administration" ^^^ "Error", ""))
    | Election { uuid; status = Running | Tallied | Archived; _ } -> (
        let* x = Api.(get (election uuid) `Nobody) in
        match x with
        | Ok (election, _) ->
            let election = Yojson.Safe.from_string election in
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
        | Error _ ->
            Lwt.return (config.vendor ^^^ s_ "Administration" ^^^ "Error", ""))
    | _ -> Lwt.return (config.vendor ^^^ s_ "Administration", "")
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
            ~alt:"Logo" ~src:"LOGO" ();
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
    let* x = Api.(get account !user) in
    match x with
    | Error e ->
        alert ("Failed to retrieve account info: " ^ string_of_error e);
        Lwt.return None
    | Ok (c, _) -> Lwt.return @@ Some c
  in
  Lwt.return
  @@
  match (configuration_opt, account_opt) with
  | Some c, Some a ->
      let address =
        match a.address with None -> "" | Some x -> Printf.sprintf " <%s>" x
      in
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
          draft_contact = Some (a.name ^ address);
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

let election_a2 (x : summary) status =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = x.uuid in
  let elt =
    a
      ~href:("#" ^ Uuid.unwrap uuid)
      (if x.name = "" then s_ "(no title)" else x.name)
  in
  let r = Tyxml_js.To_dom.of_a elt in
  r##.onclick := lwt_handler (choose_election_handler uuid status);
  div ~a:[ a_class [ "txt_with_a" ] ] [ elt ]

let list_draft () =
  let open (val !Belenios_js.I18n.gettext) in
  let* x = Api.(get elections !user) in
  let@ drafts, _ = with_ok "elections" x in
  drafts
  |> List.filter (fun (a : summary) -> a.state = `Draft)
  |> List.sort (fun (a : summary) b -> compare b.date a.date)
  |> List.map (fun x -> li [ election_a2 x Draft ])
  |> fun xs -> Lwt.return [ h2 [ txt @@ s_ "Elections being setup:" ]; ul xs ]

let status_of_state = function
  | `Draft -> Draft
  | `Open | `Closed | `Shuffling | `EncryptedTally -> Running
  | `Archived -> Archived
  | `Tallied -> Tallied

let list_elec () =
  let open (val !Belenios_js.I18n.gettext) in
  let* x = Api.(get elections !user) in
  match x with
  | Error e ->
      let msg =
        Printf.sprintf "An error occurred while retrieving elections: %s"
          (string_of_error e)
      in
      let x () = [ div [ txt msg ] ] in
      Lwt.return (x (), x ())
  | Ok (elections, _) ->
      let make title f =
        List.filter (fun (x : summary) -> f x.state) elections
        |> List.sort (fun (a : summary) b -> compare b.date a.date)
        |> List.map (fun (x : summary) ->
               li [ election_a2 x (status_of_state x.state) ])
        |> fun xs -> [ h2 [ txt title ]; ul xs ]
      in
      let elt1 =
        make (s_ "Running or finished elections:") (function
          | `Open | `Closed | `Shuffling | `EncryptedTally | `Tallied -> true
          | _ -> false)
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
      ~a:[ a_id "create_new_election"; a_class [ "clickable" ] ]
      (s_ "Create a new election")
      (fun () ->
        let* x = Api.(get elections !user) in
        let ifmatch = get_ifmatch x in
        let (Version v) = default_version in
        let* dr = newdraft () in
        match dr with
        | None ->
            alert "Creation failed: could not get config from server";
            Lwt.return_unit
        | Some d -> (
            let* x =
              Api.(post ?ifmatch elections !user (Draft (v, d)))
              |> wrap uuid_of_string
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
  let classes = a_class [ "nav-menu__item"; "clickable"; "noselect" ] in
  let elt3 =
    let r =
      div ~a:[ classes ]
        [
          div ~a:[ a_id "nav_username" ] [ txt user ];
          img ~a:[ a_id "avatar" ] ~alt:"Avatar" ~src:"static/avatar.png" ();
        ]
    in
    let dom = Tyxml_js.To_dom.of_div r in
    dom##.onclick := lwt_handler account_handler;
    r
  in
  let elt4 =
    let r = div ~a:[ a_id "logout_direct"; classes ] [ txt @@ s_ "Log out" ] in
    let dom = Tyxml_js.To_dom.of_div r in
    dom##.onclick := lwt_handler logout;
    r
  in
  Lwt.return [ elt1; elt2; elt3; elt4 ]

let footer configuration =
  let module UiBase = struct
    module Xml = Tyxml_js.Xml
    module Svg = Tyxml_js.Svg
    module Html = Tyxml_js.Html

    let uris = configuration.uris
  end in
  let module Ui = Belenios_ui.Pages_common.Make (UiBase) in
  let l = !Belenios_js.I18n.gettext in
  let { restricted_mode; _ } = configuration in
  let footer =
    div
      ~a:[ a_class [ "page-footer" ] ]
      (Ui.footer_fragment l ~restricted_mode ())
  in
  [ footer ]

let show_root main =
  let open (val !Belenios_js.I18n.gettext) in
  main##.textContent := Js.some @@ Js.string @@ s_ "Loading...";
  let@ config cont =
    let* x = Cache.(get config) in
    match x with
    | Ok x -> cont x
    | Error x ->
        main##.textContent :=
          Js.some @@ Js.string
          @@ Printf.sprintf
               (f_ "Could not get configuration from server! (%s)")
               x;
        Lwt.return_unit
  in
  let@ () = show_in main in
  let* header = header config in
  let* page_body =
    match !where_am_i with Election _ -> Lwt.return [] | _ -> page_body ()
  in
  let* nav_menu = nav_menu () in
  let banner = div ~a:[ a_id "banner" ] [] in
  let () =
    let@ () = Lwt.async in
    let url = Printf.sprintf "banner?lang=%s" lang in
    let* x = Js_of_ocaml_lwt.XmlHttpRequest.get url in
    match x.code with
    | 200 ->
        let dom = Tyxml_js.To_dom.of_div banner in
        dom##.innerHTML := Js.string x.content;
        Lwt.return_unit
    | _ -> Lwt.return_unit
  in
  Lwt.return
    [
      div
        ~a:[ a_class [ "page" ] ]
        [
          div ~a:[ a_class [ "page-header" ]; a_id "header" ] header;
          banner;
          div ~a:[ a_class [ "nav-menu" ] ] nav_menu;
          div ~a:[ a_class [ "page-body" ]; a_id "main" ] page_body;
          div ~a:[ a_class [ "footer" ] ] (footer config);
        ];
      div
        ~a:[ a_id "popup" ]
        [
          div ~a:[ a_id "popup-background-filter" ] [];
          div ~a:[ a_id "popup-content" ] [];
        ];
    ]

let find_status uuid =
  let* x = Api.(get elections !user) in
  match x with
  | Error _ -> Lwt.return None
  | Ok (elecs, _) -> (
      let e = List.find_opt (fun (x : summary) -> x.uuid = uuid) elecs in
      match e with
      | None -> Lwt.return None
      | Some ee -> Lwt.return_some @@ status_of_state ee.state)

let onhashchange () =
  let open (val !Belenios_js.I18n.gettext) in
  let* () = Cache.sync_until_success () in
  Cache.invalidate_all ();
  let hash = parse_hash () in
  let* () =
    init_api_token
      (fun t ->
        token := Some t;
        user := `Admin t)
      hash
  in
  let* () =
    let* account = Cache.(get account) in
    match account with
    | Ok { language; _ } -> Belenios_js.I18n.set ~language
    | Error _ -> Lwt.return_unit
  in
  let* where =
    match hash with
    | `Home -> Lwt.return List_draft
    | `Profile -> Lwt.return Profile
    | `Election uuid -> (
        let* status = find_status uuid in
        match status with
        | None ->
            alert @@ s_ "Unknown uuid";
            Lwt.return List_draft
        | Some status ->
            let tab = match status with Tallied -> Status | _ -> Title in
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
  let* () = Belenios_js.I18n.init ~dir:"static/" ~component:"admin" ~lang in
  onhashchange ()

let () =
  Dom_html.window##.onload := lwt_handler onload;
  Dom_html.window##.onhashchange := lwt_handler onhashchange
