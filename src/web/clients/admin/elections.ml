(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria, CNRS                                     *)
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
open Belenios
open Belenios_api.Serializable_j
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session
open Common

(* Syntaxic helper for Js.Optdef...
 * Warning: this is lazy and if something goes wrong, what follows the
 * failed assignement will be ignored.
 *)
let ( let^ ) x f = Js.Optdef.case x (fun () -> Lwt.return_unit) f

let read_full file =
  let t, u = Lwt.task () in
  let reader = new%js File.fileReader in
  reader##.onload :=
    Dom.handler (fun _ ->
        let () =
          let$ text = File.CoerceTo.string reader##.result in
          Lwt.wakeup_later u text
        in
        Js._false);
  reader##readAsText file;
  t

(* FIXME: get timezone offset from browser *)
let datestring_of_float x =
  let x = new%js Js.date_fromTimeValue (x *. 1000.) in
  let res = Js.to_string x##toISOString in
  String.sub res 0 (String.length res - 5)

(* forward declaration of the main function *)
let update_election_main = ref (fun () -> assert false)

(* open a popup that allows to choose an election uuid from which to
 * import something. handler takes an uuid (as raw_string) in input
 *)

let popup_choose_elec handler () =
  let open (val !Belenios_js.I18n.gettext) in
  let* x = get summary_list_of_string "elections" in
  match x with
  | Error e ->
      let msg =
        Printf.sprintf
          (f_ "An error occurred while retrieving elections: %s")
          (string_of_error e)
      in
      alert msg;
      Lwt.return_unit
  | Ok (elections, _) ->
      let name_uuids =
        elections
        |> List.filter (fun x ->
               match x.state with
               | `Open | `Closed | `Shuffling | `EncryptedTally | `Tallied ->
                   true
               | `Draft | `Archived -> false)
        |> List.map (fun (x : summary) ->
               let but =
                 button
                   (x.name ^ " (" ^ Uuid.unwrap x.uuid ^ ")")
                   (fun () ->
                     let* () =
                       let&&* d =
                         document##getElementById (Js.string "popup")
                       in
                       Lwt.return (d##.style##.display := Js.string "none")
                     in
                     handler (Uuid.unwrap x.uuid))
               in
               li [ but ])
      in
      (* FIXME *)
      let cancel_but =
        button (s_ "Cancel") (fun () ->
            let* () =
              let&&* d = document##getElementById (Js.string "popup") in
              Lwt.return (d##.style##.display := Js.string "none")
            in
            Lwt.return_unit)
      in
      let content =
        [
          div
            [
              txt
              @@ s_
                   "Please select the election from which you want to import \
                    data:";
            ];
          ul name_uuids;
          cancel_but;
        ]
      in
      let* () =
        let&&* container =
          document##getElementById (Js.string "popup-content")
        in
        show_in container (fun () -> Lwt.return content)
      in
      let* () =
        let&&* d = document##getElementById (Js.string "popup") in
        Lwt.return (d##.style##.display := Js.string "block")
      in
      Lwt.return_unit

(* FIXME: put a proper regex, here *)
let is_valid_url s = s <> ""

(* Ready means that it can be created. *)
let is_ready () =
  let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
  let* status = Cache.get_until_success Cache.status in
  let b =
    draft.draft_questions.t_name <> ""
    && draft.draft_contact <> None
    && draft.draft_contact <> Some ""
    && draft.draft_questions.t_questions <> [||]
    && status.num_voters > 0 && status.voter_authentication_visited
    && (draft.draft_authentication <> `Password
       || status.passwords_ready = Some true)
    && status.credential_authority_visited
    && status.credentials_ready = true
    && draft.draft_questions.t_credential_authority <> None
    && (draft.draft_questions.t_credential_authority <> Some "server"
       || status.private_credentials_downloaded = Some true)
    && status.trustees_ready = true
    && status.trustees_setup_step > 1
    && draft.draft_questions.t_administrator <> Some ""
    && draft.draft_questions.t_administrator <> None
  in
  Lwt.return b

let nb_shufflers () =
  let uuid = get_current_uuid () in
  let* x = Trustees_tab.get_shuffles uuid in
  match x with
  | None -> Lwt.return 0
  | Some x -> Lwt.return @@ List.length x.shuffles_shufflers

let default_handler tab () =
  let open (val !Belenios_js.I18n.gettext) in
  (* Before syncing, check if draft is consistent *)
  let* ok =
    if is_draft () && Cache.modified Cache.draft then
      let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
      match draft.draft_authentication with
      | `CAS s when not (is_valid_url s) ->
          alert
          @@ s_
               "Selecting CAS authentication requires setting a valid CAS \
                server";
          Lwt.return false
      | _ -> Lwt.return true
    else Lwt.return true
  in
  if ok then (
    let* res = Cache.sync () in
    match res with
    | Error msg -> popup_failsync msg
    | Ok () ->
        let uuid, status =
          match !where_am_i with
          | Election { uuid; status; _ } -> (uuid, status)
          | _ -> (Uuid.dummy, Draft)
        in
        where_am_i := Election { uuid; status; tab };
        !update_election_main ())
  else Lwt.return_unit

(* list of subpages available in the menu:
 * they are identified with a name
 * and associated to them is the following data
 *     - string to print in the menu (internationalized)
 *     - CSS id of the element to click to activate the tab
 *     - its status (done, doing, todo...)
 *     - the onclick handler, if any
 *)

type tab_status = {
  title : string;
  id : string;
  status : [ `DDone | `Doing | `Done | `Todo | `Wip | `None ];
  handler : (unit -> unit Lwt.t) option;
}

let tabs x =
  let open (val !Belenios_js.I18n.gettext) in
  let is_draft = is_draft () in
  let is_running = is_running () in
  let is_finished = is_finished () in
  let curr_tab =
    match !where_am_i with Election { tab; _ } -> tab | _ -> Title
  in
  match x with
  | Title ->
      let* status =
        if is_draft then
          if curr_tab = x then Lwt.return `Doing
          else
            let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
            Lwt.return
              (if draft.draft_questions.t_name = "" then `Todo else `Done)
        else Lwt.return `DDone
      in
      {
        title = s_ "Title";
        id = "tab_title";
        status;
        handler = Some (default_handler x);
      }
      |> Lwt.return
  | Questions ->
      let* status =
        if is_draft then
          if curr_tab = x then Lwt.return `Doing
          else
            let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
            Lwt.return
              (if draft.draft_questions.t_questions = [||] then `Todo else `Done)
        else Lwt.return `DDone
      in
      {
        title = s_ "Questions";
        id = "tab_questions";
        status;
        handler = Some (default_handler x);
      }
      |> Lwt.return
  | Voters ->
      let* status =
        if is_draft then
          if curr_tab = x then Lwt.return `Doing
          else
            let* voter_list = Cache.get_until_success Cache.voters in
            Lwt.return (if voter_list = [] then `Todo else `Done)
        else Lwt.return `DDone
      in
      {
        title = s_ "Voter list";
        id = "tab_voters";
        status;
        handler = Some (default_handler x);
      }
      |> Lwt.return
  | Dates ->
      let status =
        if is_draft then if curr_tab = x then `Doing else `Done else `DDone
      in
      {
        title = s_ "Dates";
        id = "tab_dates";
        status;
        handler =
          (if is_draft || is_running then Some (default_handler x) else None);
      }
      |> Lwt.return
  | Language ->
      let status =
        if is_draft then if curr_tab = x then `Doing else `Done else `DDone
      in
      {
        title = s_ "Languages";
        id = "tab_languages";
        status;
        handler = (if is_draft then Some (default_handler x) else None);
      }
      |> Lwt.return
  | Contact ->
      let status =
        if is_draft then if curr_tab = x then `Doing else `Done else `DDone
      in
      {
        title = s_ "Contact";
        id = "tab_contact";
        status;
        handler = (if is_draft then Some (default_handler x) else None);
      }
      |> Lwt.return
  | Trustees ->
      let* status =
        if is_finished then Lwt.return `DDone
        else if is_draft then
          if curr_tab = x then Lwt.return `Doing
          else
            let* status = Cache.get_until_success Cache.status in
            if status.trustees_ready && status.trustees_setup_step > 1 then
              Lwt.return `Done
            else Lwt.return `Todo
        else
          let* status = Cache.get_until_success Cache.e_status in
          match status.status_state with
          | `Open | `Closed | `Tallied -> Lwt.return `DDone
          | _ ->
              Lwt.return
                (if curr_tab = x then `Doing
                 else `Todo (* TODO: need some info from server *))
      in
      let* handler = Lwt.return_some (default_handler x) in
      { title = s_ "Decryption trustees"; id = "tab_trustees"; status; handler }
      |> Lwt.return
  | CredAuth ->
      let* status =
        if is_draft then
          if curr_tab = x then Lwt.return `Doing
          else
            let* status = Cache.get_until_success Cache.status in
            if status.credentials_ready then
              if
                match status.private_credentials_downloaded with
                | None -> true
                | Some b -> b
              then Lwt.return `Done
              else Lwt.return `Todo
            else
              match status.credentials_left with
              | None -> Lwt.return `Todo
              | Some _ -> Lwt.return `Wip
        else Lwt.return `DDone
      in
      {
        title = s_ "Credential authority";
        id = "tab_credentials";
        status;
        handler = (if is_draft then Some (default_handler x) else None);
      }
      |> Lwt.return
  | VotersPwd ->
      let* status =
        if is_draft then
          if curr_tab = x then Lwt.return `Doing
          else
            let* status = Cache.get_until_success Cache.status in
            if
              status.voter_authentication_visited
              && (status.passwords_ready = None
                 || status.passwords_ready = Some true)
            then Lwt.return `Done
            else Lwt.return `Todo
        else
          let* status = Cache.get_until_success Cache.e_status in
          match status.status_authentication with
          | Some `Password -> Lwt.return `None
          | _ -> Lwt.return `DDone
      in
      let handler =
        if is_draft || status = `None then Some (default_handler x) else None
      in
      {
        title = s_ "Voter's authentication";
        id = "tab_authentication";
        status;
        handler;
      }
      |> Lwt.return
  | ElectionPage ->
      let title =
        if is_draft then s_ "Preview"
        else if is_finished then s_ "Results page"
        else s_ "Election main page"
      in
      let* handler =
        let* b =
          if not is_draft then Lwt.return true
          else
            let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
            Lwt.return
              (draft.draft_questions.t_questions <> [||]
              && draft.draft_questions.t_name <> "")
        in
        if b then
          Lwt.return_some
          @@
          if is_archived () then default_handler x
          else if is_draft then fun () ->
            let* res = Cache.sync () in
            match res with
            | Error msg -> popup_failsync msg
            | Ok () -> Preview.preview_booth ()
          else fun () -> Preview.goto_mainpage ()
        else Lwt.return_none
      in
      { title; id = "tab_page"; status = `None; handler } |> Lwt.return
  | CreateOpenClose ->
      let title =
        if is_draft then s_ "Create the election" else s_ "Open / Close"
      in
      let status = match curr_tab = x with true -> `Doing | false -> `None in
      let* handler =
        let* b =
          if is_running then
            let* status = Cache.get_until_success Cache.e_status in
            Lwt.return
              (status.status_state == `Open || status.status_state == `Closed)
          else if is_draft then is_ready ()
          else Lwt.return false
        in
        if b then Lwt.return_some (default_handler x) else Lwt.return_none
      in
      { title; id = "tab_openclose"; status; handler } |> Lwt.return
  | Tally ->
      let* handler =
        let* b =
          if is_draft || is_finished then Lwt.return false
          else
            let* status = Cache.get_until_success Cache.e_status in
            Lwt.return (status.status_state = `Closed)
        in
        if b then (
          Lwt.return_some @@ fun () ->
          let uuid = get_current_uuid () in
          let confirm =
            confirm @@ s_ "Are you sure you want to tally this election?"
          in
          if not confirm then Lwt.return_unit
          else
            let* status = Cache.get_until_success Cache.e_status in
            let ifmatch = sha256_b64 @@ string_of_election_status status in
            let ifmatch = Some ifmatch in
            let* x =
              post_with_token ?ifmatch
                (string_of_admin_request `ComputeEncryptedTally)
                "elections/%s" uuid
            in
            match x.code with
            | 200 ->
                Cache.invalidate Cache.e_status;
                (* in Shuffle mode, if there is no external trustee, then FinishShuffle *)
                let* status = Cache.get_until_success Cache.e_status in
                let* () =
                  if status.status_state = `Shuffling then
                    let* nb_shufflers = nb_shufflers () in
                    if nb_shufflers = 1 then (
                      let ifmatch =
                        Some (sha256_b64 @@ string_of_election_status status)
                      in
                      let* x =
                        post_with_token ?ifmatch
                          (string_of_admin_request `FinishShuffling)
                          "elections/%s" uuid
                      in
                      (match x.code with
                      | 200 ->
                          Cache.invalidate Cache.e_status;
                          where_am_i :=
                            Election
                              {
                                uuid = Uuid.wrap uuid;
                                status = Tallied;
                                tab = Status;
                              }
                      | code ->
                          alert ("Failed with code " ^ string_of_int code);
                          where_am_i :=
                            Election
                              {
                                uuid = Uuid.wrap uuid;
                                status = Running;
                                tab = Status;
                              });
                      Lwt.return_unit)
                    else Lwt.return_unit
                  else (
                    where_am_i :=
                      Election
                        {
                          uuid = Uuid.wrap uuid;
                          status = Running;
                          tab = Status;
                        };
                    Lwt.return_unit)
                in
                !update_election_main ()
            | _ ->
                alert ("Failed with error code " ^ string_of_int x.code);
                Lwt.return_unit)
        else Lwt.return_none
      in
      {
        title = s_ "Tally the election";
        id = "tab_tally";
        status = `None;
        handler;
      }
      |> Lwt.return
  | Status ->
      {
        title = s_ "Status";
        id = "tab_status";
        status = `None;
        handler = Some (default_handler x);
      }
      |> Lwt.return
  | Destroy ->
      let handler () =
        let uuid = get_current_uuid () in
        let confirm =
          confirm @@ s_ "Are you sure you want to delete this election?"
        in
        if confirm then (
          Cache.invalidate_all ();
          let* x =
            delete_with_token
              (if is_draft then "drafts/%s" else "elections/%s")
              uuid
          in
          match x.code with
          | 200 ->
              where_am_i := List_draft;
              Dom_html.window##.location##.hash := Js.string "";
              Lwt.return_unit
          | code ->
              alert ("Deletion failed with code " ^ string_of_int code);
              Lwt.return_unit)
        else Lwt.return_unit
      in
      {
        title = s_ "Delete the election";
        id = "tab_delete";
        status = `None;
        handler = Some handler;
      }
      |> Lwt.return

let rec insert_sep sep x =
  match x with [] | [ _ ] -> x | a :: b -> a :: sep () :: insert_sep sep b

let flatten_with_sep sep x = List.flatten @@ insert_sep sep x

let lines_to_file l =
  let res = String.concat "\n" l in
  res ^ "\n"

let tab_elt title =
  div ~a:[ a_class [ "main-menu__item-menutitle" ] ] [ txt title ]

let subtab_elt name () =
  let active =
    match !where_am_i with Election { tab; _ } -> tab = name | _ -> false
  in
  let* { title; id; status; handler } = tabs name in
  let classes = [ "main-menu__item"; "noselect" ] in
  let classes =
    (if handler = None then "unavailable" else "clickable") :: classes
  in
  let classes = if active then "active" :: classes else classes in
  let attr = [ a_id id; a_class classes ] in
  let title = div ~a:attr [ txt title ] in
  let () =
    match handler with
    | None -> ()
    | Some handler ->
        let r = Tyxml_js.To_dom.of_div title in
        r##.onclick := lwt_handler handler
  in
  let status =
    match status with
    | `DDone -> div ~a:[ a_class [ "main-menu__ddone" ] ] []
    | `Done -> div ~a:[ a_class [ "main-menu__done" ] ] []
    | `Doing -> div ~a:[ a_class [ "main-menu__doing" ] ] []
    | `Todo -> div ~a:[ a_class [ "main-menu__todo" ] ] []
    | `Wip -> div ~a:[ a_class [ "main-menu__wip" ] ] []
    | `None -> div ~a:[ a_class [ "main-menu__doing" ] ] [] (* FIXME *)
  in
  let title =
    if active then
      [
        div
          ~a:[ a_class [ "positioned" ] ]
          [ div ~a:[ a_class [ "main-menu__item-active" ] ] []; title ];
      ]
    else [ title ]
  in
  Lwt.return (status :: title)

let tab_polling () =
  let open (val !Belenios_js.I18n.gettext) in
  let title = tab_elt @@ s_ "Poll" in
  let* tab_title = subtab_elt Title () in
  let* tab_questions = subtab_elt Questions () in
  let* tab_voters = subtab_elt Voters () in
  let* tab_dates = subtab_elt Dates () in
  let* tab_language = subtab_elt Language () in
  let* tab_contact = subtab_elt Contact () in
  let elt =
    [
      tab_title; tab_questions; tab_voters; tab_dates; tab_language; tab_contact;
    ]
  in
  Lwt.return
    (title
    :: flatten_with_sep
         (fun () -> [ div ~a:[ a_class [ "main-menu__item-separator" ] ] [] ])
         elt)

let tab_security () =
  let open (val !Belenios_js.I18n.gettext) in
  let title = tab_elt @@ s_ "Security" in
  let* tab_trustees = subtab_elt Trustees () in
  let* tab_credauth = subtab_elt CredAuth () in
  let* tab_voterspwd = subtab_elt VotersPwd () in
  let elt = [ tab_trustees; tab_credauth; tab_voterspwd ] in
  Lwt.return
    (title
    :: flatten_with_sep
         (fun () -> [ div ~a:[ a_class [ "main-menu__item-separator" ] ] [] ])
         elt)

let tab_manage () =
  let open (val !Belenios_js.I18n.gettext) in
  let title = tab_elt @@ s_ "Management" in
  let* tab_electionpage = subtab_elt ElectionPage () in
  let* tab_create = subtab_elt CreateOpenClose () in
  let* tab_tally = subtab_elt Tally () in
  let* tab_status = subtab_elt Status () in
  let* tab_destroy = subtab_elt Destroy () in
  let elt =
    [ tab_electionpage; tab_create; tab_tally; tab_status; tab_destroy ]
  in
  Lwt.return
    (title
    :: flatten_with_sep
         (fun () -> [ div ~a:[ a_class [ "main-menu__item-separator" ] ] [] ])
         elt)

let all_tabs () =
  let* tab_polling = tab_polling () in
  let* tab_security = tab_security () in
  let* tab_manage = tab_manage () in
  Lwt.return @@ List.flatten [ tab_polling; tab_security; tab_manage ]

(*****************************************************)
(* The main zone *)

let handler f =
  Dom_html.handler (fun e ->
      f e;
      Js._false)

let update_header () =
  let open (val !Belenios_js.I18n.gettext) in
  let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
  let title = draft.draft_questions.t_name in
  let descr = draft.draft_questions.t_description in
  let* () =
    let&&* container = document##getElementById (Js.string "election_name") in
    show_in container (fun () -> Lwt.return [ txt @@ s_ "Setup: " ^ title ])
  in
  let&&* container = document##getElementById (Js.string "election_descr") in
  show_in container (fun () -> Lwt.return [ txt descr ])

let title_content () =
  let open (val !Belenios_js.I18n.gettext) in
  if is_draft () then (
    let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
    let name, nameget =
      textarea
        ~a:[ a_id "election_name_textarea" ]
        ~cols:50 ~rows:3 draft.draft_questions.t_name
    in
    let r = Tyxml_js.To_dom.of_textarea name in
    r##.onchange :=
      lwt_handler (fun _ ->
          let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
          Cache.set Cache.draft
            (Draft
               ( v,
                 {
                   draft with
                   draft_questions =
                     { draft.draft_questions with t_name = nameget () };
                 } ));
          update_header ());
    let desc, descget =
      textarea
        ~a:[ a_id "election_description_textarea" ]
        ~cols:50 ~rows:5 draft.draft_questions.t_description
    in
    let r = Tyxml_js.To_dom.of_textarea desc in
    r##.onchange :=
      lwt_handler (fun _ ->
          let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
          Cache.set Cache.draft
            (Draft
               ( v,
                 {
                   draft with
                   draft_questions =
                     { draft.draft_questions with t_description = descget () };
                 } ));
          update_header ());
    Lwt.return
      [
        h2 [ txt @@ s_ "Title:" ];
        div [ name ];
        h2 [ txt @@ s_ "Description:" ];
        div [ desc ];
      ])
  else
    (* not is_draft, i.e. running *)
    let* (Template (_, elec)) = Cache.get_until_success Cache.e_elec in
    let tit = elec.t_name in
    let desc = elec.t_description in
    Lwt.return
      [
        h2 [ txt @@ s_ "Title:" ];
        div [ txt tit ];
        h2 [ txt @@ s_ "Description:" ];
        div [ txt desc ];
      ]

let erase_voter_elt v () =
  let elt = div ~a:[ a_class [ "del_sym" ] ] [] in
  let r = Tyxml_js.To_dom.of_div elt in
  r##.onclick :=
    lwt_handler (fun () ->
        let* voters = Cache.get_until_success Cache.voters in
        let voters = List.filter (fun x -> x <> v) voters in
        let () = Cache.set Cache.voters voters in
        !update_election_main ());
  elt

let voters_content () =
  let open (val !Belenios_js.I18n.gettext) in
  let is_draft = is_draft () in
  let* is_frozen =
    if is_draft then
      let* status = Cache.get_until_success Cache.status in
      Lwt.return status.credentials_ready
    else Lwt.return false
  in
  let* voters =
    if is_draft then Cache.get_until_success Cache.voters
    else Cache.get_until_success Cache.e_voters
  in
  let* records =
    if is_draft then Lwt.return [] else Cache.get_until_success Cache.e_records
  in
  let reco = List.map (fun r -> r.vr_username) records in
  let with_login, with_weight =
    let rec loop ((with_login, with_weight) as accu) = function
      | [] -> accu
      | ((_, { login; weight; _ }) : Voter.t) :: xs ->
          let with_login = with_login || login <> None in
          let with_weight = with_weight || weight <> None in
          if with_login && with_weight then (true, true)
          else loop (with_login, with_weight) xs
    in
    loop (false, false) voters
  in
  let header_row =
    let row = [ th [] ] in
    let row = if is_draft then row else th [ txt (s_ "voted?") ] :: row in
    let row = if with_weight then th [ txt @@ s_ "Weight" ] :: row else row in
    let row = if with_login then th [ txt @@ s_ "Login" ] :: row else row in
    tr (th [ txt @@ s_ "Identity" ] :: row)
  in
  let erv v () =
    if is_draft && not is_frozen then [ erase_voter_elt v () ] else []
  in
  let rows_of_voters =
    List.map
      (fun v ->
        tr
          (List.flatten
             [
               (let address, login, weight = Voter.get v in
                let row =
                  if is_draft then []
                  else
                    let voted = List.mem login reco in
                    [ td [ txt (if voted then "X" else "—") ] ]
                in
                let row =
                  if with_weight then
                    td [ txt @@ Weight.to_string weight ] :: row
                  else row
                in
                let row = if with_login then td [ txt login ] :: row else row in
                td [ txt address ] :: row);
               [ td ~a:[ a_class [ "clickable" ] ] (erv v ()) ];
             ]))
      voters
  in
  let rows_of_voters =
    if rows_of_voters = [] then
      [ tr [ td [ em [ txt @@ s_ "empty list" ] ]; td [] ] ]
    else rows_of_voters
  in
  let placeholder =
    "bart.simpson@example.com              # " ^ s_ "typical use"
    ^ "\nalbert.einstein@example.com,albert_e  # "
    ^ s_ "when a login is needed, e.g. CAS"
    ^ "\nasterix.legaulois@example.com,,2      # "
    ^ s_ "when some voters have several votes"
  in
  let tt, ttget = textarea ~cols:80 ~rows:10 ~placeholder "" in
  let rm_button =
    button (s_ "Delete all") (fun () ->
        let confirm = confirm @@ s_ "Warning, this action is irreversible" in
        if confirm then
          let newvoters = [] in
          let () = Cache.set Cache.voters newvoters in
          !update_election_main ()
        else Lwt.return_unit)
  in
  let add_button =
    button
      ~a:[ a_id "add_voters" ]
      (s_ "Add")
      (fun () ->
        match Voter.list_of_string @@ ttget () with
        | [] -> Lwt.return_unit
        | newvoters ->
            let* voters = Cache.get_until_success Cache.voters in
            let newvoters = voters @ newvoters in
            let () = Cache.set Cache.voters newvoters in
            !update_election_main ())
  in
  let import_but =
    button (s_ "from another election") (fun () ->
        let* res = Cache.sync () in
        match res with
        | Error msg -> popup_failsync msg
        | Ok () ->
            let* voters = Cache.get_until_success Cache.voters in
            let ifmatch = sha256_b64 @@ string_of_voter_list voters in
            let* () =
              popup_choose_elec
                (fun uuid ->
                  let r = `Import (Uuid.wrap uuid) in
                  let* x =
                    post_with_token ~ifmatch
                      (string_of_voters_request r)
                      "drafts/%s/voters" (get_current_uuid ())
                  in
                  if x.code <> 200 then
                    alert ("Failed with error code " ^ string_of_int x.code);
                  Cache.invalidate Cache.voters;
                  !update_election_main ())
                ()
            in
            Lwt.return_unit)
  in
  let upload_input, _get_filename =
    input ~a:[ a_input_type `File; a_name "fileupload"; a_id "fileupload" ] ""
  in
  let upload_button =
    button (s_ "Upload voter file") (fun () ->
        let&&* d = document##getElementById (Js.string "fileupload") in
        let&&* d = Dom_html.CoerceTo.input d in
        let^ f = d##.files in
        let&&* file = f##item 0 in
        let* text = read_full file in
        let voters = Voter.list_of_string (Js.to_string text) in
        let () = Cache.set Cache.voters voters in
        !update_election_main ())
  in
  if is_draft then
    let* config = Cache.get Cache.config in
    Lwt.return
      [
        h2
          [
            txt
              (if is_frozen then s_ "Voter list (not editable):"
               else s_ "Voter list:");
          ];
        div
          ~a:[ a_id "list_warning" ]
          [
            strong [ txt @@ s_ "Warning:" ];
            txt " ";
            txt
              (s_ "you have to make sure that the e-mail addresses are valid.");
          ];
        div
          ~a:[ a_id "list_warning2" ]
          [
            txt
              (s_
                 "You won't be able to change the e-mail addresses once the \
                  credentials are created. Voters with invalid e-mail \
                  addresses won't be able to vote.");
          ];
        tablex [ tbody (header_row :: rows_of_voters) ];
        (if is_frozen then div []
         else
           let max =
             match config with
             | Error _ -> "maybe 2500"
             | Ok c -> string_of_int c.max_voters
           in
           div
             [
               rm_button;
               div
                 ~a:[ a_id "addtolist" ]
                 [
                   div
                     [
                       txt
                         (Printf.sprintf
                            (f_
                               "Please enter the identities of voters to add, \
                                one per line (max %s).")
                            max);
                     ];
                   div
                     [
                       tt;
                       div
                         ~a:[ a_class [ "tooltip" ] ]
                         [
                           div [ txt "?" ];
                           div
                             ~a:[ a_class [ "tooltiptext" ] ]
                             [
                               txt
                                 (s_
                                    "An identity is either \"address\", or \
                                     \"address,username\", or \
                                     \"address,username,weight\", or \
                                     \"address,,weight\" where \"address\" is \
                                     an e-mail address, \"username\" the \
                                     associated user name for authentication, \
                                     and \"weight\" is the number of votes of \
                                     the voter (in case voters don't have all \
                                     the same number of votes).");
                             ];
                         ];
                     ];
                   add_button;
                   div
                     ~a:[ a_id "import_block" ]
                     [
                       h4 [ txt @@ s_ "Import voters " ];
                       ul
                         [
                           li [ import_but ];
                           li
                             [
                               txt @@ s_ "from a file: ";
                               upload_input;
                               upload_button;
                             ];
                         ];
                     ];
                 ];
             ]);
      ]
  else
    (* Running election *)
    let data =
      List.map
        (fun x -> datestring_of_float x.vr_date ^ " " ^ x.vr_username)
        records
    in
    let link =
      a_data ~filename:"records.txt" ~mime_type:"text/plain"
        ~data:(lines_to_file data) (s_ "Voting records")
    in
    let link2 =
      a_data ~filename:"voters.txt" ~mime_type:"text/plain"
        ~data:(Voter.list_to_string voters)
        (s_ "Voter list")
    in
    let nv = List.length voters in
    let n = List.length records in
    let turnout =
      Printf.sprintf
        (f_ "Current turnout: %d / %d = %.2f %%")
        n nv
        (100. *. (float_of_int n /. float_of_int nv))
    in
    Lwt.return
      [
        h2 [ txt @@ s_ "Voter list (not editable):" ];
        tablex [ tbody (header_row :: rows_of_voters) ];
        div [ txt turnout ];
        div
          ~a:[ a_class [ "txt_with_a" ] ]
          [ txt (s_ "Link to the "); link2; txt @@ s_ " in txt format." ];
        div
          ~a:[ a_class [ "txt_with_a" ] ]
          [ txt (s_ "Link to the "); link; txt @@ s_ " in txt format." ];
      ]

let is_openable () =
  if is_draft () then Lwt.return false
  else
    let* status = Cache.get_until_success Cache.e_status in
    Lwt.return
      (match status.status_state with `Open | `Closed -> true | _ -> false)

let format_date_object x =
  Printf.ksprintf Js.string "%d-%02d-%02dT%02d:%02d" x##getFullYear
    (x##getMonth + 1)
    x##getDate x##getHours x##getMinutes

let dates_content () =
  let open (val !Belenios_js.I18n.gettext) in
  let header = h2 [ txt @@ s_ "Automatic dates" ] in
  let* is_openable = is_openable () in
  if is_draft () then
    Lwt.return
      [ header; div [ txt @@ s_ "Not (yet) available for draft elections" ] ]
  else if not is_openable then
    Lwt.return
      [ header; div [ txt @@ s_ "This election can no longer be opened." ] ]
  else
    let* dates = Cache.get_until_success Cache.e_dates in
    let make_div l id get set =
      let attr = [ a_id id; a_input_type `Datetime_local ] in
      let inp, inp_get = input ~a:attr "" in
      let r = Tyxml_js.To_dom.of_input inp in
      let () =
        match get dates with
        | None -> ()
        | Some x ->
            r##.value :=
              format_date_object (new%js Js.date_fromTimeValue (x *. 1000.))
      in
      let sync () =
        let x = inp_get () in
        let d =
          if x = "" then None else Some (Js.date##parse (Js.string x) /. 1000.)
        in
        let* dates = Cache.get_until_success Cache.e_dates in
        Cache.set Cache.e_dates (set dates d);
        Cache.sync_until_success ()
      in
      let _ =
        Dom.addEventListener r
          (Dom.Event.make "focusout")
          (lwt_handler (fun _ -> sync ()))
          Js._false
      in
      let label = label ~a:[ a_label_for id ] [ txt l ] in
      let btn_soon =
        let@ () = button (s_ "In 5 minutes") in
        let t = (new%js Js.date_now)##valueOf +. 300_000. in
        r##.value := format_date_object (new%js Js.date_fromTimeValue t);
        sync ()
      in
      let btn_erase =
        let@ () = button (s_ "Erase") in
        r##.value := Js.string "";
        sync ()
      in
      div [ label; inp; btn_soon; btn_erase ]
    in
    let open_div =
      make_div (s_ "Open: ") "inpocont"
        (fun x -> x.auto_date_open)
        (fun x y -> { x with auto_date_open = y })
    in
    let close_div =
      make_div (s_ "Close: ") "inpccont"
        (fun x -> x.auto_date_close)
        (fun x y -> { x with auto_date_close = y })
    in
    let publish_div =
      make_div (s_ "Publish: ") "inppcont"
        (fun x -> x.auto_date_publish)
        (fun x y -> { x with auto_date_publish = y })
    in
    Lwt.return [ header; open_div; close_div; publish_div ]

let check_lang_choice x avail = List.for_all (fun l -> List.mem l avail) x

let language_content () =
  let open (val !Belenios_js.I18n.gettext) in
  let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
  let* config = Cache.get_until_success Cache.config in
  let lang = draft.draft_languages in
  let strlang = String.concat " " lang in
  let inp, langet = input ~a:[ a_id "inplang" ] strlang in
  let r = Tyxml_js.To_dom.of_input inp in
  r##.onchange :=
    handler (fun _ ->
        let newlist = String.split_on_char ' ' (langet ()) in
        if
          check_lang_choice newlist
            (List.map (fun (x, _) -> x) config.languages)
        then
          Cache.set Cache.draft
            (Draft (v, { draft with draft_languages = newlist }))
        else alert @@ s_ "Some language in the list is not available");
  let avail_lang =
    config.languages
    |> List.map (fun (x, y) -> tr [ td [ txt x ]; td [ txt y ] ])
  in
  let avail_lang =
    tablex
      [
        tbody
          (tr [ th [ txt @@ s_ "Code" ]; th [ txt @@ s_ "Language" ]; th [] ]
          :: avail_lang);
      ]
  in
  Lwt.return
    [
      h2 [ txt @@ s_ "Languages:" ];
      div
        ~a:[ a_id "choose_lang" ]
        [
          div
            [
              txt
              @@ s_
                   "This is a space-separated list of languages that will be \
                    used in e-mails sent by the server.";
            ];
          div [ label ~a:[ a_label_for "inplang" ] [ txt "Languages: " ]; inp ];
        ];
      div
        ~a:[ a_id "avail_lang" ]
        [
          div [ txt @@ s_ "List of available languages, with their code:" ];
          avail_lang;
        ];
    ]

let contact_content () =
  let open (val !Belenios_js.I18n.gettext) in
  let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
  let contact = Option.value ~default:"" draft.draft_contact in
  let inp, contget = input ~a:[ a_id "inpcont" ] contact in
  let r = Tyxml_js.To_dom.of_input inp in
  r##.onchange :=
    handler (fun _ ->
        let newc = contget () in
        Cache.set Cache.draft
          (Draft (v, { draft with draft_contact = Some newc })));
  (* The default set by the server is the name of the administrator;
   * no need to do it on our side. In case this changes, we default to "" *)
  let admin = Option.value ~default:"" draft.draft_questions.t_administrator in
  let inpA, adminget = input ~a:[ a_id "admincont" ] admin in
  let r = Tyxml_js.To_dom.of_input inpA in
  r##.onchange :=
    handler (fun _ ->
        let newA = adminget () in
        Cache.set Cache.draft
          (Draft
             ( v,
               {
                 draft with
                 draft_questions =
                   { draft.draft_questions with t_administrator = Some newA };
               } )));
  Lwt.return
    [
      h2 [ txt @@ s_ "Contact:" ];
      div
        [
          txt @@ s_ "This contact will be added to e-mails sent to the voters.";
        ];
      div [ label ~a:[ a_label_for "inpcont" ] [ txt "Contact: " ]; inp ];
      h2 [ txt @@ s_ "Public name of the administrator: " ];
      div
        [ txt @@ s_ "This name will be published on the election result page." ];
      div
        [
          label
            ~a:[ a_label_for "admincont" ]
            [ txt @@ s_ "Public name of the administrator:" ];
          inpA;
        ];
    ]

let send_draft_request req =
  let uuid = get_current_uuid () in
  let* x = post_with_token (string_of_draft_request req) "drafts/%s" uuid in
  if x.code <> 200 then
    alert ("Draft request failed with error code " ^ string_of_int x.code);
  Lwt.return_unit

let change_credauth_name name =
  let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
  Cache.set Cache.draft
    (Draft
       ( v,
         {
           draft with
           draft_questions =
             { draft.draft_questions with t_credential_authority = Some name };
         } ));
  let* () = Cache.sync_until_success () in
  let* () = send_draft_request `SetCredentialAuthorityVisited in
  let* res = Cache.sync () in
  match res with
  | Error msg -> popup_failsync msg
  | Ok () -> !update_election_main ()

let credauth_content () =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = get_current_uuid () in
  let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
  let* status = Cache.get_until_success Cache.status in
  let first_currsel =
    if not status.credential_authority_visited then `None
    else if draft.draft_questions.t_credential_authority = Some "server" then
      `Server
    else `Extern
  in
  let currsel = ref first_currsel in
  (* The page content, when the user can still choose between both options *)
  let* changeable_content =
    (* server ? *)
    let attr =
      [ a_id "rad_serv"; a_name "rad_credauth"; a_input_type `Radio ]
    in
    let attr = if !currsel = `Server then a_checked () :: attr else attr in
    let rad_serv, _ = input ~a:attr "" in
    let r = Tyxml_js.To_dom.of_input rad_serv in
    r##.onclick :=
      lwt_handler (fun () ->
          let* () = change_credauth_name "server" in
          currsel := `Server;
          let* () =
            let&&* d = document##getElementById (Js.string "cred_auth_name") in
            d##.style##.display := Js.string "none";
            Lwt.return_unit
          in
          let&&* d = document##getElementById (Js.string "cred_gen_serv") in
          d##.style##.visibility := Js.string "visible";
          Lwt.return_unit);
    let lab_serv =
      label
        ~a:[ a_label_for "rad_serv" ]
        [ txt @@ s_ "By our server (not ideal for decentralized security)" ]
    in
    let generate_but =
      button (s_ "Generate and send the credentials") (fun () ->
          let op = string_of_public_credentials [] in
          let* res = post_with_token op "drafts/%s/credentials/public" uuid in
          match res.code with
          | 200 -> !update_election_main ()
          | _ ->
              alert ("Failed with error code " ^ string_of_int res.code);
              Lwt.return_unit)
    in
    let generate_part =
      div
        ~a:[ a_id "cred_gen_serv" ]
        [
          generate_but;
          div [ txt @@ s_ "Warning: this will freeze the voter list!" ];
        ]
    in
    let dd = Tyxml_js.To_dom.of_div generate_part in
    if !currsel <> `Server then dd##.style##.visibility := Js.string "hidden";
    let serv_part = div [ rad_serv; lab_serv; generate_part ] in
    (* extern ? *)
    let attr = [ a_id "rad_ext"; a_name "rad_credauth"; a_input_type `Radio ] in
    let attr = if !currsel = `Extern then a_checked () :: attr else attr in
    let rad_ext, _ = input ~a:attr "" in
    let r = Tyxml_js.To_dom.of_input rad_ext in
    r##.onclick :=
      lwt_handler (fun () ->
          currsel := `Extern;
          let* () =
            let&&* d = document##getElementById (Js.string "cred_auth_name") in
            d##.style##.display := Js.string "block";
            Lwt.return_unit
          in
          let&&* d = document##getElementById (Js.string "cred_gen_serv") in
          d##.style##.visibility := Js.string "hidden";
          Lwt.return_unit);
    let lab_ext =
      label
        ~a:[ a_label_for "rad_ext" ]
        [ txt @@ s_ "By a third-party of your choice" ]
    in
    let extern_name_div, has_name =
      let value, has_name =
        if !currsel = `Extern then
          match draft.draft_questions.t_credential_authority with
          | Some x -> (x, true)
          | _ -> ("none", false)
        else ("", false)
      in
      let inp_ext, get_ext =
        input
          ~a:[ a_placeholder @@ s_ "Name of the credential authority" ]
          value
      in
      let r = Tyxml_js.To_dom.of_input inp_ext in
      r##.onchange :=
        lwt_handler (fun () ->
            let name = get_ext () in
            change_credauth_name name);
      let dd = div ~a:[ a_id "cred_auth_name" ] [ lab_ext; inp_ext ] in
      let ddd = Tyxml_js.To_dom.of_div dd in
      if !currsel <> `Extern then ddd##.style##.display := Js.string "none"
      else ddd##.style##.display := Js.string "block";
      (dd, has_name)
    in
    let* print_link =
      if has_name then
        let* x = get (fun x -> x) "drafts/%s/credentials/token" uuid in
        match x with
        | Error _ ->
            alert "Failed to get token";
            Lwt.return @@ div []
        | Ok (token, _) ->
            let* prefix = Cache.get_prefix () in
            let link =
              Printf.sprintf "%sdraft/credentials.html#%s-%s" prefix uuid token
            in
            Lwt.return
            @@ div
                 ~a:[ a_id "cred_link" ]
                 [
                   div
                     [
                       txt @@ s_ "Here is the link to send to the authority:";
                       ul
                         [
                           li
                             [
                               span ~a:[ a_id "cred_link_target" ] [ txt link ];
                             ];
                         ];
                     ];
                   div [ txt @@ s_ "Warning: this will freeze the voter list!" ];
                 ]
      else Lwt.return @@ div []
    in
    let extern_part = div [ rad_ext; lab_ext; extern_name_div; print_link ] in
    (* put things together for changeable_content *)
    Lwt.return
    @@ div ~a:[ a_class [ "which_credauth" ] ] [ serv_part; extern_part ]
  in
  (* The page content, when server is definitely chosen *)
  let* server_content =
    let* priv =
      get private_credentials_of_string "drafts/%s/credentials/private" uuid
    in
    match priv with
    | Error _ -> Lwt.return @@ div [ txt "Error" ]
    | Ok (p, _) ->
        let link =
          let href =
            string_of_private_credentials p
            |> encode_data_uri ~mime_type:"text/plain"
          in
          a ~a:[ a_download (Some "codes.txt") ] ~href
          @@ s_ "the private parts of the credentials"
        in
        let r = Tyxml_js.To_dom.of_a link in
        r##.onclick :=
          lwt_handler (fun () ->
              let* x =
                post_with_token
                  (string_of_draft_request `SetDownloaded)
                  "drafts/%s" uuid
              in
              match x.code with
              | 200 -> !update_election_main ()
              | _ ->
                  alert ("Failed with error code " ^ string_of_int x.code);
                  Lwt.return_unit);
        div
          ~a:[ a_class [ "txt_with_a" ] ]
          [
            txt @@ s_ "Please download ";
            link;
            txt @@ s_ " and save them in a secure location.";
          ]
        |> Lwt.return
  in
  (* The page content, when external authority is definitely chosen *)
  let extern_content =
    div
      [
        txt
        @@ s_
             "Credentials have been received from the external credential \
              authority.";
      ]
  in
  (* The page content, when the server is generating credentials *)
  let pending_content i =
    div
      [
        txt @@ s_ "Credentials are being generated on the server.";
        txt " ";
        Printf.ksprintf txt (f_ "Number of credentials left: %d.") i;
      ]
  in
  let content =
    match first_currsel with
    | `None -> changeable_content
    | `Server -> (
        if status.credentials_ready then server_content
        else
          match status.credentials_left with
          | None -> changeable_content
          | Some i -> pending_content i)
    | `Extern ->
        if status.credentials_ready then extern_content else changeable_content
  in
  Lwt.return [ div [ h3 [ txt @@ s_ "Management of credentials:" ]; content ] ]

let voterspwd_content_draft () =
  let open (val !Belenios_js.I18n.gettext) in
  let* status = Cache.get_until_success Cache.status in
  let first_visit = not status.voter_authentication_visited in
  let pwd_rdy = status.passwords_ready in
  let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
  let* voters = Cache.get_until_success Cache.voters in
  let curr_auth = draft.draft_authentication in
  if List.length voters = 0 then
    Lwt.return [ div [ txt @@ s_ "Please fill-in the voter list first." ] ]
  else if curr_auth = `Password && pwd_rdy = Some true then
    Lwt.return
      [ div [ txt @@ s_ "This task is completed. Passwords have been sent." ] ]
  else
    let* config = Cache.get Cache.config in
    match config with
    | Error e ->
        let msg =
          Printf.sprintf
            (f_ "Error while retrieving server configuration: %s")
            e
        in
        alert msg;
        Lwt.return
          [ h2 [ txt @@ s_ "Voter's authentication:" ]; div [ txt msg ] ]
    | Ok c ->
        let rad i sel text () =
          let id = "auth" ^ string_of_int i in
          let attr = [ a_name "auth"; a_id id; a_input_type `Radio ] in
          let attr =
            if (not first_visit) && sel then a_checked () :: attr else attr
          in
          let inp, _ = input ~a:attr "" in
          let lab = label ~a:[ a_label_for id ] [ txt text ] in
          (inp, lab)
        in
        let set_onchange e get =
          let e = Tyxml_js.To_dom.of_input e in
          let draft_authentication = get () in
          e##.onchange :=
            lwt_handler (fun _ ->
                let* () = send_draft_request `SetVoterAuthenticationVisited in
                Cache.set Cache.draft
                  (Draft (v, { draft with draft_authentication }));
                !update_election_main ())
        in
        let ll =
          c.authentications
          |> List.mapi (fun i x ->
                 match x with
                 | `Password ->
                     let inp, lab =
                       rad i (curr_auth = `Password)
                         (s_
                            "Password sent in advance by e-mail (useful for \
                             multiple elections)")
                         ()
                     in
                     set_onchange inp (fun () -> `Password);
                     let but =
                       button (s_ "Send passwords to voters") (fun () ->
                           let* (Draft (_, dr)) =
                             Cache.get_until_success Cache.draft
                           in
                           if dr.draft_authentication <> `Password then (
                             alert
                             @@ s_ "Please select password authentication first";
                             Lwt.return_unit)
                           else
                             let confirm =
                               confirm
                               @@ s_ "Warning: this will freeze the voter list!"
                             in
                             if not confirm then Lwt.return_unit
                             else
                               let uuid = get_current_uuid () in
                               let* voters =
                                 Cache.get_until_success Cache.voters
                               in
                               let ifmatch = sha256_b64 "[]" in
                               let* _ =
                                 post_with_token ~ifmatch
                                   (string_of_voter_list voters)
                                   "drafts/%s/passwords" uuid
                               in
                               !update_election_main ())
                     in
                     div [ inp; lab; but ]
                 | `CAS ->
                     let sel, casname =
                       match curr_auth with
                       | `CAS s -> (true, s)
                       | _ -> (false, "")
                     in
                     let inp, lab =
                       rad i sel
                         (s_
                            "CAS (external authentication server, offers \
                             better security guarantees when applicable)")
                         ()
                     in
                     let inp2, get2 =
                       input ~a:[ a_placeholder "https://cas.inria.fr" ] casname
                     in
                     let get () = `CAS (get2 ()) in
                     set_onchange inp get;
                     set_onchange inp2 get;
                     div [ inp; lab; inp2 ]
                 | `Configured xx -> (
                     match xx.configured_system with
                     | "dummy" ->
                         let sel =
                           match curr_auth with
                           | `Configured s -> s = xx.configured_instance
                           | _ -> false
                         in
                         let inp, lab =
                           rad i sel
                             (s_
                                "Dummy auth (should not be used in \
                                 production): "
                             ^ xx.configured_instance)
                             ()
                         in
                         set_onchange inp (fun () ->
                             `Configured xx.configured_instance);
                         div [ inp; lab ]
                     | "email" ->
                         let sel =
                           match curr_auth with
                           | `Configured s -> s = xx.configured_instance
                           | _ -> false
                         in
                         let inp, lab =
                           rad i sel
                             (s_
                                "Password sent by e-mail when voting (a short \
                                 password, renewed for each vote)")
                             ()
                         in
                         set_onchange inp (fun () ->
                             `Configured xx.configured_instance);
                         div [ inp; lab ]
                     | _ ->
                         (* TODO: add oidc, cas, password, here *)
                         let sel =
                           match curr_auth with
                           | `Configured s -> s = xx.configured_instance
                           | _ -> false
                         in
                         let inp, lab =
                           rad i sel
                             ("Unknown (" ^ xx.configured_instance ^ ")")
                             ()
                         in
                         set_onchange inp (fun () ->
                             `Configured xx.configured_instance);
                         div [ inp; lab ]))
        in
        let ll =
          match ll with
          | [] -> assert false
          | first :: others ->
              [
                h4 [ txt @@ s_ "Default mode:" ];
                first;
                h4 [ txt @@ s_ "Other authentication modes:" ];
              ]
              @ others
        in
        Lwt.return [ h2 [ txt @@ s_ "Voter's authentication:" ]; div ll ]

let voterspwd_content_running () =
  let open (val !Belenios_js.I18n.gettext) in
  let username, get_username = input "" in
  let submit =
    let@ () = button @@ s_ "Submit" in
    let uuid = get_current_uuid () in
    let username = get_username () in
    let request = `RegeneratePassword username in
    let* x =
      post_with_token (string_of_admin_request request) "elections/%s" uuid
    in
    match x.code with
    | 200 ->
        let msg =
          Printf.sprintf (f_ "A new password has been mailed to %s.") username
        in
        alert msg;
        !update_election_main ()
    | 404 ->
        let msg =
          Printf.sprintf
            (f_ "Failure, probably because of an error in the username: %s.")
            username
        in
        alert msg;
        Lwt.return_unit
    | _ ->
        let msg =
          Printf.sprintf (f_ "Unexpected failure with code %d.") x.code
        in
        alert msg;
        Lwt.return_unit
  in
  [
    h2 [ txt @@ s_ "Regenerate and e-mail a password" ];
    div [ txt @@ s_ "Username:"; txt " "; username; txt " "; submit ];
  ]
  |> Lwt.return

let voterspwd_content () =
  if is_draft () then voterspwd_content_draft ()
  else voterspwd_content_running ()

let create_content () =
  let open (val !Belenios_js.I18n.gettext) in
  (* It could be that the button is active, but the election is no longer ready.
   * Let's check again. *)
  let* ok = is_ready () in
  if not ok then title_content ()
  else
    let uuid = get_current_uuid () in
    let but =
      button (s_ "Create") (fun () ->
          let* x =
            post_with_token
              (string_of_draft_request `ValidateElection)
              "drafts/%s" uuid
          in
          let fail () =
            alert ("Failed with error code " ^ string_of_int x.code);
            Lwt.return_unit
          in
          match x.code with
          | 200 ->
              where_am_i :=
                Election
                  {
                    uuid = Uuid.wrap uuid;
                    status = Running;
                    tab = CreateOpenClose;
                  };
              !update_election_main ()
          | 400 -> (
              match request_status_of_string x.content with
              | exception _ -> fail ()
              | status -> (
                  match status.error with
                  | `ValidationError (`MissingBilling id) ->
                      let* prefix = Cache.get_prefix () in
                      let url =
                        Printf.sprintf
                          "%sdraft/prebilling?id=%s&cont=elections/%s@new"
                          prefix id uuid
                      in
                      Dom_html.window##.location##.href := Js.string url;
                      Lwt.return_unit
                  | _ -> fail ()))
          | _ -> fail ())
    in
    Lwt.return
      [
        h2 [ txt @@ s_ "Ready to create:" ];
        div [ txt @@ s_ "Warning: this is irreversible!" ];
        div ~a:[ a_id "validate_but" ] [ but ];
      ]

let open_close_content () =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = get_current_uuid () in
  Cache.invalidate Cache.e_status;
  (* Could have changed due to automatic dates *)
  let* status = Cache.get_until_success Cache.e_status in
  let ifmatch = sha256_b64 @@ string_of_election_status status in
  let ifmatch = Some ifmatch in
  let is_open = if status.status_state = `Open then true else false in
  let curr, action, request =
    if is_open then (s_ "Election is currently open", s_ "Close", `Close)
    else (s_ "Election is currently closed", s_ "Open", `Open)
  in
  let but =
    button action (fun () ->
        let* x =
          post_with_token ?ifmatch
            (string_of_admin_request request)
            "elections/%s" uuid
        in
        match x.code with
        | 200 ->
            Cache.invalidate Cache.e_status;
            !update_election_main ()
        | _ ->
            alert ("Failed with error code " ^ string_of_int x.code);
            Lwt.return_unit)
  in
  Lwt.return [ h2 [ txt curr ]; div [ but ] ]

let pretty_timestamp x =
  let x = new%js Js.date_fromTimeValue (x *. 1000.) in
  Js.to_string x##toLocaleString

let status_content () =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = get_current_uuid () in
  let@ status, state =
   fun cont ->
    let* x = Cache.get Cache.e_status in
    match x with
    | Ok status -> cont (Some status, status.status_state)
    | Error _ -> cont (None, `Draft)
  in
  let automatic =
    match status with
    | None -> []
    | Some s ->
        let archival =
          match s.status_auto_archive_date with
          | None -> []
          | Some t ->
              [
                txt
                @@ Printf.sprintf
                     (f_
                        "This election will be automatically archived after %s.")
                     (pretty_timestamp t);
              ]
        in
        List.flatten
          [
            [ h2 [ txt @@ s_ "Automatic cleaning" ] ];
            archival;
            [
              div
                [
                  txt
                  @@ Printf.sprintf
                       (f_
                          "This election will be automatically deleted after \
                           %s.")
                       (pretty_timestamp s.status_auto_delete_date);
                ];
            ];
          ]
  in
  let* content =
    let header = h2 [ txt @@ s_ "Election status" ] in
    match state with
    | `Draft ->
        Lwt.return [ header; div [ txt @@ s_ "This election is a draft." ] ]
    | `Open | `Closed ->
        Lwt.return [ header; div [ txt @@ s_ "This election is running." ] ]
    | `EncryptedTally ->
        Lwt.return
          [
            header; div [ txt @@ s_ "The tally for this election has started." ];
          ]
    | `Shuffling ->
        Lwt.return
          [ header; div [ txt @@ s_ "This election is in shuffling phase." ] ]
    | `Tallied ->
        let but =
          button (s_ "Election main page") (fun () -> Preview.goto_mainpage ())
        in
        Lwt.return
          [
            h2 [ txt @@ s_ "This election has been tallied" ];
            div [ txt @@ s_ "Go see the result on the election main page!" ];
            div [ but ];
          ]
    | `Archived ->
        let link =
          a ~href:("../../elections/" ^ uuid ^ "/archive.zip") "archive.zip"
        in
        let but =
          button (s_ "Results page") (fun () -> Preview.goto_mainpage ())
        in
        Lwt.return
          [
            h2 [ txt (s_ "This election is archived") ];
            div
              ~a:[ a_class [ "txt_with_a" ] ]
              [ txt @@ s_ "The archive can be downloaded at: "; link ];
            but;
          ]
  in
  Lwt.return (content @ automatic)

let update_main_zone () =
  let&&* container = document##getElementById (Js.string "main_zone") in
  let* content =
    match !where_am_i with
    | Election { tab = Title; _ } -> title_content ()
    | Election { tab = Questions; _ } -> Questions.questions_content ()
    | Election { tab = Voters; _ } -> voters_content ()
    | Election { tab = Dates; _ } -> dates_content ()
    | Election { tab = Language; _ } -> language_content ()
    | Election { tab = Contact; _ } -> contact_content ()
    | Election { tab = Trustees; _ } -> Trustees_tab.trustees_content ()
    | Election { tab = CredAuth; _ } -> credauth_content ()
    | Election { tab = VotersPwd; _ } -> voterspwd_content ()
    | Election { tab = CreateOpenClose; _ } ->
        if is_draft () then create_content () else open_close_content ()
    | Election { tab = Status; _ } -> status_content ()
    | _ -> Lwt.return [ txt "Error: should never print this" ]
  in
  show_in container (fun () -> Lwt.return content)

(*****************************************************)
(* called from outside, or when we redraw everything *)
let () =
  update_election_main :=
    fun () ->
      let is_draft = is_draft () in
      let* () =
        if is_draft then (
          let* res = Cache.sync () in
          match res with
          | Error msg -> popup_failsync msg
          | Ok () ->
              Cache.invalidate Cache.status;
              Lwt.return_unit)
        else (
          Cache.invalidate Cache.e_status;
          Lwt.return_unit)
      in
      let&&* container = document##getElementById (Js.string "main") in
      let* () =
        show_in container (fun () ->
            let* all_tabs = all_tabs () in
            Lwt.return
              [
                div ~a:[ a_class [ "main-menu" ]; a_id "main_menu" ] all_tabs;
                div ~a:[ a_class [ "main-zone" ]; a_id "main_zone" ] [];
              ])
      in
      update_main_zone ()

let update_main () = !update_election_main ()
