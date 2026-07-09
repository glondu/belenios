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

open Belenios
open Belenios_web_api
open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session
open Common

let send_trustees_request ?ifmatch uuid w req =
  let* x = Api.(post ?ifmatch (trustees_draft uuid w) !user req) in
  let b =
    if x.code = 200 then (
      Cache.invalidate Cache.status;
      true)
    else (
      alert ("Trustees request failed with error code " ^ string_of_int x.code);
      false)
  in
  Lwt.return b

(* Forward decl of update functions *)
let update_main_zone = ref (fun _ -> Lwt.return_unit)

let cast_bt_trustee a b =
  !+(yojson_of_trustee (yojson_of_basic_parameters a b))
  >> !*(trustee_of_yojson Fun.id)

let cast_tt_trustee a b =
  !+(yojson_of_trustee (yojson_of_pedersen_cert a b))
  >> !*(trustee_of_yojson Fun.id)

let get_trustees () =
  let@ uuid cont =
    let* status = Cache.get_until_success Cache.e_status in
    match status.trustees with None -> Lwt.return_none | Some x -> cont x
  in
  let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
  let { version; group; _ } : raw_draft = draft in
  let module G = (val Group.make { version; group }) in
  let* x = Api.(get (trustees_draft uuid (module G)) !user) in
  let ifmatch = get_ifmatch x in
  match x with
  | Error e ->
      alert (string_of_error e);
      Lwt.return_none
  | Ok (tt, _) -> (
      let step = tt.step in
      match tt.mode with
      | `Basic x ->
          let all_trustee =
            List.map (cast_bt_trustee !&G.to_string !&G.Zq.to_string) x.trustees
          in
          let mode = `Basic in
          Lwt.return_some (ifmatch, step, mode, all_trustee)
      | `Threshold x ->
          let all_trustee =
            List.map (cast_tt_trustee !&G.to_string !&G.Zq.to_string) x.trustees
          in
          let mode = `Threshold (Option.value ~default:0 x.threshold) in
          Lwt.return_some (ifmatch, step, mode, all_trustee))

let recompute_main_zone_1 ifmatch mode all_trustee =
  let open (val !Belenios_js.I18n.gettext) in
  let@ uuid cont =
    let* status = Cache.get_until_success Cache.e_status in
    match status.trustees with
    | None -> Lwt.return [ txt "no trustees" ]
    | Some x -> cont x
  in
  let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
  let { version; group; _ } : raw_draft = draft in
  let module G = (val Group.make { version; group }) in
  let erase_trustee_elt t =
    let onclick () =
      let* b = send_trustees_request uuid (module G) (`RemoveTrustee t) in
      if b then !update_main_zone () else Lwt.return_unit
    in
    div ~a:[ a_class [ "del_sym" ]; a_onclick_lwt onclick ] []
  in
  let header_row =
    tr
      [
        th [ txt @@ s_ "Trustee's e-mail" ];
        th [ txt @@ s_ "Trustee's public name" ];
        th [];
      ]
  in
  let first_row =
    tr
      [
        td
          ~a:[ a_class [ "emph" ] ]
          [ txt @@ s_ "The server is always one of the trustees" ];
        td [ em [ txt "(server)" ] ];
        td [];
      ]
  in
  let rows_of_ttees =
    first_row
    :: List.map
         (fun (t : _ trustee) ->
           let address = Option.value ~default:"N/A" t.address in
           tr
             [
               td [ txt address ];
               td [ txt t.name ];
               td ~a:[ a_class [ "clickable" ] ] [ erase_trustee_elt address ];
             ])
         all_trustee
  in
  let add_form =
    let lab1 =
      label ~a:[ a_label_for "inp1" ] [ txt @@ s_ "Trustee's e-mail " ]
    in
    let inp1, inp1_get = input ~a:[ a_id "inp1" ] `Text in
    let lab2 =
      label ~a:[ a_label_for "inp2" ] [ txt @@ s_ "Trustee's public name " ]
    in
    let inp2, inp2_get = input ~a:[ a_id "inp2" ] `Text in
    let cancel_but =
      button (s_ "Cancel") (fun () ->
          let* () =
            let&&* d = document##getElementById (Js.string "popup") in
            Lwt.return (d##.style##.display := Js.string "none")
          in
          Lwt.return_unit)
    in
    let add_but =
      button (s_ "Add a trustee") (fun () ->
          let t : addable_trustee =
            { address = inp1_get (); name = inp2_get () }
          in
          let r = `AddTrustee t in
          let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
          let { version; group; _ } : raw_draft = draft in
          let module G = (val Group.make { version; group }) in
          let* b = send_trustees_request ?ifmatch uuid (module G) r in
          let&&* d = document##getElementById (Js.string "popup") in
          d##.style##.display := Js.string "none";
          if b then !update_main_zone () else Lwt.return_unit)
    in
    [
      div
        ~a:[ a_id "add_trustee_popup" ]
        [ div [ lab1; inp1 ]; div [ lab2; inp2 ]; div [ cancel_but; add_but ] ];
    ]
  in
  let add_symbol =
    let onclick () =
      let* () =
        let&&* container =
          document##getElementById (Js.string "popup-content")
        in
        show_in container (fun () -> Lwt.return add_form)
      in
      let* () =
        let&&* d = document##getElementById (Js.string "popup") in
        Lwt.return (d##.style##.display := Js.string "block")
      in
      Lwt.return_unit
    in
    let dd =
      div ~a:[ a_class [ "ins_sym clickable" ]; a_onclick_lwt onclick ] []
    in
    div
      ~a:[ a_class [ "new_trustee" ] ]
      [
        div [ txt @@ s_ "Add a trustee" ];
        div ~a:[ a_class [ "d_i_side" ] ] [ dd ];
      ]
  in
  let thresh =
    let with_thr = mode <> `Basic in
    let attr = [ a_id "thresh"; a_class [ "clickable" ] ] in
    let attr = if with_thr then a_checked () :: attr else attr in
    let inp, _ =
      let onchange r =
        (* FIXME: the API should allow to change the mode without resetting the trustee list *)
        let ok =
          if all_trustee <> [] then
            let confirm =
              confirm
              @@ s_ "Warning, this will delete the current list of trustees"
            in
            if not confirm then false else true
          else true
        in
        if ok then
          let@ () = Lwt.async in
          let with_thr = not with_thr in
          let mm = if with_thr then `SetThreshold 0 else `SetBasic in
          let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
          let { version; group; _ } : raw_draft = draft in
          let module G = (val Group.make { version; group }) in
          let* b = send_trustees_request ?ifmatch uuid (module G) mm in
          if b then !update_main_zone () else Lwt.return_unit
        else r##.checked := Js.bool with_thr
      in
      input ~a:attr ~onchange `Checkbox
    in
    let lab =
      label ~a:[ a_label_for "thresh" ] [ txt @@ s_ "Threshold mode" ]
    in
    if with_thr then
      let nth = List.length all_trustee in
      let attr =
        [
          a_input_max (`Number (nth - 1));
          a_input_min (`Number 0);
          a_id "thresh_val";
        ]
      in
      let v = match mode with `Basic -> assert false | `Threshold i -> i in
      let inp_thval, _ =
        let onchange r =
          let vv = int_of_string (Js.to_string r##.value) in
          let mm = `SetThreshold vv in
          let@ () = Lwt.async in
          let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
          let { version; group; _ } : raw_draft = draft in
          let module G = (val Group.make { version; group }) in
          let* b = send_trustees_request ?ifmatch uuid (module G) mm in
          if b then !update_main_zone () else Lwt.return_unit
        in
        input ~a:attr ~onchange ~value:(string_of_int v) `Number
      in
      let lab_thval =
        label
          ~a:[ a_label_for "thresh_val" ]
          [
            txt @@ s_ "out of " ^ string_of_int nth
            ^ s_ " (server is not counted)";
          ]
      in
      div [ inp; lab; inp_thval; lab_thval ]
    else div [ inp; lab ]
  in
  let proc_but =
    let@ () = button (s_ "Proceed to key generation") in
    match mode with
    | `Threshold 0 ->
        alert "Threshold has not been set";
        Lwt.return_unit
    | _ ->
        (* TODO: are there more consistency checks to do, here? *)
        let confirm =
          confirm
          @@ s_ "Proceed to next step? This will freeze the list of trustees."
        in
        if confirm then
          let* b = send_trustees_request uuid (module G) @@ `SetStep 2 in
          if b then !update_main_zone () else Lwt.return_unit
        else Lwt.return_unit
  in
  Lwt.return
    [
      h2 [ txt @@ s_ "Trustee setup - Step 1: choose the trustees" ];
      tablex [ tbody (header_row :: rows_of_ttees) ];
      add_symbol;
      thresh;
      div ~a:[ a_id "trustee_proc_but" ] [ proc_but ];
    ]

let reset_but uuid w =
  let open (val !Belenios_js.I18n.gettext) in
  let@ () = button @@ s_ "Reset and start from scratch" in
  let confir = confirm @@ s_ "Are you sure you want to restart from scratch?" in
  if confir then
    let* b = send_trustees_request uuid w `Reset in
    if b then !update_main_zone () else Lwt.return_unit
  else Lwt.return_unit

let validate_but uuid w =
  let open (val !Belenios_js.I18n.gettext) in
  let@ () = button ~a:[ a_id "validate_board" ] @@ s_ "Validate board" in
  let confir =
    confirm
    @@ s_
         "Are you sure you want to validate the board? Changes will no longer \
          be possible."
  in
  if confir then
    let* b = send_trustees_request uuid w `Validate in
    if b then !update_main_zone () else Lwt.return_unit
  else Lwt.return_unit

let new_board_but () =
  let open (val !Belenios_js.I18n.gettext) in
  let@ () = button ~a:[ a_id "new_board" ] @@ s_ "Create a new board" in
  let election_uuid = get_current_uuid () in
  let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
  let { version; group; _ } : raw_draft = draft in
  let* x = Api.(post trustees_ !user { version; group }) in
  match x.code with
  | 200 -> (
      let trustees_uuid = Uuid.of_string x.content in
      let* x =
        Api.(post (draft election_uuid) !user)
          (`SetTrustees (Some trustees_uuid))
      in
      match x.code with
      | 200 ->
          Cache.invalidate Cache.e_status;
          !update_main_zone ()
      | code ->
          let msg =
            Printf.sprintf (f_ "Setting board failed with code %d!") code
          in
          alert msg;
          Lwt.return_unit)
  | code ->
      let msg =
        Printf.sprintf (f_ "Creating board failed with code %d!") code
      in
      alert msg;
      Lwt.return_unit

(* FIXME: This step 3 is just a dumb, now, but in the future, it should be
 * a page to check that the trustees have their secret key *)
let recompute_main_zone_3 all_trustee =
  let open (val !Belenios_js.I18n.gettext) in
  let* reset =
    let@ uuid cont =
      let* status = Cache.get_until_success Cache.e_status in
      match status.trustees with None -> Lwt.return_nil | Some x -> cont x
    in
    let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
    let { version; group; _ } : raw_draft = draft in
    let module G = (val Group.make { version; group }) in
    let* x = Api.(get (trustees uuid (module G)) `Nobody) in
    match x with
    | Error _ ->
        Lwt.return
          [ reset_but uuid (module G); txt " "; validate_but uuid (module G) ]
    | Ok _ -> Lwt.return [ new_board_but () ]
  in
  let header_row =
    tr
      [
        th [ txt @@ s_ "Trustee's e-mail" ];
        th [ txt @@ s_ "Trustee's public name" ];
        th [];
      ]
  in
  let first_row =
    tr
      [
        td
          ~a:[ a_class [ "emph" ] ]
          [ txt @@ s_ "The server is always one of the trustees" ];
        td [ em [ txt "(server)" ] ];
        td [];
      ]
  in
  let rows_of_ttees =
    first_row
    :: List.map
         (fun (t : _ trustee) ->
           let address = Option.value ~default:"N/A" t.address in
           tr [ td [ txt address ]; td [ txt t.name ]; td [] ])
         all_trustee
  in
  Lwt.return
    [
      h2 [ txt @@ s_ "Trustee setup - Done" ];
      tablex [ tbody (header_row :: rows_of_ttees) ];
      div reset;
    ]

(* trustee status is just an int. Conversion to a meaningful status
 * depends on the mode (threshold or not)
 *)
let string_of_state mode st =
  let open (val !Belenios_js.I18n.gettext) in
  match mode with
  | `Basic -> (
      match st with
      | None -> s_ "none"
      | Some 0 -> s_ "action required"
      | Some 1 -> s_ "done"
      | _ -> assert false)
  | `Threshold _ -> (
      match st with
      | None -> s_ "none"
      | Some 0 -> s_ "step 0?" (* should not occur if threshold is set *)
      | Some ((1 | 2 | 3 | 4 | 5 | 6) as s) -> Printf.sprintf (f_ "step %d/7") s
      | Some 7 -> "done"
      | _ -> assert false)

module Mails = Belenios_ui.Mails_admin.Make (Belenios_js.I18n)

let trustee_generate_link kind =
  let generate_mail =
    match kind with
    | `Basic -> Mails.mail_trustee_generation_basic
    | `Threshold -> Mails.mail_trustee_generation_threshold
  in
  fun ~token ~recipient ->
    let open (val !Belenios_js.I18n.gettext) in
    let@ uuid cont =
      let* status = Cache.get_until_success Cache.e_status in
      match status.trustees with None -> assert false | Some x -> cont x
    in
    let* prefix = Cache.get_prefix () in
    let href =
      Printf.sprintf "%strustee#%s/%s" prefix (Uuid.to_string uuid) token
    in
    let* subject, body = generate_mail [ lang ] href in
    Lwt.return
    @@ span
         ~a:[ a_class [ "trustee-links" ] ]
         [
           a
             ~a:[ a_class [ "trustee-link"; "trustee-generate-link" ] ]
             ~href (s_ "Direct link");
           br ();
           a_mailto
             ~a:[ a_class [ "trustee-link" ] ]
             ~recipient ~subject ~body (s_ "Send an e-mail");
         ]

let all_ttee_done mode all_trustee =
  let dd = if mode = `Basic then Some 1 else Some 7 in
  List.for_all (fun (t : _ trustee) -> t.state = dd) all_trustee

let trustee_decrypt_link ~trustees ~election ~token ~recipient =
  let open (val !Belenios_js.I18n.gettext) in
  let* prefix = Cache.get_prefix () in
  let href =
    Printf.sprintf "%strustee#%s/%s/%s" prefix (Uuid.to_string trustees) token
      (Uuid.to_string election)
  in
  let* subject, body = Mails.mail_trustee_tally [ lang ] href in
  Lwt.return
  @@ span
       ~a:[ a_class [ "trustee-links" ] ]
       [
         a
           ~a:[ a_class [ "trustee-link"; "trustee-decrypt-link" ] ]
           ~href (s_ "Direct link");
         br ();
         a_mailto
           ~a:[ a_class [ "trustee-link" ] ]
           ~recipient ~subject ~body (s_ "Send an e-mail");
       ]

let recompute_main_zone_2 ifmatch_tt mode all_trustee =
  let open (val !Belenios_js.I18n.gettext) in
  let confir =
    if all_trustee = [] then
      confirm
      @@ s_
           "No external trustee were set. Do you confirm that you trust the \
            server for privacy?"
    else true
  in
  let@ uuid cont =
    let* status = Cache.get_until_success Cache.e_status in
    match status.trustees with
    | None -> Lwt.return [ txt "no trustees" ]
    | Some x -> cont x
  in
  let* (Draft (_, draft)) = Cache.get_until_success Cache.draft in
  let { version; group; _ } : raw_draft = draft in
  let module G = (val Group.make { version; group }) in
  if not confir then
    let* _ = send_trustees_request uuid (module G) @@ `SetStep 1 in
    recompute_main_zone_1 ifmatch_tt mode all_trustee
  else if all_ttee_done mode all_trustee then
    let* _ = send_trustees_request uuid (module G) @@ `SetStep 3 in
    recompute_main_zone_3 all_trustee
  else
    let trustee_generate_link =
      let kind =
        match mode with `Basic -> `Basic | `Threshold _ -> `Threshold
      in
      trustee_generate_link kind
    in
    let header_row =
      tr
        [
          th [ txt @@ s_ "Trustee's e-mail" ];
          th [ txt @@ s_ "Trustee's name" ];
          th [ txt @@ s_ "Link to send" ];
          th [ txt @@ s_ "Status" ];
          th [];
        ]
    in
    let* rows_of_ttees =
      Lwt_list.map_s
        (fun (t : _ trustee) ->
          let* link =
            trustee_generate_link
              ~token:(Option.value ~default:"" t.token)
              ~recipient:(Option.value ~default:"" t.address)
          in
          tr
            [
              td [ txt @@ Option.value ~default:"N/A" t.address ];
              td [ txt t.name ];
              td [ link ];
              td [ txt @@ string_of_state mode t.state ];
              td [];
            ]
          |> Lwt.return)
        all_trustee
    in
    let refresh_but =
      button (s_ "Refresh status") (fun () -> !update_main_zone ())
    in
    Lwt.return
      [
        h2 [ txt @@ s_ "Trustee setup - Step 2: key generation" ];
        div
          [
            em
              [
                txt
                @@ s_
                     "You must send each trustee their link (from the table \
                      below) so that they initialize their key for this \
                      election.";
              ];
          ];
        tablex [ tbody (header_row :: rows_of_ttees) ];
        div [ refresh_but ];
        div [ reset_but uuid (module G) ];
      ]

(* This function will be used with `FinishShuffling and `ReleaseTally *)
let trustee_request req =
  let uuid = get_current_uuid () in
  Cache.invalidate Cache.e_status;
  let* status = Cache.get_until_success Cache.e_status in
  let ifmatch = Some (sha256_b64 @@ !+yojson_of_election_status status) in
  let* x = Api.(post ?ifmatch (election_status uuid) !user req) in
  match x.code with
  | 200 ->
      Cache.invalidate Cache.e_status;
      Lwt.return_unit
  | code ->
      alert ("Failed with code " ^ string_of_int code);
      Lwt.return_unit

let part_dec = ref (None : partial_decryptions option)

let all_pd () =
  match !part_dec with
  | None -> false
  | Some x ->
      List.length x.trustees
      = List.fold_left
          (fun acc z -> if z.done_ then acc + 1 else acc)
          0 x.trustees

let enough_pd () =
  match !part_dec with
  | None -> false
  | Some x ->
      let th =
        match x.threshold with None -> List.length x.trustees | Some t -> t
      in
      th
      <= List.fold_left
           (fun acc z -> if z.done_ then acc + 1 else acc)
           0 x.trustees

let get_trustees_pd () =
  let uuid = get_current_uuid () in
  let* x = Api.(get (election_partial_decryptions uuid) !user) in
  match x with
  | Error e ->
      alert (string_of_error e);
      Lwt.return_unit
  | Ok (tt, _) ->
      part_dec := Some tt;
      Lwt.return_unit

let main_zone_tallying () =
  let open (val !Belenios_js.I18n.gettext) in
  let election = get_current_uuid () in
  let@ trustees cont =
    let* status = Cache.get_until_success Cache.e_status in
    match status.trustees with None -> Lwt.return [] | Some x -> cont x
  in
  let* content =
    match !part_dec with
    | None -> Lwt.return @@ div [ txt @@ s_ "Failed to connect; please reload" ]
    | Some x ->
        let tl = x.trustees in
        let header_row =
          tr
            [
              th [ txt @@ s_ "Trustee's e-mail" ];
              th [ txt @@ s_ "Link to send" ];
              th [ txt @@ s_ "Done?" ];
              th [];
            ]
        in
        let* rows_of_ttees =
          Lwt_list.filter_map_s
            (fun (t : trustee_pd) ->
              if t.address = "server" then Lwt.return_none
              else
                let* link =
                  trustee_decrypt_link ~trustees ~election ~token:t.token
                    ~recipient:t.address
                in
                Lwt.return_some
                @@ tr
                     [
                       td [ txt t.address ];
                       td [ link ];
                       td [ (txt @@ if t.done_ then "yes" else "no") ];
                     ])
            tl
        in
        let refresh_but =
          button (s_ "Refresh status") (fun () ->
              Cache.invalidate Cache.e_status;
              !update_main_zone ())
        in
        let release_but =
          if not (enough_pd ()) then div []
          else
            button (s_ "Release tally") (fun () ->
                let* () = trustee_request `ReleaseTally in
                !update_main_zone ())
        in
        Lwt.return
        @@ div
             [
               div
                 [
                   em
                     [
                       txt
                       @@ s_
                            "You must send each trustee their link (from the \
                             table below) so that they perform their share of \
                             the decryption.";
                     ];
                 ];
               tablex [ tbody (header_row :: rows_of_ttees) ];
               refresh_but;
               release_but;
             ]
  in
  Lwt.return [ h2 [ txt @@ s_ "Tallying" ]; content ]

let shuffles = ref None

let get_shuffles uuid =
  let* x = Api.(get (election_shuffles uuid) !user) in
  match x with
  | Error e ->
      alert (string_of_error e);
      Lwt.return_none
  | Ok (tt, _) -> Lwt.return_some tt

let update_shuffles () =
  let uuid = get_current_uuid () in
  let* x = get_shuffles uuid in
  match x with
  | None -> Lwt.return_unit
  | Some x ->
      shuffles := Some x;
      Lwt.return_unit

let ready_to_decrypt () =
  match !shuffles with
  | None -> false
  | Some sh ->
      List.for_all
        (fun (t : shuffler) -> t.trustee = None || t.fingerprint <> None)
        sh.shufflers

let trustee_shuffle_link ~token ~recipient =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = get_current_uuid () in
  let* prefix = Cache.get_prefix () in
  let href =
    Printf.sprintf "%strustee#%s/%s" prefix (Uuid.to_string uuid) token
  in
  let* subject, body = Mails.mail_shuffle [ lang ] href in
  Lwt.return
  @@ span
       ~a:[ a_class [ "trustee-links" ] ]
       [
         a
           ~a:[ a_class [ "trustee-link"; "trustee-shuffle-link" ] ]
           ~href (s_ "Direct link");
         br ();
         a_mailto
           ~a:[ a_class [ "trustee-link" ] ]
           ~recipient ~subject ~body (s_ "Send an e-mail");
       ]

let main_zone_shuffling () =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = get_current_uuid () in
  let* content =
    match !shuffles with
    | None -> Lwt.return @@ div [ txt @@ s_ "Failed to connect; please reload" ]
    | Some x ->
        let sl = x.shufflers in
        let header_row =
          tr
            [
              th [ txt @@ s_ "Shuffler's e-mail" ];
              th [];
              (* select and skip buttons / link to send *)
              th [ txt @@ s_ "Done?" ];
              th [];
            ]
        in
        let sel_exists =
          List.exists
            (fun (t : shuffler) ->
              match t.trustee with Some (_, true) -> true | _ -> false)
            sl
        in
        let* sel_but_list =
          Lwt_list.map_s
            (fun (t : shuffler) ->
              match t.trustee with
              | None -> Lwt.return []
              | Some (trustee, true) ->
                  let* link =
                    trustee_shuffle_link ~token:trustee.token
                      ~recipient:trustee.address
                  in
                  Lwt.return [ link ]
              | Some (trustee, false) ->
                  let attr =
                    if sel_exists || t.fingerprint <> None then
                      [ a_disabled () ]
                    else []
                  in
                  let make_but lab req =
                    button ~a:attr lab (fun () ->
                        let* x =
                          Api.(
                            post
                              (election_shuffle uuid trustee.address)
                              !user req)
                        in
                        match x.code with
                        | 200 -> !update_main_zone ()
                        | code ->
                            alert ("Failed with code " ^ string_of_int code);
                            Lwt.return_unit)
                  in
                  let but1 = make_but (s_ "Select this trustee") `Select in
                  let but2 = make_but (s_ "Skip") `Skip in
                  Lwt.return [ but1; but2 ])
            sl
        in
        let rows_of_sh =
          List.map2
            (fun (t : shuffler) b ->
              match t.trustee with
              | None ->
                  tr
                    [
                      td
                        ~a:[ a_class [ "emph" ] ]
                        [ txt @@ s_ "server (has already shuffled)" ];
                      td [];
                      td [];
                      td [];
                    ]
              | Some (trustee, _) ->
                  tr
                    [
                      td [ txt trustee.address ];
                      td b;
                      td
                        [
                          (txt
                          @@
                          match t.fingerprint with
                          | Some None -> s_ "skipped"
                          | Some _ -> s_ "yes"
                          | _ -> "no");
                        ];
                      td [];
                    ])
            sl sel_but_list
        in
        let refresh_but =
          button (s_ "Refresh status") (fun () ->
              Cache.invalidate Cache.e_status;
              !update_main_zone ())
        in
        let finish_but =
          button (s_ "Skip all remaining trustees") (fun () ->
              let* () = trustee_request `FinishShuffling in
              !update_main_zone ())
        in
        Lwt.return
        @@ div
             [
               div
                 [
                   em
                     [
                       txt
                       @@ s_
                            "You must select each trustee in turn, and send \
                             them a link so that they perform their shuffle. \
                             You can also skip a trustee, but keep in mind \
                             that this reduces security.";
                     ];
                 ];
               tablex [ tbody (header_row :: rows_of_sh) ];
               div [ refresh_but ];
               div [ finish_but ];
             ]
  in
  Lwt.return [ h2 [ txt @@ s_ "Tallying: shuffling step" ]; content ]

let import_but () =
  let open (val !Belenios_js.I18n.gettext) in
  let@ () = button @@ s_ "Use board from another election" in
  let to_uuid = get_current_uuid () in
  let@ from_uuid = popup_choose_elec to_uuid in
  let* x = Api.(post (draft to_uuid) !user) @@ `ImportTrustees from_uuid in
  match x.code with
  | 200 ->
      Cache.invalidate_all ();
      !update_main_zone ()
  | code ->
      let open (val !Belenios_js.I18n.gettext) in
      let msg = Printf.sprintf (f_ "Failed with error %d!") code in
      alert msg;
      Lwt.return_unit

let recompute_main_zone () =
  let open (val !Belenios_js.I18n.gettext) in
  let checkpriv () =
    let uuid = get_current_uuid () in
    let href = "trustee#check/" ^ Uuid.to_string uuid in
    let label = s_ "Check private key ownership" in
    [
      h2 [ txt label ];
      div
        [
          txt
          @@ s_
               "The following link can be used by trustees to check that they \
                own the right decryption key.";
        ];
      ul [ li [ a ~href label ] ];
    ]
  in
  let* content, show_checkpriv =
    if is_draft () then
      let* x = get_trustees () in
      match x with
      | None ->
          let content =
            [
              div [ txt @@ s_ "There is no electoral board at the moment." ];
              div [ new_board_but (); txt " "; import_but () ];
            ]
          in
          Lwt.return (content, false)
      | Some (ifmatch_tt, step, mode, all_trustee) -> (
          let@ () =
           fun cont ->
            let* r = cont () in
            Lwt.return (r, step >= 3 && all_trustee <> [])
          in
          match step with
          | 1 -> recompute_main_zone_1 ifmatch_tt mode all_trustee
          | 2 -> recompute_main_zone_2 ifmatch_tt mode all_trustee
          | 3 -> recompute_main_zone_3 all_trustee
          | _ ->
              alert "Should not get there; aborting.";
              assert false)
    else
      let* status = Cache.get_until_success Cache.e_status in
      match status.state with
      | `EncryptedTally ->
          let* () = get_trustees_pd () in
          if all_pd () then
            let* () = trustee_request `ReleaseTally in
            Lwt.return ([], false)
          else
            let* r = main_zone_tallying () in
            Lwt.return (r, false)
      | `Shuffling ->
          let* () = update_shuffles () in
          if ready_to_decrypt () then
            let* () = trustee_request `FinishShuffling in
            let* () = get_trustees_pd () in
            let* r = main_zone_tallying () in
            Lwt.return (r, false)
          else
            let* r = main_zone_shuffling () in
            Lwt.return (r, false)
      | _ -> Lwt.return ([], false)
  in
  match content with
  | [] ->
      Lwt.return
      @@ [
           div
             [
               em
                 [
                   txt
                   @@ s_ "Nothing to show here. Try going to the Status tab.";
                 ];
             ];
         ]
  | _ ->
      let checkpriv = if show_checkpriv then checkpriv () else [] in
      Lwt.return @@ List.flatten [ content; checkpriv ]

let () =
  update_main_zone :=
    fun () ->
      let* content = recompute_main_zone () in
      let&&* container = document##getElementById (Js.string "main_zone") in
      show_in container (fun () -> Lwt.return content)

(* Called from the outside.
 * Returns stuff to be put in the main zone.
 *)
let trustees_content () =
  let* content = recompute_main_zone () in
  Lwt.return content
