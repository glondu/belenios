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
open Belenios_api.Serializable_j
open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session
open Common

let send_draft_request req =
  let uuid = get_current_uuid () in
  let* x = post_with_token (string_of_draft_request req) "drafts/%s" uuid in
  if x.code <> 200 then
    alert ("Draft request failed with error code " ^ string_of_int x.code);
  Cache.invalidate Cache.status;
  Lwt.return_unit

type trustees_mode = [ `Basic | `Threshold of int ]

let all_trustee = ref ([] : Yojson.Safe.t trustee list)
let ifmatch_tt = ref (Some "")
let mode = ref (`Basic : trustees_mode)
let step = ref 0

(* Forward decl of update functions *)
let update_main_zone = ref (fun _ -> Lwt.return_unit)

let cast_bt_trustee x =
  x
  |> string_of_trustee
       (write_trustee_public_key Yojson.Safe.write_json Yojson.Safe.write_json)
  |> trustee_of_string Yojson.Safe.read_json

let cast_tt_trustee x =
  x
  |> string_of_trustee (write_cert Yojson.Safe.write_json)
  |> trustee_of_string Yojson.Safe.read_json

let get_trustees () =
  let uuid = get_current_uuid () in
  let* status = Cache.get_until_success Cache.status in
  step := status.trustees_setup_step;
  let* x =
    get
      (draft_trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json)
      "drafts/%s/trustees" uuid
  in
  ifmatch_tt := get_ifmatch x;
  match x with
  | Error e ->
      alert (string_of_error e);
      Lwt.return_unit
  | Ok (tt, _) -> (
      match tt with
      | `Basic x ->
          all_trustee := List.map cast_bt_trustee x.bt_trustees;
          mode := `Basic;
          Lwt.return_unit
      | `Threshold x ->
          all_trustee := List.map cast_tt_trustee x.tt_trustees;
          mode := `Threshold (Option.value ~default:0 x.tt_threshold);
          Lwt.return_unit)

let recompute_main_zone_1 () =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = get_current_uuid () in
  let erase_trustee_elt t =
    let encoded_trustee =
      t |> Js.string |> Js.encodeURIComponent |> Js.to_string
    in
    let elt = div ~a:[ a_class [ "del_sym" ] ] [] in
    let r = Tyxml_js.To_dom.of_div elt in
    r##.onclick :=
      lwt_handler (fun () ->
          let* x =
            delete_with_token "drafts/%s/trustees/%s" uuid encoded_trustee
          in
          match x.code with
          | 200 -> !update_main_zone ()
          | code ->
              alert ("Deletion failed with code " ^ string_of_int code);
              Lwt.return_unit);
    elt
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
        td [ txt "server" ];
        td [];
      ]
  in
  let rows_of_ttees =
    first_row
    :: List.map
         (fun t ->
           let address = Option.value ~default:"@" t.trustee_address in
           tr
             [
               td [ txt address ];
               td [ txt t.trustee_name ];
               td ~a:[ a_class [ "clickable" ] ] [ erase_trustee_elt address ];
             ])
         !all_trustee
  in
  let add_form =
    let lab1 =
      label ~a:[ a_label_for "inp1" ] [ txt @@ s_ "Trustee's e-mail " ]
    in
    let inp1, inp1_get = input ~a:[ a_id "inp1" ] "" in
    let lab2 =
      label ~a:[ a_label_for "inp2" ] [ txt @@ s_ "Trustee's public name " ]
    in
    let inp2, inp2_get = input ~a:[ a_id "inp2" ] "" in
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
          let t =
            {
              trustee_address = Some (inp1_get ());
              trustee_name = inp2_get ();
              trustee_token = None;
              trustee_state = None;
              trustee_key = None;
            }
          in
          let r = `Add t in
          let* x =
            post_with_token ?ifmatch:!ifmatch_tt
              (string_of_trustees_request r)
              "drafts/%s/trustees" uuid
          in
          let&&* d = document##getElementById (Js.string "popup") in
          d##.style##.display := Js.string "none";
          match x.code with
          | 200 -> !update_main_zone ()
          | code ->
              alert ("Add failed with code " ^ string_of_int code);
              Lwt.return_unit)
    in
    [
      div
        ~a:[ a_id "add_trustee_popup" ]
        [ div [ lab1; inp1 ]; div [ lab2; inp2 ]; div [ cancel_but; add_but ] ];
    ]
  in
  let add_symbol =
    let dd = div ~a:[ a_class [ "ins_sym clickable" ] ] [] in
    let r = Tyxml_js.To_dom.of_div dd in
    r##.onclick :=
      lwt_handler (fun () ->
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
          Lwt.return_unit);
    div
      ~a:[ a_class [ "new_trustee" ] ]
      [
        div [ txt @@ s_ "Add a trustee" ];
        div ~a:[ a_class [ "d_i_side" ] ] [ dd ];
      ]
  in
  let thresh =
    let with_thr = !mode <> `Basic in
    let attr =
      [ a_id "thresh"; a_class [ "clickable" ]; a_input_type `Checkbox ]
    in
    let attr = if with_thr then a_checked () :: attr else attr in
    let inp, _ = input ~a:attr "" in
    let r = Tyxml_js.To_dom.of_input inp in
    r##.onchange :=
      lwt_handler (fun _ ->
          (* FIXME: the API should allow to change the mode without resetting the trustee list *)
          let ok =
            if !all_trustee <> [] then
              let confirm =
                confirm
                @@ s_ "Warning, this will delete the current list of trustees"
              in
              if not confirm then false else true
            else true
          in
          if ok then (
            let with_thr = not with_thr in
            let mm =
              string_of_trustees_request
                (if with_thr then `SetThreshold 0 else `SetBasic)
            in
            let ifmatch = !ifmatch_tt in
            let* x = post_with_token ?ifmatch mm "drafts/%s/trustees" uuid in
            match x.code with
            | 200 -> !update_main_zone ()
            | _ ->
                alert "Error";
                Lwt.return_unit)
          else (
            r##.checked := Js.bool with_thr;
            Lwt.return_unit));
    let lab =
      label ~a:[ a_label_for "thresh" ] [ txt @@ s_ "Threshold mode" ]
    in
    if with_thr then (
      let nth = List.length !all_trustee in
      let attr =
        [
          a_input_type `Number;
          a_input_max (`Number (nth - 1));
          a_input_min (`Number 0);
          a_id "thresh_val";
        ]
      in
      let v = match !mode with `Basic -> assert false | `Threshold i -> i in
      let inp_thval, _ = input ~a:attr (string_of_int v) in
      let r = Tyxml_js.To_dom.of_input inp_thval in
      r##.onchange :=
        lwt_handler (fun _ ->
            let vv = int_of_string (Js.to_string r##.value) in
            let mm = string_of_trustees_request (`SetThreshold vv) in
            let ifmatch = !ifmatch_tt in
            let* x = post_with_token ?ifmatch mm "drafts/%s/trustees" uuid in
            match x.code with
            | 200 -> !update_main_zone ()
            | _ ->
                alert "Error";
                Lwt.return_unit);
      let lab_thval =
        label
          ~a:[ a_label_for "thresh_val" ]
          [
            txt @@ s_ "out of " ^ string_of_int nth
            ^ s_ " (server is not counted)";
          ]
      in
      div [ inp; lab; inp_thval; lab_thval ])
    else div [ inp; lab ]
  in
  let proc_but =
    let@ () = button (s_ "Proceed to key generation") in
    match !mode with
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
          let* () = send_draft_request @@ `SetTrusteesSetupStep 2 in
          !update_main_zone ()
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

(* FIXME: This step 3 is just a dumb, now, but in the future, it should be
 * a page to check that the trustees have their secret key *)
let recompute_main_zone_3 () =
  let open (val !Belenios_js.I18n.gettext) in
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
        td [ txt "server" ];
        td [];
      ]
  in
  let rows_of_ttees =
    first_row
    :: List.map
         (fun t ->
           let address = Option.value ~default:"@" t.trustee_address in
           tr [ td [ txt address ]; td [ txt t.trustee_name ]; td [] ])
         !all_trustee
  in
  Lwt.return
    [
      h2 [ txt @@ s_ "Trustee setup - Done" ];
      tablex [ tbody (header_row :: rows_of_ttees) ];
    ]

(* trustee status is just an int. Conversion to a meaningful status
 * depends on the mode (threshold or not)
 *)
let string_of_state st =
  let open (val !Belenios_js.I18n.gettext) in
  match !mode with
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
      | Some 1 | Some 2 -> s_ "step 1 / 3"
      | Some 3 | Some 4 -> s_ "step 2 / 3"
      | Some 5 | Some 6 -> s_ "step 3 / 3"
      | Some 7 -> "done"
      | _ -> assert false)

let maillink_of_token tk =
  let uuid = get_current_uuid () in
  let* prefix = Cache.get_prefix () in
  let href = Printf.sprintf "%strustee#generate/%s/%s" prefix uuid tk in
  Lwt.return @@ a ~href "Link"

let all_ttee_done () =
  let dd = if !mode = `Basic then Some 1 else Some 7 in
  List.for_all (fun t -> t.trustee_state = dd) !all_trustee

let maillink_of_token_direct tk =
  let uuid = get_current_uuid () in
  let* prefix = Cache.get_prefix () in
  let href = Printf.sprintf "%strustee#decrypt/%s/%s" prefix uuid tk in
  Lwt.return @@ a ~href "Link"

let recompute_main_zone_2 () =
  let open (val !Belenios_js.I18n.gettext) in
  let confir =
    if !all_trustee = [] then
      confirm
      @@ s_
           "No external trustee were set. Do you confirm that you trust the \
            server for privacy?"
    else true
  in
  if not confir then (
    let* () = send_draft_request @@ `SetTrusteesSetupStep 1 in
    step := 1;
    recompute_main_zone_1 ())
  else if all_ttee_done () then (
    let* () = send_draft_request @@ `SetTrusteesSetupStep 3 in
    step := 3;
    recompute_main_zone_3 ())
  else
    (* TODO: put also a mailto link *)
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
        (fun t ->
          let* link =
            maillink_of_token
              (match t.trustee_token with Some x -> x | None -> "")
          in
          tr
            [
              td [ txt @@ Option.value ~default:"" t.trustee_address ];
              td [ txt t.trustee_name ];
              td [ link ];
              td [ txt @@ string_of_state t.trustee_state ];
              td [];
            ]
          |> Lwt.return)
        !all_trustee
    in
    let reset_but =
      button (s_ "Reset and start from scratch") (fun () ->
          let confir =
            confirm @@ s_ "Are you sure you want to restart from scratch?"
          in
          if confir then
            let* () = send_draft_request @@ `SetTrusteesSetupStep 1 in
            !update_main_zone ()
          else Lwt.return_unit)
    in
    let refresh_but =
      button (s_ "Refresh status") (fun () -> !update_main_zone ())
    in
    Lwt.return
      [
        h2 [ txt @@ s_ "Trustee setup - Step 2: key generation" ];
        tablex [ tbody (header_row :: rows_of_ttees) ];
        div [ refresh_but ];
        div [ reset_but ];
      ]

(* This function will be used with `FinishShuffling and `ReleaseTally *)
let trustee_request req =
  let uuid = get_current_uuid () in
  Cache.invalidate Cache.e_status;
  let* status = Cache.get_until_success Cache.e_status in
  let ifmatch = Some (sha256_b64 @@ string_of_election_status status) in
  let* x =
    post_with_token ?ifmatch (string_of_admin_request req) "elections/%s" uuid
  in
  match x.code with
  | 200 ->
      Cache.invalidate Cache.e_status;
      Lwt.return_unit
  | code ->
      alert ("Failed with code " ^ string_of_int code);
      Lwt.return_unit

let part_dec = ref None

let all_pd () =
  match !part_dec with
  | None -> false
  | Some x ->
      List.length x.partial_decryptions_trustees
      = List.fold_left
          (fun acc z -> if z.trustee_pd_done then acc + 1 else acc)
          0 x.partial_decryptions_trustees

let enough_pd () =
  match !part_dec with
  | None -> false
  | Some x ->
      let th =
        match x.partial_decryptions_threshold with
        | None -> List.length x.partial_decryptions_trustees
        | Some t -> t
      in
      th
      <= List.fold_left
           (fun acc z -> if z.trustee_pd_done then acc + 1 else acc)
           0 x.partial_decryptions_trustees

let get_trustees_pd () =
  let uuid = get_current_uuid () in
  let* x =
    get partial_decryptions_of_string "elections/%s/partial-decryptions" uuid
  in
  match x with
  | Error e ->
      alert (string_of_error e);
      Lwt.return_unit
  | Ok (tt, _) ->
      part_dec := Some tt;
      Lwt.return_unit

let main_zone_tallying () =
  let open (val !Belenios_js.I18n.gettext) in
  let* content =
    match !part_dec with
    | None -> Lwt.return @@ div [ txt @@ s_ "Failed to connect; please reload" ]
    | Some x ->
        let tl = x.partial_decryptions_trustees in
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
            (fun t ->
              if t.trustee_pd_address = "server" then Lwt.return_none
              else
                let* link = maillink_of_token_direct t.trustee_pd_token in
                Lwt.return_some
                @@ tr
                     [
                       td [ txt t.trustee_pd_address ];
                       td [ link ];
                       td [ (txt @@ if t.trustee_pd_done then "yes" else "no") ];
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
               tablex [ tbody (header_row :: rows_of_ttees) ];
               refresh_but;
               release_but;
             ]
  in
  Lwt.return [ h2 [ txt @@ s_ "Tallying" ]; content ]

let shuffles = ref None

let get_shuffles uuid =
  let* x = get shuffles_of_string "elections/%s/shuffles" uuid in
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
        (fun t ->
          t.shuffler_address = "server" || t.shuffler_fingerprint <> None)
        sh.shuffles_shufflers

let shuffle_link token =
  let uuid = get_current_uuid () in
  let* prefix = Cache.get_prefix () in
  let href = Printf.sprintf "%strustee#shuffle/%s/%s" prefix uuid token in
  Lwt.return @@ a ~href "Link"

let main_zone_shuffling () =
  let open (val !Belenios_js.I18n.gettext) in
  let uuid = get_current_uuid () in
  let* content =
    match !shuffles with
    | None -> Lwt.return @@ div [ txt @@ s_ "Failed to connect; please reload" ]
    | Some x ->
        let sl = x.shuffles_shufflers in
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
            (fun t -> match t.shuffler_token with Some _ -> true | _ -> false)
            sl
        in
        let* sel_but_list =
          Lwt_list.map_s
            (fun t ->
              match t.shuffler_token with
              | Some token ->
                  let* link = shuffle_link token in
                  Lwt.return [ link ]
              | _ ->
                  let attr =
                    if sel_exists || t.shuffler_fingerprint <> None then
                      [ a_disabled () ]
                    else []
                  in
                  let make_but lab req =
                    button ~a:attr lab (fun () ->
                        let encoded =
                          t.shuffler_address |> Js.string
                          |> Js.encodeURIComponent |> Js.to_string
                        in
                        let* x =
                          post_with_token
                            (string_of_shuffler_request req)
                            "elections/%s/shuffles/%s" uuid encoded
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
            (fun t b ->
              if t.shuffler_address = "server" then
                tr
                  [
                    td
                      ~a:[ a_class [ "emph" ] ]
                      [ txt @@ s_ "server (has already shuffled)" ];
                    td [];
                    td [];
                    td [];
                  ]
              else
                tr
                  [
                    td [ txt t.shuffler_address ];
                    td b;
                    td
                      [
                        (txt
                        @@
                        match t.shuffler_fingerprint with
                        | Some "" -> s_ "skipped"
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
               tablex [ tbody (header_row :: rows_of_sh) ];
               div [ refresh_but ];
               div [ finish_but ];
             ]
  in
  Lwt.return [ h2 [ txt @@ s_ "Tallying: shuffling step" ]; content ]

let recompute_main_zone () =
  let open (val !Belenios_js.I18n.gettext) in
  let checkpriv =
    let uuid = get_current_uuid () in
    let href = "trustee#check/" ^ uuid in
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
  let* content =
    if is_draft () then (
      let* () = get_trustees () in
      match !step with
      | 1 -> recompute_main_zone_1 ()
      | 2 -> recompute_main_zone_2 ()
      | 3 -> recompute_main_zone_3 ()
      | _ ->
          alert "Should not get there; aborting.";
          assert false)
    else
      let* status = Cache.get_until_success Cache.e_status in
      match status.status_state with
      | `EncryptedTally ->
          let* () = get_trustees_pd () in
          if not (all_pd ()) then main_zone_tallying ()
          else
            let* () = trustee_request `ReleaseTally in
            Lwt.return_nil
      | `Shuffling ->
          let* () = update_shuffles () in
          if not (ready_to_decrypt ()) then main_zone_shuffling ()
          else
            let* () = trustee_request `FinishShuffling in
            let* () = get_trustees_pd () in
            main_zone_tallying ()
      | _ -> Lwt.return_nil
  in
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
