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
open Belenios_core.Common
open Belenios_api.Serializable_j
open Belenios_tool_js_common.Tool_js_common
open Belenios_tool_js_common.Tool_js_html
open Common

let rec show main uuid =
  let@ () = show_in main in
  let* x = get (fun x -> x) "elections/%s/election" uuid in
  let@ raw_election = with_ok "election" x in
  let* x = get election_status_of_string "elections/%s" uuid in
  let@ status = with_ok "status" x in
  let make label request =
    let@ () = button label in
    let* x = post_with_token (string_of_admin_request request) "elections/%s" uuid in
    let@ () = show_in main in
    generic_proceed x (fun () -> show main uuid)
  in
  let button_delete =
    let@ () = button "Delete election" in
    if confirm "Are you sure?" then (
      let* x = delete_with_token "elections/%s" uuid in
      let@ () = show_in main in
      let@ () = generic_proceed x in
      Dom_html.window##.location##.hash := Js.string "";
      Lwt.return_unit
    ) else (
      Lwt.return_unit
    )
  in
  let buttons =
    [
      node @@ make "Open election" `Open;
      node @@ make "Close election" `Close;
      node @@ make "Compute encrypted tally" `ComputeEncryptedTally;
      node @@ make "Finish shuffling" `FinishShuffling;
      node @@ make "Release tally" `ReleaseTally;
      node @@ make "Archive election" `Archive;
      node @@ button_delete;
    ]
  in
  let get_date x =
    let y = Js.to_string x##.value in
    if y = "" then None else Some (Js.date##parse x##.value /. 1000.)
  in
  let auto_dates =
    let make_input d =
      let r = input [] in
      let () =
        match d with
        | None -> ()
        | Some x ->
           let x = new%js Js.date_fromTimeValue (x *. 1000.) in
           r##.value := x##toISOString
      in
      r
    in
    let auto_open = make_input status.status_auto_open_date in
    let auto_close = make_input status.status_auto_close_date in
    let set_button =
      let@ () = button "Set automatic dates" in
      let dates =
        {
          auto_date_open = get_date auto_open;
          auto_date_close = get_date auto_close;
        }
      in
      let request = `SetAutomaticDates dates in
      let* x = post_with_token (string_of_admin_request request) "elections/%s" uuid in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [node auto_open; node auto_close; node set_button]
  in
  let regenpwd =
    let i = input [] in
    let set_button =
      let@ () = button "Regenerate a password" in
      let request = `RegeneratePassword (Js.to_string i##.value) in
      let* x = post_with_token (string_of_admin_request request) "elections/%s" uuid in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [node i; node set_button]
  in
  let postpone =
    let i = input [] in
    let set_button =
      let@ () = button "Set postpone date" in
      let request = `SetPostponeDate (get_date i) in
      let* x = post_with_token (string_of_admin_request request) "elections/%s" uuid in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [node i; node set_button]
  in
  let make of_string to_string what =
    let* x = get of_string "elections/%s/%s" uuid what in
    let@ voters = with_ok what x in
    let t = textarea () in
    t##.value := Js.string @@ to_string voters;
    Lwt.return [node t]
  in
  let* voters = make voter_list_of_string string_of_voter_list "voters" in
  let* records = make records_of_string string_of_records "records" in
  let* pds = make partial_decryptions_of_string string_of_partial_decryptions "partial-decryptions" in
  let* shuffles = make shuffles_of_string string_of_shuffles "shuffles" in
  let shuffle =
    let i = input [] in
    let make label request =
      let@ () = button label in
      let encoded = i##.value |> Js.encodeURIComponent |> Js.to_string in
      let* x = post_with_token (string_of_shuffler_request request) "elections/%s/shuffles/%s" uuid encoded in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [node @@ i; node @@ make "Skip" `Skip; node @@ make "Select" `Select]
  in
  Lwt.return [
      node @@ div [node @@ a ~href:"#" "Home"];
      node @@ h1 [txt "Raw election"];
      node @@ div [txt raw_election];
      node @@ h1 [txt "Status"];
      node @@ div [txt @@ string_of_election_status status];
      node @@ h1 [txt "Actions"];
      node @@ div buttons;
      node @@ div auto_dates;
      node @@ div regenpwd;
      node @@ div postpone;
      node @@ h1 [txt "Voters"];
      node @@ div voters;
      node @@ h1 [txt "Records"];
      node @@ div records;
      node @@ h1 [txt "Shuffles"];
      node @@ div shuffles;
      node @@ div shuffle;
      node @@ h1 [txt "Partial decryptions"];
      node @@ div pds;
    ]
