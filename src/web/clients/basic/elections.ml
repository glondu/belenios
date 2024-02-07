(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session
open Common

let rec show main uuid =
  let@ () = show_in main in
  let* x = get (fun x -> x) "elections/%s/election" uuid in
  let@ raw_election, _ = with_ok "election" x in
  let* x = get election_status_of_string "elections/%s" uuid in
  let ifmatch = get_ifmatch x in
  let@ status, _ = with_ok "status" x in
  let make label request =
    let@ () = button label in
    let* x =
      post_with_token ?ifmatch
        (string_of_admin_request request)
        "elections/%s" uuid
    in
    let@ () = show_in main in
    generic_proceed x (fun () -> show main uuid)
  in
  let button_delete =
    let@ () = button "Delete election" in
    if confirm "Are you sure?" then (
      let* x = delete_with_token ?ifmatch "elections/%s" uuid in
      let@ () = show_in main in
      let@ () = generic_proceed x in
      Dom_html.window##.location##.hash := Js.string "";
      Lwt.return_unit)
    else Lwt.return_unit
  in
  let buttons =
    [
      make "Open election" `Open;
      make "Close election" `Close;
      make "Compute encrypted tally" `ComputeEncryptedTally;
      make "Finish shuffling" `FinishShuffling;
      make "Release tally" `ReleaseTally;
      make "Archive election" `Archive;
      button_delete;
    ]
  in
  let get_date (_, get) =
    let y = get () in
    if y = "" then None else Some (Js.date##parse (Js.string y) /. 1000.)
  in
  let* auto_dates =
    let* x =
      get election_auto_dates_of_string "elections/%s/automatic-dates" uuid
    in
    let@ dates, ifmatch = with_ok "automatic-dates" x in
    let make_input d =
      let value =
        match d with
        | None -> ""
        | Some x ->
            let x = new%js Js.date_fromTimeValue (x *. 1000.) in
            Js.to_string x##toISOString
      in
      input value
    in
    let auto_open = make_input dates.auto_date_open in
    let auto_close = make_input dates.auto_date_close in
    let set_button =
      let@ () = button "Set automatic dates" in
      let dates =
        {
          auto_date_open = get_date auto_open;
          auto_date_close = get_date auto_close;
        }
      in
      let* x =
        put_with_token ~ifmatch
          (string_of_election_auto_dates dates)
          "elections/%s/automatic-dates" uuid
      in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    Lwt.return [ fst auto_open; fst auto_close; set_button ]
  in
  let regenpwd =
    let i, iget = input "" in
    let set_button =
      let@ () = button "Regenerate a password" in
      let request = `RegeneratePassword (iget ()) in
      let* x =
        post_with_token ?ifmatch
          (string_of_admin_request request)
          "elections/%s" uuid
      in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [ i; set_button ]
  in
  let postpone =
    let i = input "" in
    let set_button =
      let@ () = button "Set postpone date" in
      let request = `SetPostponeDate (get_date i) in
      let* x =
        post_with_token ?ifmatch
          (string_of_admin_request request)
          "elections/%s" uuid
      in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [ fst i; set_button ]
  in
  let make of_string to_string what =
    let* x = get of_string "elections/%s/%s" uuid what in
    let@ voters, _ = with_ok what x in
    let t, _ = textarea (to_string voters) in
    Lwt.return [ t ]
  in
  let* voters = make voter_list_of_string string_of_voter_list "voters" in
  let* records = make records_of_string string_of_records "records" in
  let* pds =
    make partial_decryptions_of_string string_of_partial_decryptions
      "partial-decryptions"
  in
  let* shuffles = make shuffles_of_string string_of_shuffles "shuffles" in
  let shuffle =
    let i, iget = input "" in
    let make label request =
      let@ () = button label in
      let encoded =
        iget () |> Js.string |> Js.encodeURIComponent |> Js.to_string
      in
      let* x =
        post_with_token
          (string_of_shuffler_request request)
          "elections/%s/shuffles/%s" uuid encoded
      in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [ i; make "Skip" `Skip; make "Select" `Select ]
  in
  Lwt.return
    [
      div [ a ~href:"#" "Home" ];
      h1 [ txt "Raw election" ];
      div [ txt raw_election ];
      h1 [ txt "Status" ];
      div [ txt @@ string_of_election_status status ];
      h1 [ txt "Actions" ];
      div buttons;
      div auto_dates;
      div regenpwd;
      div postpone;
      h1 [ txt "Voters" ];
      div voters;
      h1 [ txt "Records" ];
      div records;
      h1 [ txt "Shuffles" ];
      div shuffles;
      div shuffle;
      h1 [ txt "Partial decryptions" ];
      div pds;
    ]
