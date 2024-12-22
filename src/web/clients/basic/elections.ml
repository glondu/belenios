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
open Belenios_api
open Tyxml_js.Html5
open Belenios_js.Common
open Belenios_js.Session
open Common

let rec show main uuid =
  let@ () = show_in main in
  let* x = Api.(get (election uuid) `Nobody) in
  let@ raw_election, _ = with_ok "election" x in
  let* x = Api.(get (election_status uuid) `Nobody) in
  let ifmatch = get_ifmatch x in
  let@ status, _ = with_ok "status" x in
  let make label request =
    let@ () = button label in
    let* x = Api.(post ?ifmatch (election_status uuid) !user request) in
    let@ () = show_in main in
    generic_proceed x (fun () -> show main uuid)
  in
  let button_delete =
    let@ () = button "Delete election" in
    if confirm "Are you sure?" then (
      let* x = Api.(delete (election uuid) !user) in
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
    let* x = Api.(get (election_auto_dates uuid) `Nobody) in
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
    let auto_publish = make_input dates.auto_date_publish in
    let set_button =
      let@ () = button "Set automatic dates" in
      let dates =
        {
          auto_date_open = get_date auto_open;
          auto_date_close = get_date auto_close;
          auto_date_publish = get_date auto_publish;
        }
      in
      let* x = Api.(put ~ifmatch (election_auto_dates uuid) !user dates) in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    Lwt.return [ fst auto_open; fst auto_close; fst auto_publish; set_button ]
  in
  let regenpwd =
    let i, iget = input "" in
    let set_button =
      let@ () = button "Regenerate a password" in
      let request = `RegeneratePassword (iget ()) in
      let* x = Api.(post ?ifmatch (election_status uuid) !user request) in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [ i; set_button ]
  in
  let make what e =
    let* x = Api.get e !user in
    let@ voters, _ = with_ok what x in
    let t, _ = textarea (e.to_string voters) in
    Lwt.return [ t ]
  in
  let* voters = make "voters" (Api.election_voters uuid) in
  let* records = make "records" (Api.election_records uuid) in
  let* pds =
    make "partial-decryptions" (Api.election_partial_decryptions uuid)
  in
  let* shuffles = make "shuffles" (Api.election_shuffles uuid) in
  let shuffle =
    let i, iget = input "" in
    let make label request =
      let@ () = button label in
      let* x = Api.(post (election_shuffle uuid (iget ())) !user request) in
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
