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
open Belenios_tool_js_common.Tool_js_html
open Common

let rec show main uuid =
  let@ () = show_in main in
  let* x = get (fun x -> x) "elections/%s/election" uuid in
  let@ raw_election = with_ok "election" x in
  let* x = get election_status_of_string "elections/%s" uuid in
  let@ status = with_ok "status" x in
  let button_switch =
    match status.status_state with
    | (`Open | `Closed) as x ->
       let label, request =
         match x with
         | `Open -> "Close election", `Close
         | `Closed -> "Open election", `Open
       in
       let b =
         let@ () = button label in
         let* x = post_with_token (string_of_admin_request request) "elections/%s" uuid in
         let@ () = show_in main in
         generic_proceed x (fun () -> show main uuid)
       in
       node @@ b
    | _ -> txt ""
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
      let get x =
        let y = Js.to_string x##.value in
        if y = "" then None else Some (Js.date##parse x##.value /. 1000.)
      in
      let dates =
        {
          auto_date_open = get auto_open;
          auto_date_close = get auto_close;
        }
      in
      let request = `SetAutomaticDates dates in
      let* x = post_with_token (string_of_admin_request request) "elections/%s" uuid in
      let@ () = show_in main in
      generic_proceed x (fun () -> show main uuid)
    in
    [node auto_open; node auto_close; node set_button]
  in
  Lwt.return [
      node @@ div [txt "Raw election: "; txt raw_election];
      node @@ div [txt "Status: "; txt @@ string_of_election_status status];
      node @@ div [button_switch];
      node @@ div auto_dates;
    ]
