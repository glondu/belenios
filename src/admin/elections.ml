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
open Belenios_core.Common
open Belenios_api.Serializable_j
open Belenios_tool_js_common.Tool_js_html
open Common

let rec show main uuid =
  let@ () = show_in main in
  let* x = get (fun x -> x) "elections/%s" uuid in
  let@ raw_election = with_ok "election" x in
  let* x = get election_status_of_string "elections/%s/status" uuid in
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
         let* x = post_with_token (string_of_admin_request request) "elections/%s/admin" uuid in
         let@ () = show_in main in
         generic_proceed x (fun () -> show main uuid)
       in
       node @@ b
    | _ -> txt ""
  in
  Lwt.return [
      node @@ div [txt "Raw election: "; txt raw_election];
      node @@ div [txt "Status: "; txt @@ string_of_election_status status];
      node @@ div [button_switch];
    ]
