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
open Belenios_core.Serializable_builtin_t
open Belenios_api.Serializable_j
open Web_serializable_builtin_t
open Api_generic

let dispatch_election _token endpoint method_ _body _uuid raw =
  match endpoint with
  | [] ->
     begin
       match method_ with
       | `GET -> Lwt.return (200, raw)
       | _ -> method_not_allowed
     end
  | _ -> not_found

let dispatch token endpoint method_ body =
  match endpoint with
  | [] ->
     begin
       let@ token = Option.unwrap unauthorized token in
       let@ account = Option.unwrap unauthorized (lookup_token token) in
       match method_ with
       | `GET ->
          let* elections = Web_persist.get_elections_by_owner account.account_id in
          let elections =
            List.fold_left
              (fun accu (kind, summary_uuid, date, summary_name) ->
                let summary_date = unixfloat_of_datetime date in
                match kind with
                | `Draft -> accu
                | (`Validated | `Tallied | `Archived) as x ->
                   let summary_kind = Some x in
                   {summary_uuid; summary_name; summary_date; summary_kind} :: accu
              ) [] elections
          in
          Lwt.return (200, string_of_summary_list elections)
       | _ -> method_not_allowed
     end
  | uuid :: endpoint ->
     let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
     let* raw = Web_persist.get_raw_election uuid in
     let@ raw = Option.unwrap not_found raw in
     dispatch_election token endpoint method_ body uuid raw
