(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2026 Inria                                           *)
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
open Belenios
open Belenios_storage_api
open Belenios_server_core

let get_next_action s =
  let* draft = Storage.E.get s Draft in
  match Lopt.get_string draft with
  | Some _ ->
      let* dates = Storage.E.get s Dates in
      let&* dates = Lopt.get_value dates in
      let next_t = add_days dates.creation Defaults.days_to_delete in
      Lwt.return_some (`Destroy, next_t)
  | None -> (
      let* state = Storage.E.get s State in
      let* dates = Storage.E.get s Dates in
      let dates =
        Option.value (Lopt.get_value dates) ~default:default_election_dates
      in
      let since x = Option.value x ~default:dates.creation in
      match Option.map to_election_state (Lopt.get_value state) with
      | Some (`Open | `Closed | `Shuffling | `EncryptedTally) ->
          let next_t =
            add_days (since dates.finalization) Defaults.days_to_delete
          in
          Lwt.return_some (`Delete, next_t)
      | Some `Tallied ->
          let next_t = add_days (since dates.tally) Defaults.days_to_archive in
          Lwt.return_some (`Archive, next_t)
      | None ->
          let next_t = add_days (since dates.archive) Defaults.days_to_delete in
          Lwt.return_some (`Delete, next_t))

let process_election uuid =
  let* comment =
    let@ s = Storage.E.with_transaction uuid in
    let* action =
      Lwt.catch (fun () -> get_next_action s) (fun _ -> Lwt.return_none)
    in
    match action with
    | Some (action, next_t) when datetime_now () > next_t -> (
        match action with
        | `Destroy ->
            let* () = Storage.E.delete_election s in
            Lwt.return_some "destroyed"
        | `Delete ->
            let* () = Storage.E.delete_election s in
            Lwt.return_some "deleted"
        | `Archive ->
            let* () = Storage.E.archive_election s in
            Lwt.return_some "archived")
    | _ -> Lwt.return_none
  in
  match comment with
  | None -> Lwt.return_unit
  | Some comment ->
      Printf.ksprintf Ocsigen.Messages.warning
        "Election %s has been automatically %s" (Uuid.to_string uuid) comment;
      Lwt.return_unit

let rec loop () =
  let open Ocsigen.Messages in
  let* () =
    if Storage.readonly.get () then (
      accesslog "Data policy process skipped because of readonly mode";
      Lwt.return_unit)
    else
      let () = accesslog "Data policy process started" in
      let* elections = Storage.list_elections () in
      let* () = Lwt_list.iter_s process_election elections in
      let () = accesslog "Data policy process completed" in
      Lwt.return_unit
  in
  let* () = Lwt_unix.sleep 3600. in
  loop ()
