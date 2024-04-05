(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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
open Belenios_server_core

module type INPUT = sig
  type session

  val get : session -> Storage.file -> string option Lwt.t
  val list_elections : session -> uuid list Lwt.t
  val with_transaction : (session -> 'a Lwt.t) -> 'a Lwt.t
end

module Make (I : INPUT) () = struct
  let elections_by_owner_cache = ref None
  let elections_by_owner_mutex = Lwt_mutex.create ()

  module Clear = struct
    let clear () = elections_by_owner_cache := None
  end

  let get_live_election_summary s uuid =
    let* state =
      let* x = I.get s (Election (uuid, State)) in
      match x with
      | None -> Lwt.return `Archived
      | Some x -> Lwt.return @@ election_state_of_string x
    in
    let get of_string file =
      let* x = I.get s (Election (uuid, file)) in
      match x with None -> Lwt.fail Exit | Some x -> Lwt.return @@ of_string x
    in
    let* metadata = get metadata_of_string Metadata in
    let* roots = get roots_of_string Roots in
    let* name =
      match roots.roots_setup_data with
      | None -> Lwt.fail Exit
      | Some setup_data ->
          let* setup_data = get setup_data_of_string (Data setup_data) in
          let* (Template (_, template)) =
            get Election.template_of_string (Data setup_data.setup_election)
          in
          Lwt.return template.t_name
    in
    let* date =
      let* dates = get election_dates_of_string Dates in
      match state with
      | `Open | `Closed | `Shuffling | `EncryptedTally ->
          Lwt.return
          @@ Option.value dates.e_finalization ~default:Defaults.validation_date
      | `Tallied ->
          Lwt.return @@ Option.value dates.e_tally ~default:Defaults.tally_date
      | `Archived ->
          Lwt.return
          @@ Option.value dates.e_archive ~default:Defaults.archive_date
    in
    let date = Datetime.to_unixfloat date in
    let state = (state :> Belenios_api.Serializable_t.state) in
    let item : Belenios_api.Serializable_t.summary =
      { uuid; state; date; name }
    in
    Lwt.return (metadata.e_owners, item)

  let get_draft_election_summary uuid se =
    let date =
      Option.value ~default:Defaults.creation_date se.se_creation_date
      |> Datetime.to_unixfloat
    in
    let name = se.se_questions.t_name in
    let item : Belenios_api.Serializable_t.summary =
      { uuid; date; name; state = `Draft }
    in
    (se.se_owners, item)

  let get_election_summary s uuid =
    let* draft = I.get s (Election (uuid, Draft)) in
    match draft with
    | None -> get_live_election_summary s uuid
    | Some x ->
        let (Draft (_, se)) = draft_election_of_string x in
        Lwt.return @@ get_draft_election_summary uuid se

  let umap_add user x map =
    let xs = match IMap.find_opt user map with None -> [] | Some xs -> xs in
    IMap.add user (x :: xs) map

  let build_elections_by_owner_cache () =
    let* elections =
      let@ s = I.with_transaction in
      I.list_elections s
    in
    Lwt_list.fold_left_s
      (fun accu uuid ->
        Lwt.catch
          (fun () ->
            let@ s = I.with_transaction in
            let* ids, item = get_election_summary s uuid in
            Lwt.return
            @@ List.fold_left (fun accu id -> umap_add id item accu) accu ids)
          (fun _ -> Lwt.return accu))
      IMap.empty elections

  let get_elections_by_owner user =
    let* cache =
      match !elections_by_owner_cache with
      | Some x -> Lwt.return x
      | None ->
          let@ () = Lwt_mutex.with_lock elections_by_owner_mutex in
          let* x = build_elections_by_owner_cache () in
          elections_by_owner_cache := Some x;
          Lwt.return x
    in
    match IMap.find_opt user cache with
    | None -> Lwt.return []
    | Some xs -> Lwt.return xs
end
