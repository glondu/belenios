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
open Belenios_core.Serializable_j
open Belenios_api.Serializable_j
open Web_serializable_builtin_t
open Web_serializable_t
open Web_common
open Api_generic

let with_administrator token metadata f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token, metadata.e_owner with
  | Some a, Some o when Accounts.check_account a o -> f a
  | _ -> unauthorized

let get_election_status uuid =
  let* s = Web_persist.get_election_state uuid in
  let* d = Web_persist.get_election_dates uuid in
  let status_state =
    match s with
    | `EncryptedTally _ -> `EncryptedTally
    | (`Open | `Closed | `Shuffling | `Tallied | `Archived) as x -> x
  in
  Lwt.return {
      status_state;
      status_auto_open_date = Option.map unixfloat_of_datetime d.e_auto_open;
      status_auto_close_date = Option.map unixfloat_of_datetime d.e_auto_close;
    }

let set_election_state uuid state =
  let* allowed =
    let* state = Web_persist.get_election_state uuid in
    match state with
    | `Open | `Closed -> Lwt.return_true
    | _ -> Lwt.return_false
  in
  if allowed then (
    let* () =
      Web_persist.set_election_state uuid
        (state : [`Open | `Closed] :> Web_serializable_t.election_state)
    in
    let* dates = Web_persist.get_election_dates uuid in
    let* () =
      Web_persist.set_election_dates uuid
        {dates with e_auto_open = None; e_auto_close = None}
    in
    Lwt.return_true
  ) else (
    Lwt.return_false
  )

let open_election uuid = set_election_state uuid `Open
let close_election uuid = set_election_state uuid `Closed

let set_election_auto_dates uuid d =
  let e_auto_open = Option.map datetime_of_unixfloat d.auto_date_open in
  let e_auto_close = Option.map datetime_of_unixfloat d.auto_date_close in
  let* dates = Web_persist.get_election_dates uuid in
  Web_persist.set_election_dates uuid {dates with e_auto_open; e_auto_close}

let perform_server_side_decryption election metadata tally =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let tally = encrypted_tally_of_string W.G.read tally in
  let decrypt i =
    let* x = Web_persist.get_private_key uuid in
    match x with
    | Some sk ->
       let* pd = W.E.compute_factor tally sk in
       let pd = string_of_partial_decryption W.G.write pd in
       Web_persist.set_partial_decryptions uuid [i, pd]
    | None ->
       Lwt.fail (Error "missing server private key")
  in
  Option.value metadata.e_trustees ~default:["server"]
  |> List.mapi (fun i t -> i, t)
  |> Lwt_list.iter_s
       (fun (i, t) ->
         if t = "server" then decrypt (i + 1) else Lwt.return_unit
       )

let transition_to_encrypted_tally election metadata tally =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* () = perform_server_side_decryption election metadata tally in
  Web_persist.set_election_state uuid @@ `EncryptedTally (0, 0, "")

let compute_encrypted_tally election metadata =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* state = Web_persist.get_election_state uuid in
  match state with
  | `Closed ->
     let* tally = Web_persist.compute_encrypted_tally election in
     if Belenios.Election.has_nh_questions W.election then (
       let* () = Web_persist.set_election_state uuid `Shuffling in
       Lwt.return_true
     ) else (
       let* () = transition_to_encrypted_tally election metadata tally in
       Lwt.return_true
     )
  | _ -> Lwt.return_false

let finish_shuffling election metadata =
  let module W = (val election : Site_common_sig.ELECTION_LWT) in
  let uuid = W.election.e_uuid in
  let* state = Web_persist.get_election_state uuid in
  match state with
  | `Shuffling ->
     begin
       let* x = Web_persist.compute_encrypted_tally_after_shuffling election in
       match x with
       | None -> Lwt.fail (Error "compute_encrypted_tally_after_shuffling")
       | Some tally ->
          let* () = transition_to_encrypted_tally election metadata tally in
          Lwt.return_true
     end
  | _ -> Lwt.return_false

let dispatch_election token endpoint method_ body uuid raw metadata =
  match endpoint with
  | [] ->
     begin
       match method_ with
       | `GET ->
          let* x = get_election_status uuid in
          Lwt.return (200, string_of_election_status x)
       | `POST ->
          begin
            let@ _ = with_administrator token metadata in
            let@ request = body.run admin_request_of_string in
            match request with
            | (`Open | `Close) as x ->
               let doit =
                 match x with
                 | `Open -> open_election
                 | `Close -> close_election
               in
               let* b = doit uuid in
               if b then ok else forbidden
            | `SetAutomaticDates d ->
               let* () = set_election_auto_dates uuid d in
               ok
            | (`ComputeEncryptedTally | `FinishShuffling) as x ->
               let doit =
                 match x with
                 | `ComputeEncryptedTally -> compute_encrypted_tally
                 | `FinishShuffling -> finish_shuffling
               in
               let module W = Belenios.Election.Make (struct let raw_election = raw end) (LwtRandom) () in
               let* b = doit (module W) metadata in
               if b then ok else forbidden
          end
       | _ -> method_not_allowed
     end
  | ["election"] ->
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
     let* metadata = Web_persist.get_election_metadata uuid in
     dispatch_election token endpoint method_ body uuid raw metadata
