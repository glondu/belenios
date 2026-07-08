(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2025 Inria                                           *)
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
open Types

type 'a file =
  | Election of uuid * 'a election_file
  | Trustees of uuid * 'a trustees_file
  | Credentials of uuid * 'a credentials_file
  | Account of 'a account_file

module type BACKEND = sig
  val get_unixfilename : 'a file -> string Lwt.t
  val get : 'a file -> 'a lopt Lwt.t
  val set : 'a file -> ('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t
  val del : 'a file -> unit Lwt.t

  val update :
    'a file ->
    ('a lopt * (('a, 'b) string_or_value_spec -> 'b -> unit Lwt.t) -> 'r Lwt.t) ->
    'r Lwt.t

  val append : uuid -> ?last:last_event -> append_operation list -> bool Lwt.t
  val append_sealing : uuid -> sealing_event -> bool Lwt.t
  val new_election : unit -> uuid Lwt.t
  val delete_sensitive_data : uuid -> unit Lwt.t
  val delete_live_data : uuid -> unit Lwt.t
  val write_deleted_file : uuid -> deleted_election -> unit Lwt.t
  val delete_draft_election : uuid -> unit Lwt.t
end

let ( let&! ) x f = match x with None -> Lwt.return_unit | Some x -> f x

let delete_live_election s uuid roots =
  let module S = (val s : BACKEND) in
  let ( let&? ) x f =
    let* x = S.get (Election (uuid, x)) in
    let&! x = Lopt.get_value x in
    f x
  in
  let@ setup_data cont =
    let&! x = roots.setup_data in
    let&? x = Data x in
    cont (!*setup_data_of_yojson x)
  in
  let@ election cont =
    let&? x = Data setup_data.election in
    cont (!*Election.t_of_yojson x)
  in
  let@ trustees cont =
    let&? x = Data setup_data.trustees in
    cont (!*(trustees_of_yojson Fun.id Fun.id) x)
  in
  let module W = (val election) in
  let module G = W.G in
  let module CredSet = Set.Make (W.G) in
  let&? metadata = Metadata in
  let&? dates = Dates in
  let* () = S.delete_sensitive_data uuid in
  let credential_authority =
    match W.template.credential_authority with
    | `Server -> `Server
    | `External _ -> `External ""
  in
  let de_template =
    {
      description = "";
      name = W.template.name;
      questions = Array.map Question.erase_question W.template.questions;
      administrator = "";
      credential_authority;
      language = None;
    }
  in
  let de_owners = metadata.owners in
  let de_date =
    match dates.tally with
    | Some x -> x
    | None -> (
        match dates.finalization with Some x -> x | None -> dates.creation)
  in
  let de_authentication_method =
    match metadata.auth_config with
    | Some [ { auth_system = "cas"; auth_config; _ } ] ->
        let server = List.assoc "server" auth_config in
        `CAS server
    | Some [ { auth_system = "email"; _ } ] -> `Email
    | _ -> `Unknown
  in
  let* de_trustees =
    trustees
    |> List.map (function
      | `Single _ -> `Single
      | `Pedersen (t : _ threshold_parameters) ->
          `Pedersen (t.context.threshold, Array.length t.verification_keys))
    |> Lwt.return
  in
  let* de_nb_ballots =
    match roots.last_ballot_event with
    | None -> Lwt.return 0
    | Some e ->
        let rec loop seen accu e =
          let@ event cont =
            let* x = S.get (Election (uuid, Data e)) in
            match Lopt.get_value x with
            | None -> Lwt.return accu
            | Some x -> cont (!*event_of_yojson x)
          in
          match (event.typ, event.payload, event.parent) with
          | `Ballot, Some b, Some p ->
              let@ ballot cont =
                let* x = S.get (Election (uuid, Data b)) in
                match Lopt.get_value x with
                | None -> Lwt.return accu
                | Some b -> cont (!*[%group_of_yojson: _ ballot] b)
              in
              let credential = ballot.message.credential in
              if CredSet.mem credential seen then loop seen accu p
              else loop (CredSet.add credential seen) (accu + 1) p
          | _ -> Lwt.return accu
        in
        loop CredSet.empty 0 e
  in
  let* de_nb_voters, de_has_weights =
    let* x = S.get (Election (uuid, Voters_config)) in
    match Lopt.get_value x with
    | None -> Lwt.return (0, false)
    | Some { has_explicit_weights; nb_voters; _ } ->
        Lwt.return (nb_voters, has_explicit_weights)
  in
  let de =
    {
      uuid;
      template = de_template;
      owners = de_owners;
      nb_voters = de_nb_voters;
      nb_ballots = de_nb_ballots;
      date = de_date;
      tallied = roots.result <> None;
      authentication_method = de_authentication_method;
      trustees = de_trustees;
      has_weights = de_has_weights;
    }
  in
  let* () = S.write_deleted_file uuid de in
  S.delete_live_data uuid

let delete_election s uuid =
  let module S = (val s : BACKEND) in
  let* x = S.get (Election (uuid, Roots)) in
  match Lopt.get_value x with
  | None -> S.delete_draft_election uuid
  | Some roots -> delete_live_election s uuid roots

let archive_election s uuid =
  let module S = (val s : BACKEND) in
  let* () = S.delete_sensitive_data uuid in
  let@ dates, set = S.update (Election (uuid, Dates)) in
  let&! dates = Lopt.get_value dates in
  set Value { dates with archive = Some (datetime_now ()) }
