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
open Belenios_server_core
open Serializable_j

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
  val new_election : unit -> uuid option Lwt.t
  val delete_sensitive_data : uuid -> unit Lwt.t
  val delete_live_data : uuid -> unit Lwt.t
  val write_deleted_file : uuid -> deleted_election -> unit Lwt.t
  val delete_draft_election : uuid -> unit Lwt.t
  val init_credential_mapping : uuid -> public_credentials Lwt.t
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
    let&! x = roots.roots_setup_data in
    let&? x = Data x in
    cont (setup_data_of_string x)
  in
  let@ election cont =
    let&? x = Data setup_data.setup_election in
    cont (Election.of_string (module Random) x)
  in
  let@ trustees cont =
    let&? x = Data setup_data.setup_trustees in
    cont (trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json x)
  in
  let module W = (val election) in
  let module CredSet = Set.Make (W.G) in
  let&? metadata = Metadata in
  let&? dates = Dates in
  let* () = S.delete_sensitive_data uuid in
  let de_template =
    {
      t_description = "";
      t_name = W.template.t_name;
      t_questions = Array.map W.erase_question W.template.t_questions;
      t_administrator = None;
      t_credential_authority = None;
    }
    |> string_of_template W.write_question
    |> template_of_string Yojson.Safe.read_json
  in
  let de_owners = metadata.e_owners in
  let de_date =
    match dates.e_date_tally with
    | Some x -> x
    | None -> (
        match dates.e_date_finalization with
        | Some x -> x
        | None -> dates.e_date_creation)
  in
  let de_authentication_method =
    match metadata.e_auth_config with
    | Some [ { auth_system = "cas"; auth_config; _ } ] ->
        let server = List.assoc "server" auth_config in
        `CAS server
    | Some [ { auth_system = "password"; _ } ] -> `Password
    | _ -> `Unknown
  in
  let de_credential_method =
    match metadata.e_cred_authority with
    | Some "server" -> `Automatic
    | _ -> `Manual
  in
  let* de_trustees =
    trustees
    |> List.map (function
      | `Single _ -> `Single
      | `Pedersen t ->
          `Pedersen (t.t_threshold, Array.length t.t_verification_keys))
    |> Lwt.return
  in
  let* de_nb_ballots =
    match roots.roots_last_ballot_event with
    | None -> Lwt.return 0
    | Some e ->
        let rec loop seen accu e =
          let@ event cont =
            let* x = S.get (Election (uuid, Data e)) in
            match Lopt.get_value x with
            | None -> Lwt.return accu
            | Some x -> cont (event_of_string x)
          in
          match (event.event_typ, event.event_payload, event.event_parent) with
          | `Ballot, Some b, Some p ->
              let@ ballot cont =
                let* x = S.get (Election (uuid, Data b)) in
                match Lopt.get_value x with
                | None -> Lwt.return accu
                | Some b -> cont (W.read_ballot ++ b)
              in
              let@ credential cont =
                match W.get_credential ballot with
                | None -> loop seen (accu + 1) p
                | Some c -> cont c
              in
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
      de_uuid = uuid;
      de_template;
      de_owners;
      de_nb_voters;
      de_nb_ballots;
      de_date = Datetime.from_unixfloat de_date;
      de_tallied = roots.roots_result <> None;
      de_authentication_method;
      de_credential_method;
      de_trustees;
      de_has_weights;
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
  set Value { dates with e_date_archive = Some (Unix.gettimeofday ()) }

exception Validation_error of Belenios_web_api.validation_error

let validate_election_exn s uuid =
  let module S = (val s : BACKEND) in
  let@ draft cont =
    let* x = S.get (Election (uuid, Draft)) in
    match Lopt.get_value x with None -> raise Not_found | Some x -> cont x
  in
  let (Draft (v, se)) = draft in
  let version = se.se_version in
  let questions =
    let x = se.se_questions in
    let t_administrator =
      match x.t_administrator with None -> se.se_administrator | x -> x
    in
    let t_credential_authority =
      match x.t_credential_authority with
      | None -> se.se_metadata.e_cred_authority
      | x -> x
    in
    { x with t_administrator; t_credential_authority }
  in
  (* trustees *)
  let group = Group.of_string ~version se.se_group in
  let module G = (val group : GROUP) in
  let trustees =
    let open Belenios_storage_api in
    se.se_trustees
    |> string_of_draft_trustees Yojson.Safe.write_json
    |> draft_trustees_of_string (sread G.Zq.of_string)
  in
  let module Trustees = (val Trustees.get_by_version version) in
  let module K = Trustees.MakeCombinator (G) in
  let module KG = Trustees.MakeSimple (G) (Random) in
  let* trustee_names, trustees, private_keys =
    match trustees with
    | `Basic x -> (
        let ts = x.dbp_trustees in
        match ts with
        | [] ->
            let private_key = KG.generate () in
            let public_key = KG.prove private_key in
            let public_key = { public_key with trustee_name = Some "server" } in
            Lwt.return ([ "server" ], [ `Single public_key ], `KEY private_key)
        | _ :: _ ->
            let private_key =
              List.fold_left
                (fun accu { st_private_key; _ } ->
                  match st_private_key with Some x -> x :: accu | None -> accu)
                [] ts
            in
            let private_key =
              match private_key with
              | [ x ] -> `KEY x
              | _ -> raise @@ Validation_error `NotSinglePrivateKey
            in
            Lwt.return
              ( List.map (fun { st_id; _ } -> st_id) ts,
                List.map
                  (fun { st_public_key; st_name; _ } ->
                    let pk =
                      trustee_public_key_of_string (sread G.of_string)
                        (sread G.Zq.of_string) st_public_key
                    in
                    let pk = { pk with trustee_name = st_name } in
                    `Single pk)
                  ts,
                private_key ))
    | `Threshold x -> (
        let ts = x.dtp_trustees in
        match x.dtp_parameters with
        | None -> raise @@ Validation_error `KeyEstablishmentNotFinished
        | Some tp ->
            let tp =
              threshold_parameters_of_string (sread G.of_string)
                (sread G.Zq.of_string) tp
            in
            let named =
              List.combine (Array.to_list tp.t_verification_keys) ts
              |> List.map (fun ((k : _ trustee_public_key), t) ->
                  { k with trustee_name = t.stt_name })
              |> Array.of_list
            in
            let tp = { tp with t_verification_keys = named } in
            let trustee_names = List.map (fun { stt_id; _ } -> stt_id) ts in
            let private_keys =
              List.map
                (fun { stt_voutput; _ } ->
                  match stt_voutput with
                  | Some v ->
                      let voutput =
                        voutput_of_string (sread G.of_string)
                          (sread G.Zq.of_string) v
                      in
                      voutput.vo_private_key
                  | None -> failwith "inconsistent state")
                ts
            in
            let server_private_key = KG.generate () in
            let server_public_key = KG.prove server_private_key in
            let server_public_key =
              { server_public_key with trustee_name = Some "server" }
            in
            Lwt.return
              ( "server" :: trustee_names,
                [ `Single server_public_key; `Pedersen tp ],
                `KEYS (server_private_key, private_keys) ))
  in
  let y = K.combine_keys trustees in
  (* election parameters *)
  let metadata =
    {
      se.se_metadata with
      e_trustees = Some trustee_names;
      e_owners = se.se_owners;
    }
  in
  let template = Belenios.Election.Template (v, questions) in
  let raw_election =
    let public_key = G.to_string y in
    Election.make_raw_election ~version:se.se_version template ~uuid
      ~group:se.se_group ~public_key
  in
  (* write election files to disk *)
  let voters = se.se_voters |> List.map (fun x -> x.sv_id) in
  let* () = voters |> S.set (Election (uuid, Voters)) Value in
  let* () = metadata |> S.set (Election (uuid, Metadata)) Value in
  (* initialize credentials *)
  let* public_creds = S.init_credential_mapping uuid in
  (* initialize events *)
  let* () =
    let raw_trustees =
      string_of_trustees (swrite G.to_string) (swrite G.Zq.to_string) trustees
    in
    let raw_public_creds = string_of_public_credentials public_creds in
    let setup_election = Hash.hash_string raw_election in
    let setup_trustees = Hash.hash_string raw_trustees in
    let setup_credentials = Hash.hash_string raw_public_creds in
    let raw_certificate, setup_credentials_certificate =
      match se.se_public_creds_certificate with
      | None -> ([], None)
      | Some c ->
          let raw =
            c
            |> string_of_credentials_certificate Yojson.Safe.write_json
                 Yojson.Safe.write_json
          in
          ([ Data raw ], Some (Hash.hash_string raw))
    in
    let setup_data =
      {
        setup_election;
        setup_trustees;
        setup_credentials;
        setup_credentials_certificate;
      }
    in
    let setup_data_s = string_of_setup_data setup_data in
    let* x =
      [
        [ Data raw_election; Data raw_trustees; Data raw_public_creds ];
        raw_certificate;
        [
          Data setup_data_s; Event (`Setup, Some (Hash.hash_string setup_data_s));
        ];
      ]
      |> List.flatten |> S.append uuid
    in
    match x with
    | true -> Lwt.return_unit
    | false -> failwith "race condition in validate_election"
  in
  (* create file with private keys, if any *)
  let* () =
    match private_keys with
    | `KEY x ->
        swrite G.Zq.to_string -- x
        |> S.set (Election (uuid, Private_key)) String
    | `KEYS (x, y) ->
        let* () =
          swrite G.Zq.to_string -- x
          |> S.set (Election (uuid, Private_key)) String
        in
        y |> S.set (Election (uuid, Private_keys)) Value
  in
  (* clean up draft *)
  let@ dates, set_dates =
   fun cont ->
    let@ dates, set = S.update (Election (uuid, Dates)) in
    match Lopt.get_value dates with
    | None -> assert false
    | Some dates -> cont (dates, set)
  in
  let* () = S.del (Election (uuid, Draft)) in
  (* clean up private credentials, if any *)
  let* () = S.del (Election (uuid, Private_creds)) in
  (* write passwords *)
  let* () =
    match metadata.e_auth_config with
    | Some [ { auth_system = "password"; _ } ] ->
        let db =
          List.filter_map
            (fun v ->
              let login = Voter.get v.sv_id in
              let& salt, hashed = v.sv_password in
              Some [ login; salt; hashed ])
            se.se_voters
        in
        if db <> [] then S.set (Election (uuid, Passwords)) Value db
        else Lwt.return_unit
    | _ -> Lwt.return_unit
  in
  (* finish *)
  let* () = S.set (Election (uuid, State)) Value `Closed in
  set_dates Value
    { dates with e_date_finalization = Some (Unix.gettimeofday ()) }

let validate_election s uuid =
  Lwt.try_bind
    (fun () -> validate_election_exn s uuid)
    Lwt_result.return
    (function Validation_error e -> Lwt_result.fail e | e -> Lwt.reraise e)
