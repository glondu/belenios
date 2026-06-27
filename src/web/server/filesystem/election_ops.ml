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
open Serializable

type 'a file =
  | Election of uuid * 'a election_file
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
  val new_election : unit -> uuid option Lwt.t
  val delete_sensitive_data : uuid -> unit Lwt.t
  val delete_live_data : uuid -> unit Lwt.t
  val write_deleted_file : uuid -> deleted_election -> unit Lwt.t
  val delete_draft_election : uuid -> unit Lwt.t

  val init_credential_mapping :
    uuid -> ('a, 'b) group_witness -> 'a public_credentials Lwt.t
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
  let module CredSet = Set.Make (W.G) in
  let&? metadata = Metadata in
  let&? dates = Dates in
  let* () = S.delete_sensitive_data uuid in
  let de_template =
    {
      description = "";
      name = W.template.name;
      questions = Array.map W.erase_question W.template.questions;
      administrator = None;
      credential_authority = None;
      language = None;
    }
    |> !+(yojson_of_template W.yojson_of_question)
    |> !*(template_of_yojson Fun.id)
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
  let de_credential_method =
    match metadata.cred_authority with
    | Some "server" -> `Automatic
    | _ -> `Manual
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
                | Some b -> cont (!*W.ballot_of_yojson b)
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
      uuid;
      template = de_template;
      owners = de_owners;
      nb_voters = de_nb_voters;
      nb_ballots = de_nb_ballots;
      date = Datetime.from_unixfloat de_date;
      tallied = roots.result <> None;
      authentication_method = de_authentication_method;
      credential_method = de_credential_method;
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
  set Value { dates with archive = Some (Unix.gettimeofday ()) }

exception Validation_error of Belenios_web_api.validation_error

let validate_election_exn s uuid =
  let module S = (val s : BACKEND) in
  let@ draft cont =
    let* x = S.get (Election (uuid, Draft)) in
    match Lopt.get_value x with None -> raise Not_found | Some x -> cont x
  in
  let (W (w, Draft (v, se))) = draft in
  let version = se.version in
  let questions =
    let x = se.questions in
    let administrator =
      match x.administrator with None -> se.administrator | x -> x
    in
    let credential_authority =
      match x.credential_authority with
      | None -> se.metadata.cred_authority
      | x -> x
    in
    { x with administrator; credential_authority }
  in
  (* trustees *)
  let group = Group.of_string ~version se.group in
  let module G = (val group : GROUP) in
  let Equal = Group_witness.provably_equal __FUNCTION__ w G.witness in
  let trustees =
    let open Belenios_storage_api in
    se.trustees
  in
  let module Trustees = (val Trustees.get_by_version version) in
  let module K = Trustees.MakeCombinator (G) in
  let module KG = Trustees.MakeSimple (G) in
  let* trustee_names, trustees, private_keys =
    match trustees with
    | `Basic x -> (
        let ts = x.trustees in
        match ts with
        | [] ->
            let private_key = KG.generate () in
            let public_key = KG.prove private_key in
            Lwt.return ([ None ], [ `Single public_key ], `KEY private_key)
        | _ :: _ ->
            let private_key =
              List.fold_left
                (fun accu t ->
                  match t.kind with
                  | External _ -> accu
                  | Server u -> u.private_key :: accu)
                [] ts
            in
            let private_key =
              match private_key with
              | [ x ] -> `KEY x
              | _ -> raise @@ Validation_error `NotSinglePrivateKey
            in
            Lwt.return
              ( List.map
                  (fun (x : _ draft_trustee) ->
                    match x.kind with
                    | Server _ -> None
                    | External u -> Some u.id)
                  ts,
                List.map
                  (fun { public_key; _ } ->
                    match public_key with
                    | None ->
                        raise @@ Validation_error `KeyEstablishmentNotFinished
                    | Some x -> `Single x)
                  ts,
                private_key ))
    | `Threshold x -> (
        let ts = x.trustees in
        match x.parameters with
        | None -> raise @@ Validation_error `KeyEstablishmentNotFinished
        | Some tp ->
            let trustee_names =
              List.map
                (fun ({ id; _ } : _ draft_threshold_trustee) -> Some id)
                ts
            in
            let private_keys =
              List.map
                (fun { voutput; _ } ->
                  match voutput with
                  | Some v -> v.private_key
                  | None -> failwith "inconsistent state")
                ts
            in
            let server_private_key = KG.generate () in
            let server_public_key = KG.prove server_private_key in
            Lwt.return
              ( None :: trustee_names,
                [ `Single server_public_key; `Pedersen tp ],
                `KEYS (server_private_key, private_keys) ))
  in
  let y = K.combine_keys trustees in
  (* election parameters *)
  let metadata =
    { se.metadata with trustees = Some trustee_names; owners = se.owners }
  in
  let template = Belenios.Election.Template (v, questions) in
  let raw_election =
    let public_key = G.to_string y in
    Election.make_raw_election ~version:se.version template ~uuid
      ~group:se.group ~public_key
    |> Json.to_string
  in
  (* write election files to disk *)
  let voters = se.voters |> List.map (fun (x : draft_voter) -> x.id) in
  let* () = voters |> S.set (Election (uuid, Voters)) Value in
  let* () = metadata |> S.set (Election (uuid, Metadata)) Value in
  (* initialize credentials *)
  let* public_creds = S.init_credential_mapping uuid G.witness in
  (* initialize events *)
  let* () =
    let raw_trustees =
      !+(yojson_of_trustees !&G.to_string !&G.Zq.to_string) trustees
    in
    let raw_public_creds =
      !+(yojson_of_public_credentials !&G.to_string) public_creds
    in
    let setup_election = Hash.hash_string raw_election in
    let setup_trustees = Hash.hash_string raw_trustees in
    let setup_credentials = Hash.hash_string raw_public_creds in
    let raw_certificate, setup_credentials_certificate =
      match se.public_creds_certificate with
      | None -> ([], None)
      | Some c ->
          let raw =
            c
            |> !+(yojson_of_credentials_certificate !&G.to_string
                    !&G.Zq.to_string)
          in
          ([ Data raw ], Some (Hash.hash_string raw))
    in
    let setup_data =
      {
        election = setup_election;
        trustees = setup_trustees;
        credentials = setup_credentials;
        credentials_certificate = setup_credentials_certificate;
      }
    in
    let setup_data_s = !+yojson_of_setup_data setup_data in
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
    | `KEY x -> x |> S.set (Election (uuid, Private_key G.witness)) Value
    | `KEYS (x, y) ->
        let* () = x |> S.set (Election (uuid, Private_key G.witness)) Value in
        y |> S.set (Election (uuid, Private_keys G.witness)) Value
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
  (* finish *)
  let* () = S.set (Election (uuid, State)) Value `Closed in
  set_dates Value { dates with finalization = Some (Unix.gettimeofday ()) }

let validate_election s uuid =
  Lwt.try_bind
    (fun () -> validate_election_exn s uuid)
    Lwt_result.return
    (function Validation_error e -> Lwt_result.fail e | e -> Lwt.reraise e)
