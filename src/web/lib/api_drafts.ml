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
open Belenios_platform.Platform
open Belenios_core.Common
open Belenios_core.Serializable_builtin_t
open Belenios_core.Serializable_t
open Belenios_core.Signatures
open Belenios_api.Serializable_t
open Web_serializable_builtin_t
open Web_serializable_t
open Web_common

exception Error of string

let ( / ) = Filename.concat

let get_authentication se =
  match se.se_metadata.e_auth_config with
  | Some [{auth_system = "password"; _}] -> `Password
  | Some [{auth_system = "cas"; auth_config; _}] -> `CAS (List.assoc "server" auth_config)
  | Some [{auth_system = "import"; auth_instance; _}] -> `Configured auth_instance
  | _ -> raise (Error "invalid authentication")

let auth_config_of_authentication = function
  | `Password -> {auth_system = "password"; auth_instance = "password"; auth_config = []}
  | `CAS server -> {auth_system = "cas"; auth_instance = "cas"; auth_config = ["server", server]}
  | `Configured auth_instance -> {auth_system = "import"; auth_instance; auth_config = []}

let api_of_draft se =
  let draft_questions =
    {
      se.se_questions with
      t_credential_authority = Some (Option.get se.se_metadata.e_cred_authority "");
      t_administrator = Some (Option.get se.se_administrator "");
    }
  in
  {
    draft_version = Option.get se.se_version 0;
    draft_questions;
    draft_languages = Option.get se.se_metadata.e_languages [];
    draft_contact = se.se_metadata.e_contact;
    draft_booth = Option.get se.se_metadata.e_booth_version 1;
    draft_authentication = get_authentication se;
    draft_group = se.se_group;
  }

let assert_ msg b f =
  if b then f () else raise (Error msg)

let draft_of_api se d =
  let version = Option.get se.se_version 0 in
  let () =
    if d.draft_version <> version then
      raise (Error "cannot change version")
  in
  let@ () = assert_ "invalid booth version" (1 <= d.draft_booth && d.draft_booth <= 2) in
  let@ () = assert_ "there must be at least one language" (List.length d.draft_languages >= 1) in
  let e_cred_authority = d.draft_questions.t_credential_authority in
  let () =
    let old = se.se_metadata.e_cred_authority in
    if e_cred_authority <> old then
      if se.se_public_creds_received && (old = Some "server" || e_cred_authority = Some "server") then
        raise (Error "credential authority change not allowed")
  in
  let se_group = d.draft_group in
  let () =
    let old = se.se_group in
    if se_group <> old then (
      if se.se_public_creds_received then
        raise (Error "the group cannot be changed")
      else (
        try
          let module G = (val Belenios.Group.of_string ~version se_group) in
          ()
        with _ ->
          raise (Error "invalid group")
      )
    )
  in
  let () =
    let has_nh_questions =
      Array.exists
        (function Belenios_core.Question.NonHomomorphic _ -> true | _ -> false)
        d.draft_questions.t_questions
    in
    if has_nh_questions then (
      match se_group with
      | "RFC-3526-2048" -> ()
      | _ -> raise (Error "a NH group is required for these questions")
    )
  in
  let se_metadata =
    {
      se.se_metadata with
      e_contact = d.draft_contact;
      e_languages = Some d.draft_languages;
      e_booth_version = Some d.draft_booth;
      e_cred_authority;
      e_auth_config = Some [auth_config_of_authentication d.draft_authentication];
    }
  in
  {
    se with
    se_metadata;
    se_questions = d.draft_questions;
    se_administrator = d.draft_questions.t_administrator;
    se_group;
  }

let delete_draft uuid =
  let* () = rmdir (!Web_config.spool_dir / raw_string_of_uuid uuid) in
  Web_persist.clear_elections_by_owner_cache ()

let generate_uuid () =
  let length = !Web_config.uuid_length in
  let* token = generate_token ?length () in
  Lwt.return (uuid_of_raw_string token)

let post_drafts account draft =
  let owner = `Id account.account_id in
  let* uuid = generate_uuid () in
  let* token = generate_token () in
  let se_metadata =
    {
      e_owner = Some owner;
      e_auth_config = None;
      e_cred_authority = None;
      e_trustees = None;
      e_languages = None;
      e_contact = None;
      e_server_is_trustee = None;
      e_booth_version = None;
    }
  in
  let se_questions =
    {
      t_description = "";
      t_name = "";
      t_questions = [||];
      t_administrator = None;
      t_credential_authority = None;
    }
  in
  let se =
    {
      se_version = Some default_version;
      se_owner = owner;
      se_group = !Web_config.default_group;
      se_voters = [];
      se_questions;
      se_public_keys = [];
      se_metadata;
      se_public_creds = token;
      se_public_creds_received = false;
      se_threshold = None;
      se_threshold_trustees = None;
      se_threshold_parameters = None;
      se_threshold_error = None;
      se_creation_date = Some (now ());
      se_administrator = None;
    }
  in
  let se = draft_of_api se draft in
  let* () = Lwt_unix.mkdir (!Web_config.spool_dir / raw_string_of_uuid uuid) 0o700 in
  let* () = Web_persist.set_draft_election uuid se in
  let* () = Web_persist.clear_elections_by_owner_cache () in
  Lwt.return uuid

let get_drafts_voters se =
  se.se_voters
  |> List.map (fun x -> x.sv_id)

let put_drafts_voters uuid se voters =
  let existing_voters =
    List.fold_left (fun accu v -> SMap.add v.sv_id v accu) SMap.empty se.se_voters
  in
  let se_voters =
    List.map
      (fun sv_id ->
        match SMap.find_opt sv_id existing_voters with
        | None -> {sv_id; sv_password = None}
        | Some x -> x
      ) voters
  in
  let* total_weight, _, _ =
    Lwt_list.fold_left_s
      (fun (total_weight, shape, voters) {sv_id; _} ->
        if not (is_identity sv_id) then (
          Lwt.fail @@ Error (Printf.sprintf "invalid identity: %s" sv_id)
        ) else (
          let address, login, weight = split_identity_opt sv_id in
          let* () =
            match shape, login with
            | Some (true, _), None -> Lwt.fail @@ Error (Printf.sprintf "missing login in %s" sv_id)
            | Some (false, _), Some _ -> Lwt.fail @@ Error (Printf.sprintf "extra login in %s" sv_id)
            | _ -> Lwt.return_unit
          in
          let* () =
            match shape, weight with
            | Some (_, true), None -> Lwt.fail @@ Error (Printf.sprintf "missing weight in %s" sv_id)
            | Some (_, false), Some _ -> Lwt.fail @@ Error (Printf.sprintf "extra weight in %s" sv_id)
            | _ -> Lwt.return_unit
          in
          let shape =
            match shape with
            | Some _ -> shape
            | None -> Some ((login <> None), (weight <> None))
          in
          let login = String.lowercase_ascii (Option.get login address) in
          let* voters =
            if SSet.mem login voters then (
              Lwt.fail @@ Error (Printf.sprintf "duplicate login in %s" sv_id)
            ) else (
              Lwt.return (SSet.add login voters)
            )
          in
          let weight = Option.get weight Weight.one in
          Lwt.return (Weight.(total_weight + weight), shape, voters)
        )
      ) (Weight.zero, None, SSet.empty) se_voters
  in
  let* () =
    let expanded = Weight.expand ~total:total_weight total_weight in
    if Z.compare expanded Weight.max_expanded_weight > 0 then (
      Lwt.fail @@ Error (Printf.sprintf "expanded total weight too big: %s/%s" (Z.to_string expanded) (Z.to_string Weight.max_expanded_weight))
    ) else (
      Lwt.return_unit
    )
  in
  let se = {se with se_voters} in
  Web_persist.set_draft_election uuid se

let get_drafts_passwords se =
  se.se_voters
  |> List.filter_map (fun x -> Option.map (fun _ -> x.sv_id) x.sv_password)

let post_drafts_passwords generate uuid se voters =
  let se_voters =
    List.fold_left (fun accu v -> SMap.add v.sv_id v accu) SMap.empty se.se_voters
  in
  let () =
    if SMap.cardinal se_voters > !Web_config.maxmailsatonce then
      raise (Error "too many voters")
  in
  let voters =
    List.map
      (fun id ->
        match SMap.find_opt id se_voters with
        | None -> raise (Error (Printf.sprintf "voter not in voter list: %s" id))
        | Some v -> v
      ) voters
  in
  let* () =
    Lwt_list.iter_s
      (fun v ->
        let* x = generate se.se_metadata v.sv_id in
        v.sv_password <- Some x;
        Lwt.return_unit
      ) voters
  in
  Web_persist.set_draft_election uuid se

let get_draft_credentials who uuid se =
  let credentials_token =
    if se.se_metadata.e_cred_authority = Some "server" then
      None
    else
      Some se.se_public_creds
  in
  let* credentials_public = read_file ~uuid "public_creds.txt" in
  let* credentials_private =
    match who with
    | `Administrator _ -> read_file ~uuid "private_creds.txt"
    | `CredentialAuthority -> Lwt.return_none
  in
  Lwt.return {credentials_token; credentials_public; credentials_private}

type generate_credentials_on_server_error =
  [ `NoVoters
  | `TooManyVoters
  | `Already
  | `NoServer
  ]

module CG = Belenios_core.Credential.MakeGenerate (LwtRandom)

let generate_credentials_on_server send uuid se =
  let nvoters = List.length se.se_voters in
  if nvoters > !Web_config.maxmailsatonce then
    Lwt.return (Stdlib.Error `TooManyVoters)
  else if nvoters = 0 then
    Lwt.return (Stdlib.Error `NoVoters)
  else if se.se_public_creds_received then
    Lwt.return (Stdlib.Error `Already)
  else if se.se_metadata.e_cred_authority <> Some "server" then
    Lwt.return (Stdlib.Error `NoServer)
  else (
    let show_weight =
      List.exists
        (fun v ->
          let _, _, weight = split_identity_opt v.sv_id in
          weight <> None
        ) se.se_voters
    in
    let version = Option.get se.se_version 0 in
    let module G = (val Belenios.Group.of_string ~version se.se_group : GROUP) in
    let module CMap = Map.Make (G) in
    let module CD = Belenios_core.Credential.MakeDerive (G) in
    let* public_creds, private_creds =
      Lwt_list.fold_left_s (fun (public_creds, private_creds) v ->
          let recipient, login, weight = split_identity v.sv_id in
          let* cred = CG.generate () in
          let pub_cred =
            let x = CD.derive uuid cred in
            G.(g **~ x)
          in
          let* () = send ~recipient ~login ~weight ~cred in
          Lwt.return (CMap.add pub_cred weight public_creds, (v.sv_id, cred) :: private_creds)
        ) (CMap.empty, []) se.se_voters
    in
    let private_creds = List.rev_map (fun (id, c) -> id ^ " " ^ c) private_creds in
    let* () = write_file ~uuid "private_creds.txt" private_creds in
    let public_creds =
      CMap.bindings public_creds
      |> List.map
           (fun (cred, weight) ->
             let cred = G.to_string cred in
             if show_weight then
               Printf.sprintf "%s,%s" cred (Weight.to_string weight)
             else cred
           )
    in
    let* () = write_file ~uuid "public_creds.txt" public_creds in
    se.se_public_creds_received <- true;
    let* () = Web_persist.set_draft_election uuid se in
    Lwt.return (Ok ())
  )

let exn_of_generate_credentials_on_server_error = function
  | `NoVoters -> Error "no voters"
  | `TooManyVoters -> Error "too many voters"
  | `Already -> Error "already done"
  | `NoServer -> Error "credential authority is not the server"

let submit_public_credentials uuid se credentials =
  let version = Option.get se.se_version 0 in
  let module G = (val Belenios.Group.of_string ~version se.se_group : GROUP) in
  let weights =
    List.fold_left
      (fun (i, weights) x ->
        try
          let x, w = extract_weight x in
          let x = G.of_string x in
          if not (G.check x) then raise Exit;
          i + 1, w :: weights
        with _ ->
          raise (Error (Printf.sprintf "invalid credential at index %d" i))
      ) (0, []) credentials
    |> (fun (_, weights) -> List.sort Weight.compare weights)
  in
  let expected_weights =
    List.fold_left
      (fun accu {sv_id; _} ->
        let _, _, weight = split_identity sv_id in
        weight :: accu
      ) [] se.se_voters
    |> List.sort Weight.compare
  in
  if weights <> expected_weights then raise (Error "discrepancy in weights");
  let* () = write_file ~uuid "public_creds.txt" credentials in
  se.se_public_creds_received <- true;
  Web_persist.set_draft_election uuid se
