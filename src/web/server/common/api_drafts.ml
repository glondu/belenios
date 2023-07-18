(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_core.Serializable_j
open Belenios_core.Signatures
open Belenios
open Belenios_api.Serializable_j
open Web_serializable_j
open Web_common
open Api_generic

let with_administrator token se f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some a when Accounts.check a se.se_owners -> f a
  | _ -> not_found

let with_administrator_or_credential_authority token se f =
  let@ token = Option.unwrap unauthorized token in
  if token = se.se_public_creds then f `CredentialAuthority
  else
    match lookup_token token with
    | Some a when Accounts.check a se.se_owners -> f (`Administrator a)
    | _ -> not_found

let with_administrator_or_nobody token se f =
  match token with
  | None -> f `Nobody
  | Some token -> (
      match lookup_token token with
      | Some a when Accounts.check a se.se_owners -> f (`Administrator a)
      | _ -> not_found)

let with_threshold_trustee token se f =
  let@ token = Option.unwrap unauthorized token in
  match se.se_trustees with
  | `Basic _ -> not_found
  | `Threshold t -> (
      match List.find_opt (fun x -> x.stt_token = token) t.dtp_trustees with
      | Some x -> f (x, t)
      | None -> not_found)

let get_authentication se =
  match se.se_metadata.e_auth_config with
  | Some [ { auth_system = "password"; _ } ] -> `Password
  | Some [ { auth_system = "cas"; auth_config; _ } ] ->
      `CAS (List.assoc "server" auth_config)
  | Some [ { auth_system = "import"; auth_instance; _ } ] ->
      `Configured auth_instance
  | _ -> raise (Error (`Invalid "authentication"))

let auth_config_of_authentication = function
  | `Password ->
      { auth_system = "password"; auth_instance = "password"; auth_config = [] }
  | `CAS server ->
      {
        auth_system = "cas";
        auth_instance = "cas";
        auth_config = [ ("server", server) ];
      }
  | `Configured auth_instance ->
      { auth_system = "import"; auth_instance; auth_config = [] }

let api_of_draft se =
  let draft_questions =
    {
      se.se_questions with
      t_credential_authority =
        Some (Option.value se.se_metadata.e_cred_authority ~default:"");
      t_administrator = Some (Option.value se.se_administrator ~default:"");
    }
  in
  Lwt.return
    {
      draft_version = se.se_version;
      draft_owners = se.se_owners;
      draft_questions;
      draft_languages = Option.value se.se_metadata.e_languages ~default:[];
      draft_contact = se.se_metadata.e_contact;
      draft_booth = Option.value se.se_metadata.e_booth_version ~default:1;
      draft_authentication = get_authentication se;
      draft_group = se.se_group;
    }

let assert_ msg b f = if b then f () else raise (Error msg)

let draft_of_api a se d =
  let version = se.se_version in
  let () =
    if d.draft_version <> version then raise (Error (`CannotChange "version"))
  in
  let@ () =
    assert_ (`Invalid "booth version")
      (List.mem d.draft_booth supported_booth_versions)
  in
  let@ () =
    assert_ (`Invalid "languages") (List.length d.draft_languages >= 1)
  in
  let@ () = assert_ (`Invalid "owners") (List.mem a.id d.draft_owners) in
  let e_cred_authority = d.draft_questions.t_credential_authority in
  let () =
    let old = se.se_metadata.e_cred_authority in
    if e_cred_authority <> old then
      if
        se.se_public_creds_received
        && (old = Some "server" || e_cred_authority = Some "server")
      then raise (Error (`CannotChange "credential authority"))
  in
  let se_group = d.draft_group in
  let () =
    let old = se.se_group in
    if se_group <> old then
      if se.se_public_creds_received then raise (Error (`CannotChange "group"))
      else
        try
          let module G = (val Group.of_string ~version se_group) in
          ()
        with _ -> raise (Error (`Invalid "group"))
  in
  let () =
    let has_nh_questions =
      Array.exists
        (function
          | Belenios_core.Question.NonHomomorphic _ -> true | _ -> false)
        d.draft_questions.t_questions
    in
    if has_nh_questions then
      match se_group with
      | "RFC-3526-2048" | "Ed25519" -> ()
      | _ -> raise (Error (`Invalid "NH group"))
  in
  let se_metadata =
    {
      se.se_metadata with
      e_contact = d.draft_contact;
      e_languages = Some d.draft_languages;
      e_booth_version = Some d.draft_booth;
      e_cred_authority;
      e_auth_config =
        Some [ auth_config_of_authentication d.draft_authentication ];
    }
  in
  {
    se with
    se_metadata;
    se_owners = d.draft_owners;
    se_questions = d.draft_questions;
    se_administrator = d.draft_questions.t_administrator;
    se_group;
  }

let generate_uuid () =
  let length = !Web_config.uuid_length in
  let token = generate_token ?length () in
  Lwt.return (Uuid.wrap token)

let post_drafts account draft =
  let@ () =
   fun cont ->
    if !Web_config.deny_newelection then Lwt.return_none
    else Lwt.(cont () >>= return_some)
  in
  let owners = [ account.id ] in
  let* uuid = generate_uuid () in
  let token = generate_token () in
  let se_metadata =
    {
      e_owners = owners;
      e_auth_config = None;
      e_cred_authority = None;
      e_trustees = None;
      e_languages = None;
      e_contact = None;
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
      se_version = List.hd supported_crypto_versions;
      se_owners = owners;
      se_group = !Web_config.default_group;
      se_voters = [];
      se_questions;
      se_trustees = `Basic { dbp_trustees = [] };
      se_metadata;
      se_public_creds = token;
      se_public_creds_received = false;
      se_creation_date = Some (Datetime.now ());
      se_administrator = None;
      se_credential_authority_visited = false;
      se_voter_authentication_visited = false;
      se_trustees_setup_step = 1;
    }
  in
  let se = draft_of_api account se draft in
  let* () = Web_persist.create_draft uuid se in
  Lwt.return uuid

let get_draft_voters se = se.se_voters |> List.map (fun x -> x.sv_id)

let put_draft_voters uuid se voters =
  let existing_voters =
    List.fold_left
      (fun accu v ->
        let _, login, _ = Voter.get v.sv_id in
        SMap.add (String.lowercase_ascii login) v accu)
      SMap.empty se.se_voters
  in
  let se_voters =
    List.map
      (fun voter ->
        if not (Voter.validate voter) then raise @@ Error (`Invalid "identity");
        let _, login, _ = Voter.get voter in
        match SMap.find_opt (String.lowercase_ascii login) existing_voters with
        | None -> { sv_id = voter; sv_password = None }
        | Some v ->
            v.sv_id <- voter;
            v)
      voters
  in
  let* total_weight, _, _ =
    Lwt_list.fold_left_s
      (fun (total_weight, shape, voters) v ->
        let shape =
          let shape' =
            let (typ, { login; weight; _ }) : Voter.t = v.sv_id in
            match typ with
            | `Plain -> `Plain (login <> None, weight <> None)
            | `Json -> `Json
          in
          match shape with
          | Some x when x <> shape' -> raise @@ Error (`Invalid "format mix")
          | _ -> Some shape'
        in
        let _, login, weight = Voter.get v.sv_id in
        let login = String.lowercase_ascii login in
        let* voters =
          if SSet.mem login voters then
            Lwt.fail @@ Error (`Invalid "duplicate login")
          else Lwt.return (SSet.add login voters)
        in
        Lwt.return (Weight.(total_weight + weight), shape, voters))
      (Weight.zero, None, SSet.empty)
      se_voters
  in
  let* () =
    let expanded = Weight.expand ~total:total_weight total_weight in
    if Z.compare expanded Weight.max_expanded_weight > 0 then
      Lwt.fail
      @@ Error
           (`GenericError
             (Printf.sprintf "expanded total weight too big: %s/%s"
                (Z.to_string expanded)
                (Z.to_string Weight.max_expanded_weight)))
    else Lwt.return_unit
  in
  let se = { se with se_voters } in
  Web_persist.set_draft_election uuid se

let get_draft_passwords se =
  se.se_voters
  |> List.filter_map (fun x ->
         Option.map
           (fun _ ->
             let _, login, _ = Voter.get x.sv_id in
             login)
           x.sv_password)

let post_draft_passwords generate uuid se voters =
  let se_voters =
    List.fold_left
      (fun accu v ->
        let _, login, _ = Voter.get v.sv_id in
        SMap.add (String.lowercase_ascii login) v accu)
      SMap.empty se.se_voters
  in
  let () =
    if SMap.cardinal se_voters > !Web_config.maxmailsatonce then
      raise (Error (`ValidationError `TooManyVoters))
  in
  let voters =
    List.map
      (fun login ->
        match SMap.find_opt (String.lowercase_ascii login) se_voters with
        | None -> raise (Error (`Missing login))
        | Some v -> v)
      voters
  in
  let* jobs =
    Lwt_list.fold_left_s
      (fun jobs v ->
        let* job, x = generate se.se_metadata v.sv_id in
        v.sv_password <- Some x;
        Lwt.return (job :: jobs))
      [] voters
  in
  let* () = Web_persist.set_draft_election uuid se in
  Lwt.return jobs

let get_credentials_token se =
  if se.se_metadata.e_cred_authority = Some "server" then Lwt.return_none
  else Lwt.return_some se.se_public_creds

type generate_credentials_on_server_error =
  [ `NoVoters | `TooManyVoters | `Already | `NoServer ]

module CG = Belenios_core.Credential.MakeGenerate (Random)

let generate_credentials_on_server send uuid se =
  let nvoters = List.length se.se_voters in
  if nvoters > !Web_config.maxmailsatonce then
    Lwt.return (Stdlib.Error `TooManyVoters)
  else if nvoters = 0 then Lwt.return (Stdlib.Error `NoVoters)
  else if se.se_public_creds_received then Lwt.return (Stdlib.Error `Already)
  else if se.se_metadata.e_cred_authority <> Some "server" then
    Lwt.return (Stdlib.Error `NoServer)
  else
    let show_weight = has_explicit_weights se.se_voters in
    let version = se.se_version in
    let module G = (val Group.of_string ~version se.se_group : GROUP) in
    let module CMap = Map.Make (G) in
    let module CD = Belenios_core.Credential.MakeDerive (G) in
    let* public_creds, private_creds, jobs =
      Lwt_list.fold_left_s
        (fun (public_creds, private_creds, jobs) v ->
          let recipient, login, weight = Voter.get v.sv_id in
          let credential = CG.generate () in
          let pub_cred =
            let x = CD.derive uuid credential in
            G.(g **~ x)
          in
          let* job = send ~recipient ~login ~weight ~credential in
          Lwt.return
            ( CMap.add pub_cred (weight, login) public_creds,
              (login, credential) :: private_creds,
              job :: jobs ))
        (CMap.empty, [], []) se.se_voters
    in
    let private_creds =
      List.rev private_creds |> string_of_private_credentials
    in
    let* () = Web_persist.set_draft_private_credentials uuid private_creds in
    let public_creds =
      CMap.bindings public_creds
      |> List.map (fun (cred, (weight, login)) ->
             G.to_string cred
             ^ (if show_weight then
                  Printf.sprintf ",%s" (Weight.to_string weight)
                else ",")
             ^ Printf.sprintf ",%s" login)
    in
    let* () = Web_persist.set_draft_public_credentials uuid public_creds in
    se.se_public_creds_received <- true;
    let* () = Web_persist.set_draft_election uuid se in
    Lwt.return (Ok jobs)

let exn_of_generate_credentials_on_server_error = function
  | `NoVoters -> Error (`ValidationError `NoVoters)
  | `TooManyVoters -> Error (`ValidationError `TooManyVoters)
  | `Already -> Error (`GenericError "already done")
  | `NoServer -> Error (`GenericError "credential authority is not the server")

let submit_public_credentials uuid se credentials =
  let () =
    if se.se_voters = [] then raise (Error (`ValidationError `NoVoters))
  in
  let version = se.se_version in
  let module G = (val Group.of_string ~version se.se_group : GROUP) in
  let usernames =
    List.fold_left
      (fun accu { sv_id; _ } ->
        let _, username, weight = Voter.get sv_id in
        if SMap.mem username accu then
          raise
            (Error
               (`GenericError (Printf.sprintf "duplicate username %s" username)))
        else SMap.add username (weight, ref false) accu)
      SMap.empty se.se_voters
  in
  let _ =
    List.fold_left
      (fun (i, accu) x ->
        let invalid fmt =
          Printf.ksprintf
            (fun x ->
              raise
                (Error
                   (`GenericError (Printf.sprintf "invalid %s at index %d" x i))))
            fmt
        in
        let cred, weight, username =
          match String.split_on_char ',' x with
          | [ c; ""; u ] -> (G.of_string c, Weight.one, u)
          | [ c; w; u ] -> (G.of_string c, Weight.of_string w, u)
          | _ -> invalid "record"
        in
        let cred_s = G.to_string cred in
        let () =
          match SMap.find_opt username usernames with
          | None -> invalid "username %s" username
          | Some (w, used) ->
              if !used then invalid "duplicate username %s" username
              else if Weight.compare w weight <> 0 then
                invalid "differing weight"
              else if SSet.mem cred_s accu then invalid "duplicate credential"
              else if not (G.check cred) then invalid "public credential"
              else used := true
        in
        (i + 1, SSet.add cred_s accu))
      (0, SSet.empty) credentials
  in
  let* () = Web_persist.set_draft_public_credentials uuid credentials in
  se.se_public_creds_received <- true;
  Web_persist.set_draft_election uuid se

let get_draft_trustees ~is_admin se =
  match se.se_trustees with
  | `Basic x ->
      let bt_trustees =
        List.filter_map
          (fun t ->
            if t.st_id = "server" then None
            else
              let trustee_state, trustee_key =
                if t.st_public_key = "" then (Some 0, None)
                else
                  ( Some 1,
                    Some
                      (trustee_public_key_of_string Yojson.Safe.read_json
                         t.st_public_key) )
              in
              let trustee_address, trustee_token, trustee_state =
                if is_admin then (Some t.st_id, Some t.st_token, trustee_state)
                else (None, None, None)
              in
              Some
                {
                  trustee_address;
                  trustee_name = Option.value t.st_name ~default:"";
                  trustee_token;
                  trustee_state;
                  trustee_key;
                })
          x.dbp_trustees
      in
      `Basic { bt_trustees }
  | `Threshold x ->
      let tt_trustees =
        List.map
          (fun t ->
            let trustee_address, trustee_token, trustee_state =
              if is_admin then
                ( Some t.stt_id,
                  Some t.stt_token,
                  Some (Option.value t.stt_step ~default:0) )
              else (None, None, None)
            in
            {
              trustee_address;
              trustee_name = Option.value t.stt_name ~default:"";
              trustee_token;
              trustee_state;
              trustee_key = t.stt_cert;
            })
          x.dtp_trustees
      in
      `Threshold { tt_threshold = x.dtp_threshold; tt_trustees }

let check_address address =
  if not @@ is_email address then raise (Error (`Invalid "e-mail address"))

let ensure_none label x =
  if x <> None then
    raise (Error (`GenericError (Printf.sprintf "%s must not be set" label)))

let generate_server_trustee se =
  let st_id = "server" and st_token = "" in
  let version = se.se_version in
  let module G = (val Group.of_string ~version se.se_group) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module K = Trustees.MakeSimple (G) (Random) in
  let private_key = K.generate () in
  let public_key = K.prove private_key in
  let st_public_key =
    string_of_trustee_public_key (swrite G.to_string) public_key
  in
  let st_private_key = Some private_key in
  let st_name = Some "server" in
  Lwt.return { st_id; st_token; st_public_key; st_private_key; st_name }

let post_draft_trustees uuid se t =
  let address =
    match t.trustee_address with
    | Some x ->
        check_address x;
        x
    | None -> raise (Error (`Missing "address"))
  in
  let () = ensure_none "token" t.trustee_token in
  let () = ensure_none "state" t.trustee_state in
  let () = ensure_none "key" t.trustee_key in
  match se.se_trustees with
  | `Basic x ->
      let* ts =
        let ts = x.dbp_trustees in
        if List.exists (fun x -> x.st_id = "server") ts then Lwt.return ts
        else
          let* server = generate_server_trustee se in
          Lwt.return (ts @ [ server ])
      in
      let () =
        if List.exists (fun x -> x.st_id = address) ts then
          raise (Error (`GenericError "address already used"))
      in
      let st_token = generate_token () in
      let t =
        {
          st_id = address;
          st_name = Some t.trustee_name;
          st_public_key = "";
          st_private_key = None;
          st_token;
        }
      in
      x.dbp_trustees <- ts @ [ t ];
      Web_persist.set_draft_election uuid se
  | `Threshold x ->
      let ts = x.dtp_trustees in
      let () =
        if List.exists (fun x -> x.stt_id = address) ts then
          raise (Error (`GenericError "address already used"))
      in
      let stt_token = generate_token () in
      let t =
        {
          stt_id = address;
          stt_name = Some t.trustee_name;
          stt_token;
          stt_step = None;
          stt_cert = None;
          stt_polynomial = None;
          stt_vinput = None;
          stt_voutput = None;
        }
      in
      x.dtp_trustees <- ts @ [ t ];
      Web_persist.set_draft_election uuid se

let rec filter_out_first f = function
  | [] -> (false, [])
  | x :: xs ->
      if f x then (true, xs)
      else
        let touched, xs = filter_out_first f xs in
        (touched, x :: xs)

let delete_draft_trustee uuid se trustee =
  match se.se_trustees with
  | `Basic x ->
      let ts = x.dbp_trustees in
      let touched, ts = filter_out_first (fun x -> x.st_id = trustee) ts in
      if touched then (
        x.dbp_trustees <- ts;
        let* () = Web_persist.set_draft_election uuid se in
        Lwt.return_true)
      else Lwt.return_false
  | `Threshold x ->
      let ts = x.dtp_trustees in
      let touched, ts = filter_out_first (fun x -> x.stt_id = trustee) ts in
      if touched then (
        x.dtp_trustees <- ts;
        let* () = Web_persist.set_draft_election uuid se in
        Lwt.return_true)
      else Lwt.return_false

let set_threshold uuid se threshold =
  match se.se_trustees with
  | `Basic _ -> Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x when x.dtp_trustees = [] ->
      Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x ->
      let ts = x.dtp_trustees in
      let maybe_threshold, step =
        if threshold = 0 then (None, None) else (Some threshold, Some 1)
      in
      if 0 <= threshold && threshold < List.length ts then (
        List.iter (fun t -> t.stt_step <- step) ts;
        x.dtp_threshold <- maybe_threshold;
        let* () = Web_persist.set_draft_election uuid se in
        Lwt.return @@ Ok ())
      else Lwt.return @@ Stdlib.Error `OutOfBounds

let get_draft_trustees_mode se =
  match se.se_trustees with
  | `Basic _ -> `Basic
  | `Threshold x -> `Threshold (Option.value x.dtp_threshold ~default:0)

let put_draft_trustees_mode uuid se mode =
  match (get_draft_trustees_mode se, mode) with
  | a, b when a = b -> Lwt.return_unit
  | _, `Basic ->
      se.se_trustees <- `Basic { dbp_trustees = [] };
      Web_persist.set_draft_election uuid se
  | `Basic, `Threshold 0 ->
      let dtp =
        {
          dtp_threshold = None;
          dtp_trustees = [];
          dtp_parameters = None;
          dtp_error = None;
        }
      in
      se.se_trustees <- `Threshold dtp;
      Web_persist.set_draft_election uuid se
  | `Threshold _, `Threshold threshold -> (
      let* x = set_threshold uuid se threshold in
      match x with
      | Ok () -> Lwt.return_unit
      | Error `NoTrustees -> Lwt.fail (Error (`GenericError "no trustees"))
      | Error `OutOfBounds ->
          Lwt.fail (Error (`GenericError "threshold out of bounds")))
  | _, _ -> Lwt.fail (Error (`GenericError "change not allowed"))

let get_draft_status uuid se =
  let* private_credentials_downloaded =
    if se.se_metadata.e_cred_authority = Some "server" then
      let* b = Web_persist.get_private_creds_downloaded uuid in
      Lwt.return_some b
    else Lwt.return_none
  in
  Lwt.return
    {
      num_voters = List.length se.se_voters;
      passwords_ready =
        (match se.se_metadata.e_auth_config with
        | Some [ { auth_system = "password"; _ } ] ->
            Some (List.for_all (fun v -> v.sv_password <> None) se.se_voters)
        | _ -> None);
      credentials_ready = se.se_public_creds_received;
      private_credentials_downloaded;
      trustees_ready =
        (match se.se_trustees with
        | `Basic x ->
            List.for_all (fun t -> t.st_public_key <> "") x.dbp_trustees
        | `Threshold x ->
            List.for_all (fun t -> t.stt_step = Some 7) x.dtp_trustees);
      nh_and_weights_compatible =
        (let has_weights = has_explicit_weights se.se_voters in
         let has_nh =
           Array.exists
             (function
               | Belenios_core.Question.NonHomomorphic _ -> true | _ -> false)
             se.se_questions.t_questions
         in
         not (has_weights && has_nh));
      credential_authority_visited = se.se_credential_authority_visited;
      voter_authentication_visited = se.se_voter_authentication_visited;
      trustees_setup_step = se.se_trustees_setup_step;
    }

let merge_voters a b f =
  let weights =
    List.fold_left
      (fun accu sv ->
        let _, login, weight = Voter.get sv.sv_id in
        let login = String.lowercase_ascii login in
        SMap.add login weight accu)
      SMap.empty a
  in
  let rec loop weights accu = function
    | [] ->
        Ok (List.rev accu, Weight.(SMap.fold (fun _ x y -> x + y) weights zero))
    | sv_id :: xs ->
        let _, login, weight = Voter.get sv_id in
        let login = String.lowercase_ascii login in
        if SMap.mem login weights then Stdlib.Error sv_id
        else
          loop
            (SMap.add login weight weights)
            ({ sv_id; sv_password = f sv_id } :: accu)
            xs
  in
  loop weights (List.rev a) b

let import_voters uuid se from =
  let* voters = Web_persist.get_all_voters from in
  let* passwords = Web_persist.get_passwords from in
  let get_password =
    match passwords with
    | None -> fun _ -> None
    | Some p ->
        fun sv_id ->
          let _, login, _ = Voter.get sv_id in
          SMap.find_opt (String.lowercase_ascii login) p
  in
  if se.se_public_creds_received then Lwt.return @@ Stdlib.Error `Forbidden
  else
    match merge_voters se.se_voters voters get_password with
    | Ok (voters, total_weight) ->
        let expanded = Weight.expand ~total:total_weight total_weight in
        if Z.compare expanded Weight.max_expanded_weight <= 0 then (
          se.se_voters <- voters;
          let* () = Web_persist.set_draft_election uuid se in
          Lwt.return @@ Ok ())
        else Lwt.return @@ Stdlib.Error (`TotalWeightTooBig total_weight)
    | Error x ->
        let _, login, _ = Voter.get x in
        Lwt.return @@ Stdlib.Error (`Duplicate login)

let import_trustees uuid se from metadata =
  let open Belenios_core.Serializable_j in
  match metadata.e_trustees with
  | None -> Lwt.return @@ Stdlib.Error `None
  | Some names -> (
      let* trustees = Web_persist.get_trustees from in
      let version = se.se_version in
      let module G = (val Group.of_string ~version se.se_group : GROUP) in
      let module Trustees = (val Trustees.get_by_version version) in
      let module K = Trustees.MakeCombinator (G) in
      let trustees = trustees_of_string (sread G.of_string) trustees in
      if not (K.check trustees) then Lwt.return @@ Stdlib.Error `Invalid
      else
        let import_pedersen t names =
          let* privs = Web_persist.get_private_keys from in
          let* x =
            match privs with
            | Some privs ->
                let rec loop ts pubs privs accu =
                  match (ts, pubs, privs) with
                  | stt_id :: ts, vo_public_key :: pubs, vo_private_key :: privs
                    ->
                      let stt_name = vo_public_key.trustee_name in
                      let stt_token = generate_token () in
                      let stt_voutput = { vo_public_key; vo_private_key } in
                      let stt_voutput =
                        Some
                          (string_of_voutput (swrite G.to_string) stt_voutput)
                      in
                      let stt =
                        {
                          stt_id;
                          stt_token;
                          stt_voutput;
                          stt_step = Some 7;
                          stt_cert = None;
                          stt_polynomial = None;
                          stt_vinput = None;
                          stt_name;
                        }
                      in
                      loop ts pubs privs (stt :: accu)
                  | [], [], [] -> Lwt.return @@ Ok (List.rev accu)
                  | _, _, _ -> Lwt.return @@ Stdlib.Error `Inconsistent
                in
                loop names (Array.to_list t.t_verification_keys) privs []
            | None -> Lwt.return @@ Stdlib.Error `MissingPrivateKeys
          in
          match x with
          | Ok se_threshold_trustees ->
              let dtp =
                {
                  dtp_threshold = Some t.t_threshold;
                  dtp_trustees = se_threshold_trustees;
                  dtp_parameters =
                    Some (string_of_threshold_parameters (swrite G.to_string) t);
                  dtp_error = None;
                }
              in
              se.se_trustees <- `Threshold dtp;
              let* () = Web_persist.set_draft_election uuid se in
              Lwt.return @@ Ok `Threshold
          | Stdlib.Error _ as x -> Lwt.return x
        in
        match trustees with
        | [ `Pedersen t ] -> import_pedersen t names
        | [ `Single x; `Pedersen t ] when x.trustee_name = Some "server" ->
            import_pedersen t (List.tl names)
        | ts ->
            let@ ts cont =
              try
                ts
                |> List.map (function
                     | `Single x -> x
                     | `Pedersen _ -> raise Exit)
                |> cont
              with Exit -> Lwt.return @@ Stdlib.Error `Unsupported
            in
            let* ts =
              let module KG = Trustees.MakeSimple (G) (Random) in
              List.combine names ts
              |> Lwt_list.map_p (fun (st_id, public_key) ->
                     let* st_token, st_private_key, st_public_key =
                       if st_id = "server" then
                         let private_key = KG.generate () in
                         let public_key = KG.prove private_key in
                         let public_key =
                           string_of_trustee_public_key (swrite G.to_string)
                             public_key
                         in
                         Lwt.return ("", Some private_key, public_key)
                       else
                         let st_token = generate_token () in
                         let public_key =
                           string_of_trustee_public_key (swrite G.to_string)
                             public_key
                         in
                         Lwt.return (st_token, None, public_key)
                     in
                     let st_name = public_key.trustee_name in
                     Lwt.return
                       {
                         st_id;
                         st_token;
                         st_public_key;
                         st_private_key;
                         st_name;
                       })
            in
            se.se_trustees <- `Basic { dbp_trustees = ts };
            let* () = Web_persist.set_draft_election uuid se in
            Lwt.return @@ Ok `Basic)

let check_owner account uuid cont =
  let* metadata = Web_persist.get_election_metadata uuid in
  if Accounts.check account metadata.e_owners then cont metadata
  else unauthorized

let post_draft_status uuid se = function
  | `SetDownloaded ->
      let* () = Web_persist.set_private_creds_downloaded uuid in
      ok
  | `ValidateElection ->
      let* s = get_draft_status uuid se in
      let* () = Web_persist.validate_election uuid se s in
      ok
  | `SetCredentialAuthorityVisited ->
      let* () =
        if se.se_credential_authority_visited <> true then (
          se.se_credential_authority_visited <- true;
          let* () = Web_persist.set_draft_election uuid se in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `SetVoterAuthenticationVisited ->
      let* () =
        if se.se_voter_authentication_visited <> true then (
          se.se_voter_authentication_visited <- true;
          let* () = Web_persist.set_draft_election uuid se in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `SetTrusteesSetupStep i ->
      let* () =
        if se.se_trustees_setup_step <> i then (
          se.se_trustees_setup_step <- i;
          let* () = Web_persist.set_draft_election uuid se in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok

let dispatch_credentials ~token endpoint method_ body uuid se =
  match endpoint with
  | [ "token" ] -> (
      let@ _ = with_administrator token se in
      match method_ with
      | `GET -> handle_get_option (fun () -> get_credentials_token se)
      | _ -> method_not_allowed)
  | [ "private" ] -> (
      let@ _ = with_administrator token se in
      match method_ with
      | `GET ->
          handle_get_option (fun () ->
              Web_persist.get_draft_private_credentials uuid)
      | _ -> method_not_allowed)
  | [ "public" ] -> (
      match method_ with
      | `GET ->
          handle_get_option (fun () ->
              Web_persist.get_draft_public_credentials uuid)
      | `POST -> (
          let@ who = with_administrator_or_credential_authority token se in
          if se.se_public_creds_received then forbidden
          else
            let@ x = body.run public_credentials_of_string in
            match (who, x) with
            | `Administrator _, [] -> (
                let@ () = handle_generic_error in
                let send = Mails_voter.generate_credential_email uuid se in
                let* x = generate_credentials_on_server send uuid se in
                match x with
                | Ok jobs ->
                    let* () = Mails_voter.submit_bulk_emails jobs in
                    ok
                | Error e ->
                    Lwt.fail @@ exn_of_generate_credentials_on_server_error e)
            | `CredentialAuthority, credentials ->
                let@ () = handle_generic_error in
                let* () = submit_public_credentials uuid se credentials in
                ok
            | _ -> forbidden)
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch_draft ~token ~ifmatch endpoint method_ body uuid se =
  match endpoint with
  | [] -> (
      let@ who = with_administrator_or_nobody token se in
      let get () =
        let* x = api_of_draft se in
        Lwt.return @@ string_of_draft x
      in
      match (method_, who) with
      | `GET, _ -> handle_get get
      | `PUT, `Administrator account ->
          let@ () = handle_ifmatch ifmatch get in
          let@ draft = body.run draft_of_string in
          let@ () = handle_generic_error in
          let update_cache =
            draft.draft_questions.t_name <> se.se_questions.t_name
            || se.se_owners <> draft.draft_owners
          in
          let se = draft_of_api account se draft in
          let* () = Web_persist.set_draft_election uuid se in
          let* () =
            if update_cache then Web_persist.clear_elections_by_owner_cache ()
            else Lwt.return_unit
          in
          ok
      | `POST, `Administrator _ ->
          let@ () = handle_ifmatch ifmatch get in
          let@ x = body.run draft_request_of_string in
          let@ () = handle_generic_error in
          post_draft_status uuid se x
      | `DELETE, `Administrator _ ->
          let@ () = handle_ifmatch ifmatch get in
          let@ () = handle_generic_error in
          let* () = Web_persist.delete_draft uuid in
          ok
      | _ -> method_not_allowed)
  | [ "voters" ] -> (
      let@ who = with_administrator_or_credential_authority token se in
      let get () =
        let x = get_draft_voters se in
        Lwt.return @@ string_of_voter_list x
      in
      match (method_, who) with
      | `GET, _ -> handle_get get
      | `PUT, `Administrator _ ->
          let@ () = handle_ifmatch ifmatch get in
          if se.se_public_creds_received then forbidden
          else
            let@ voters = body.run voter_list_of_string in
            let@ () = handle_generic_error in
            let* () = put_draft_voters uuid se voters in
            ok
      | `POST, `Administrator account -> (
          let@ () = handle_ifmatch ifmatch get in
          let@ request = body.run voters_request_of_string in
          let@ () = handle_generic_error in
          match request with
          | `Import from -> (
              let@ _ = check_owner account from in
              let* x = import_voters uuid se from in
              match x with
              | Ok () -> ok
              | Stdlib.Error `Forbidden -> forbidden
              | Stdlib.Error `NotFound -> not_found
              | Stdlib.Error (`TotalWeightTooBig _) ->
                  Lwt.fail (Error (`GenericError "total weight too big"))
              | Stdlib.Error (`Duplicate x) ->
                  Lwt.fail (Error (`GenericError ("duplicate: " ^ x)))))
      | _ -> method_not_allowed)
  | [ "passwords" ] -> (
      let@ _ = with_administrator token se in
      let get () =
        let x = get_draft_passwords se in
        Lwt.return @@ string_of_string_list x
      in
      match method_ with
      | `GET -> handle_get get
      | `POST ->
          let@ () = handle_ifmatch ifmatch get in
          let@ voters = body.run string_list_of_string in
          let@ () = handle_generic_error in
          let generate =
            let title = se.se_questions.t_name in
            let langs = get_languages se.se_metadata.e_languages in
            let show_weight = has_explicit_weights se.se_voters in
            fun metadata id ->
              Mails_voter.generate_password_email metadata langs title uuid id
                show_weight
          in
          let* jobs = post_draft_passwords generate uuid se voters in
          let* () = Mails_voter.submit_bulk_emails jobs in
          ok
      | _ -> method_not_allowed)
  | "credentials" :: endpoint ->
      dispatch_credentials ~token endpoint method_ body uuid se
  | [ "trustees-pedersen" ] -> (
      let@ trustee, dtp = with_threshold_trustee token se in
      let get () =
        let pedersen_certs =
          List.fold_left
            (fun accu x ->
              match x.stt_cert with None -> accu | Some c -> c :: accu)
            [] dtp.dtp_trustees
          |> List.rev |> Array.of_list
        in
        let r =
          {
            pedersen_threshold = Option.value ~default:0 dtp.dtp_threshold;
            pedersen_step = Option.value ~default:0 trustee.stt_step;
            pedersen_certs;
            pedersen_vinput = trustee.stt_vinput;
            pedersen_voutput =
              Option.map
                (voutput_of_string Yojson.Safe.read_json)
                trustee.stt_voutput;
          }
        in
        Lwt.return @@ string_of_pedersen Yojson.Safe.write_json r
      in
      match method_ with `GET -> handle_get get | _ -> method_not_allowed)
  | [ "trustees" ] -> (
      let@ who = with_administrator_or_nobody token se in
      let get is_admin () =
        let open Belenios_api.Serializable_j in
        let x = get_draft_trustees ~is_admin se in
        Lwt.return @@ string_of_draft_trustees x
      in
      match (method_, who) with
      | `GET, `Nobody -> handle_get (get false)
      | `GET, `Administrator _ -> handle_get (get true)
      | `POST, `Administrator account -> (
          let@ () = handle_ifmatch ifmatch (get true) in
          let@ request = body.run trustees_request_of_string in
          let@ () = handle_generic_error in
          match request with
          | `Add trustee ->
              let* () = post_draft_trustees uuid se trustee in
              ok
          | `SetBasic ->
              let* () = put_draft_trustees_mode uuid se `Basic in
              ok
          | `SetThreshold t ->
              let* () = put_draft_trustees_mode uuid se (`Threshold t) in
              ok
          | `Import from -> (
              let@ metadata = check_owner account from in
              let* x = import_trustees uuid se from metadata in
              match x with
              | Ok _ -> ok
              | Stdlib.Error e ->
                  let msg =
                    match e with
                    | `None -> "none"
                    | `Invalid -> "invalid"
                    | `Inconsistent -> "inconsistent"
                    | `MissingPrivateKeys -> "missing private keys"
                    | `Unsupported -> "unsupported"
                  in
                  Lwt.fail (Error (`GenericError msg))))
      | _ -> method_not_allowed)
  | [ "trustees"; trustee ] -> (
      let@ _ = with_administrator token se in
      match method_ with
      | `DELETE ->
          let@ () = handle_generic_error in
          let* x = delete_draft_trustee uuid se trustee in
          if x then ok else not_found
      | _ -> method_not_allowed)
  | [ "status" ] -> (
      let@ _ = with_administrator token se in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = get_draft_status uuid se in
          Lwt.return (200, string_of_draft_status x)
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch ~token ~ifmatch endpoint method_ body =
  match endpoint with
  | [] -> (
      let@ token = Option.unwrap unauthorized token in
      let@ account = Option.unwrap unauthorized (lookup_token token) in
      let get () =
        let* elections = Web_persist.get_elections_by_owner account.id in
        let elections =
          List.fold_left
            (fun accu (kind, summary_uuid, date, summary_name) ->
              let summary_date = Datetime.to_unixfloat date in
              let summary_kind = None in
              if kind = `Draft then
                { summary_uuid; summary_name; summary_date; summary_kind }
                :: accu
              else accu)
            [] elections
        in
        Lwt.return @@ string_of_summary_list elections
      in
      match method_ with
      | `GET -> handle_get get
      | `POST -> (
          let@ () = handle_ifmatch ifmatch get in
          let@ draft = body.run draft_of_string in
          let@ () = handle_generic_error in
          let* uuid = post_drafts account draft in
          match uuid with
          | Some uuid -> Lwt.return (200, string_of_uuid uuid)
          | None -> forbidden)
      | _ -> method_not_allowed)
  | uuid :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let* se = Web_persist.get_draft_election uuid in
      let@ se = Option.unwrap not_found se in
      dispatch_draft ~token ~ifmatch endpoint method_ body uuid se
