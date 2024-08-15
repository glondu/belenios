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
open Belenios
open Belenios_api.Serializable_j
open Belenios_api.Common
open Belenios_server_core
open Web_common
open Api_generic

let with_administrator token (Draft (_, se)) f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some a when Accounts.check a se.se_owners -> f a
  | _ -> not_found

let with_administrator_or_credential_authority token (Draft (_, se)) f =
  let@ token = Option.unwrap unauthorized token in
  if token = se.se_public_creds then f `CredentialAuthority
  else
    match lookup_token token with
    | Some a when Accounts.check a se.se_owners -> f (`Administrator a)
    | _ -> not_found

let with_administrator_or_nobody token (Draft (_, se)) f =
  match token with
  | None -> f `Nobody
  | Some token -> (
      match lookup_token token with
      | Some a when Accounts.check a se.se_owners -> f (`Administrator a)
      | _ -> not_found)

let with_trustee token (Draft (_, se)) f =
  let@ token = Option.unwrap unauthorized token in
  match se.se_trustees with
  | `Basic b -> (
      match List.find_opt (fun x -> x.st_token = token) b.dbp_trustees with
      | Some x -> f (`Basic x)
      | None -> unauthorized)
  | `Threshold t -> (
      match
        List.findi
          (fun i x -> if x.stt_token = token then Some (i, x) else None)
          t.dtp_trustees
      with
      | Some (i, x) -> f (`Threshold (i + 1, x, t))
      | None -> unauthorized)

let authentication_of_auth_config = function
  | Some [ { auth_system = "password"; _ } ] -> Some `Password
  | Some [ { auth_system = "cas"; auth_config; _ } ] ->
      Some (`CAS (List.assoc "server" auth_config))
  | Some [ { auth_system = "import"; auth_instance; _ } ] ->
      Some (`Configured auth_instance)
  | _ -> None

let get_authentication se =
  match authentication_of_auth_config se.se_metadata.e_auth_config with
  | Some x -> x
  | None -> raise (Error (`Invalid "authentication"))

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

let api_of_draft (Draft (v, se)) =
  let draft_questions =
    {
      se.se_questions with
      t_credential_authority =
        Some (Option.value se.se_metadata.e_cred_authority ~default:"");
      t_administrator = Some (Option.value se.se_administrator ~default:"");
    }
  in
  Lwt.return
    (Belenios_api.Common.Draft
       ( v,
         {
           draft_version = se.se_version;
           draft_owners = se.se_owners;
           draft_questions;
           draft_languages = Option.value se.se_metadata.e_languages ~default:[];
           draft_contact = se.se_metadata.e_contact;
           draft_booth = Option.value se.se_metadata.e_booth_version ~default:1;
           draft_authentication = get_authentication se;
           draft_group = se.se_group;
         } ))

let assert_ msg b f = if b then f () else raise (Error msg)

let draft_of_api a uuid (Draft (v, se) as fse)
    (Belenios_api.Common.Draft (v', d)) =
  let version = se.se_version in
  let@ Refl =
   fun cont ->
    match Belenios.Election.compare_version v v' with
    | Some x -> cont x
    | None -> raise (Error (`CannotChange "version"))
  in
  let@ () =
    assert_ (`Invalid "booth version")
      (List.mem d.draft_booth Defaults.supported_booth_versions)
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
        Web_persist.get_credentials_status uuid fse <> `None
        && (old = Some "server" || e_cred_authority = Some "server")
      then raise (Error (`CannotChange "credential authority"))
  in
  let se_group = d.draft_group in
  let () =
    let old = se.se_group in
    if se_group <> old then
      if Web_persist.get_credentials_status uuid fse <> `None then
        raise (Error (`CannotChange "group"))
      else
        try
          let module G = (val Group.of_string ~version se_group) in
          ()
        with _ -> raise (Error (`Invalid "group"))
  in
  let () =
    let has_nh_questions =
      Belenios.Election.has_nh_questions (Template (v, d.draft_questions))
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
  |> fun x -> Draft (v, x)

let post_drafts account s draft =
  let@ () =
   fun cont -> if !Web_config.deny_newelection then Lwt.return_none else cont ()
  in
  let owners = [ account.id ] in
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
      e_billing_request = None;
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
  let se_version = Defaults.version in
  let (Version v) = Belenios.Election.version_of_int se_version in
  let se =
    {
      se_version;
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
      se_pending_credentials = false;
    }
  in
  let module S = (val s : Storage.BACKEND) in
  let* uuid = S.new_election () in
  let&* uuid = uuid in
  let se = draft_of_api account uuid (Draft (v, se)) draft in
  let* () = Web_persist.create_draft s uuid se in
  Lwt.return_some uuid

let get_draft_voters (Draft (_, se)) =
  se.se_voters |> List.map (fun x -> x.sv_id)

let put_draft_voters (Draft (v, se), set) voters =
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
  set (Draft (v, se))

let get_draft_passwords (Draft (_, se)) =
  se.se_voters
  |> List.filter_map (fun x ->
         Option.map
           (fun _ ->
             let _, login, _ = Voter.get x.sv_id in
             login)
           x.sv_password)

let post_draft_passwords account generate (Draft (v, se), set) voters =
  let se_voters =
    List.fold_left
      (fun accu v ->
        let _, login, _ = Voter.get v.sv_id in
        SMap.add (String.lowercase_ascii login) v accu)
      SMap.empty se.se_voters
  in
  let () =
    if SMap.cardinal se_voters > Accounts.max_voters account then
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
  let* () = set (Draft (v, se)) in
  Lwt.return jobs

let get_credentials_token (Draft (_, se)) =
  if se.se_metadata.e_cred_authority = Some "server" then Lwt.return_none
  else Lwt.return_some se.se_public_creds

type generate_credentials_on_server_error =
  [ `NoVoters | `TooManyVoters | `Already | `NoServer ]

let generate_credentials_on_server account uuid (Draft (_, se) as draft) =
  let nvoters = List.length se.se_voters in
  if nvoters > Accounts.max_voters account then
    Lwt.return (Stdlib.Error `TooManyVoters)
  else if nvoters = 0 then Lwt.return (Stdlib.Error `NoVoters)
  else if Web_persist.get_credentials_status uuid draft <> `None then
    Lwt.return (Stdlib.Error `Already)
  else if se.se_metadata.e_cred_authority <> Some "server" then
    Lwt.return (Stdlib.Error `NoServer)
  else
    let () = Web_persist.generate_credentials_on_server_async uuid draft in
    Lwt.return (Ok ())

let exn_of_generate_credentials_on_server_error = function
  | `NoVoters -> Error (`ValidationError `NoVoters)
  | `TooManyVoters -> Error (`ValidationError `TooManyVoters)
  | `Already -> Error (`GenericError "already done")
  | `NoServer -> Error (`GenericError "credential authority is not the server")

let submit_public_credentials s uuid (Draft (v, se), set) credentials =
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
  let _, _, _, salts =
    List.fold_left
      (fun (i, creds, saltset, salts) x ->
        let invalid fmt =
          Printf.ksprintf
            (fun x ->
              raise
                (Error
                   (`GenericError (Printf.sprintf "invalid %s at index %d" x i))))
            fmt
        in
        let p = parse_public_credential G.of_string x in
        let weight = Option.value ~default:Weight.one p.weight in
        let username =
          match p.username with Some u -> u | None -> invalid "record"
        in
        let cred_s = G.to_string p.credential in
        let () =
          match SMap.find_opt username usernames with
          | None -> invalid "username %s" username
          | Some (w, used) ->
              if !used then invalid "duplicate username %s" username
              else if Weight.compare w weight <> 0 then
                invalid "differing weight"
              else if SSet.mem cred_s creds then invalid "duplicate credential"
              else if
                match p.salt with None -> false | Some s -> SSet.mem s saltset
              then invalid "duplicate salt"
              else if not (G.check p.credential) then
                invalid "public credential"
              else used := true
        in
        match p.salt with
        | None -> (i + 1, SSet.add cred_s creds, saltset, salts)
        | Some s ->
            (i + 1, SSet.add cred_s creds, SSet.add s saltset, s :: salts))
      (0, SSet.empty, SSet.empty, [])
      credentials
  in
  let salts = List.rev salts in
  let* () = Spool.create s uuid Spool.draft_public_credentials credentials in
  let* () =
    if salts <> [] then Spool.create s uuid Spool.salts salts
    else Lwt.return_unit
  in
  se.se_public_creds_received <- true;
  set (Draft (v, se))

let get_draft_trustees ~is_admin (Draft (_, se)) =
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
                         Yojson.Safe.read_json t.st_public_key) )
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

let generate_server_trustee (Draft (_, se)) =
  let st_id = "server" and st_token = "" in
  let version = se.se_version in
  let module G = (val Group.of_string ~version se.se_group) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module K = Trustees.MakeSimple (G) (Random) in
  let private_key = K.generate () in
  let public_key = K.prove private_key in
  let st_public_key =
    string_of_trustee_public_key (swrite G.to_string) (swrite G.Zq.to_string)
      public_key
  in
  let st_private_key = Some (`String (private_key |> G.Zq.to_string)) in
  let st_name = Some "server" in
  Lwt.return { st_id; st_token; st_public_key; st_private_key; st_name }

let post_draft_trustees (Draft (v, se), set) t =
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
          let* server = generate_server_trustee (Draft (v, se)) in
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
      set (Draft (v, se))
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
      set (Draft (v, se))

let rec filter_out_first f = function
  | [] -> (false, [])
  | x :: xs ->
      if f x then (true, xs)
      else
        let touched, xs = filter_out_first f xs in
        (touched, x :: xs)

let delete_draft_trustee (Draft (v, se), set) trustee =
  match se.se_trustees with
  | `Basic x ->
      let ts = x.dbp_trustees in
      let touched, ts = filter_out_first (fun x -> x.st_id = trustee) ts in
      if touched then (
        x.dbp_trustees <- ts;
        let* () = set (Draft (v, se)) in
        Lwt.return_true)
      else Lwt.return_false
  | `Threshold x ->
      let ts = x.dtp_trustees in
      let touched, ts = filter_out_first (fun x -> x.stt_id = trustee) ts in
      if touched then (
        x.dtp_trustees <- ts;
        let* () = set (Draft (v, se)) in
        Lwt.return_true)
      else Lwt.return_false

let set_threshold (Draft (v, se), set) threshold =
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
        let* () = set (Draft (v, se)) in
        Lwt.return @@ Ok ())
      else Lwt.return @@ Stdlib.Error `OutOfBounds

let get_draft_trustees_mode (Draft (_, se)) =
  match se.se_trustees with
  | `Basic _ -> `Basic
  | `Threshold x -> `Threshold (Option.value x.dtp_threshold ~default:0)

let put_draft_trustees_mode (Draft (v, se), set) mode =
  match (get_draft_trustees_mode (Draft (v, se)), mode) with
  | a, b when a = b -> Lwt.return_unit
  | _, `Basic ->
      se.se_trustees <- `Basic { dbp_trustees = [] };
      set (Draft (v, se))
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
      set (Draft (v, se))
  | `Threshold _, `Threshold threshold -> (
      let* x = set_threshold (Draft (v, se), set) threshold in
      match x with
      | Ok () -> Lwt.return_unit
      | Error `NoTrustees -> Lwt.fail (Error (`GenericError "no trustees"))
      | Error `OutOfBounds ->
          Lwt.fail (Error (`GenericError "threshold out of bounds")))
  | _, _ -> Lwt.fail (Error (`GenericError "change not allowed"))

let get_draft_status s uuid (Draft (v, se)) =
  let* private_credentials_downloaded =
    if se.se_metadata.e_cred_authority = Some "server" then
      let* b = Web_persist.get_private_creds_downloaded s uuid in
      Lwt.return_some b
    else Lwt.return_none
  in
  let credentials_ready, credentials_left =
    match Web_persist.get_credentials_status uuid (Draft (v, se)) with
    | `None -> (false, None)
    | `Pending n -> (false, Some n)
    | `Done -> (true, None)
  in
  let has_weights = has_explicit_weights se.se_voters in
  let restricted_mode_error =
    if !Web_config.restricted_mode then
      if se.se_metadata.e_cred_authority = Some "server" then
        Some `AutoCredentials
      else if
        match se.se_metadata.e_auth_config with
        | Some [ { auth_system = "import"; _ } ] -> false
        | _ -> true
      then Some `VoterAuthentication
      else if Belenios.Election.has_nh_questions (Template (v, se.se_questions))
      then Some `ForbiddenQuestions
      else if has_weights then Some `HasWeights
      else if se.se_group <> "Ed25519" then Some `BadGroup
      else
        match se.se_trustees with
        | `Basic _ -> Some `NoThreshold
        | `Threshold { dtp_trustees = _ :: _ :: _; _ } -> None
        | _ -> Some `NotEnoughTrustees
    else None
  in
  Lwt.return
    {
      num_voters = List.length se.se_voters;
      passwords_ready =
        (match se.se_metadata.e_auth_config with
        | Some [ { auth_system = "password"; _ } ] ->
            Some (List.for_all (fun v -> v.sv_password <> None) se.se_voters)
        | _ -> None);
      credentials_ready;
      credentials_left;
      private_credentials_downloaded;
      trustees_ready =
        (match se.se_trustees with
        | `Basic x ->
            List.for_all (fun t -> t.st_public_key <> "") x.dbp_trustees
        | `Threshold x ->
            List.for_all (fun t -> t.stt_step = Some 7) x.dtp_trustees);
      nh_and_weights_compatible =
        (let has_nh =
           Belenios.Election.has_nh_questions (Template (v, se.se_questions))
         in
         not (has_weights && has_nh));
      credential_authority_visited = se.se_credential_authority_visited;
      voter_authentication_visited = se.se_voter_authentication_visited;
      trustees_setup_step = se.se_trustees_setup_step;
      restricted_mode_error;
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

let get_passwords s uuid =
  let module S = (val s : Storage.BACKEND) in
  let* csv = S.get (Election (uuid, Passwords)) in
  let&* csv = csv in
  let* csv =
    Lwt_preemptive.detach (fun db -> Csv.(input_all (of_string db))) csv
  in
  let res =
    List.fold_left
      (fun accu line ->
        match line with
        | [ login; salt; hash ] ->
            SMap.add (String.lowercase_ascii login) (salt, hash) accu
        | _ -> accu)
      SMap.empty csv
  in
  Lwt.return_some res

let import_voters s uuid (Draft (v, se), set) from =
  let* voters = Web_persist.get_all_voters s from in
  let* passwords = get_passwords s from in
  let get_password =
    match passwords with
    | None -> fun _ -> None
    | Some p ->
        fun sv_id ->
          let _, login, _ = Voter.get sv_id in
          SMap.find_opt (String.lowercase_ascii login) p
  in
  if Web_persist.get_credentials_status uuid (Draft (v, se)) <> `None then
    Lwt.return @@ Stdlib.Error `Forbidden
  else
    match merge_voters se.se_voters voters get_password with
    | Ok (voters, total_weight) ->
        let expanded = Weight.expand ~total:total_weight total_weight in
        if Z.compare expanded Weight.max_expanded_weight <= 0 then (
          se.se_voters <- voters;
          let* () = set (Draft (v, se)) in
          Lwt.return @@ Ok ())
        else Lwt.return @@ Stdlib.Error (`TotalWeightTooBig total_weight)
    | Error x ->
        let _, login, _ = Voter.get x in
        Lwt.return @@ Stdlib.Error (`Duplicate login)

let import_trustees (Draft (v, se), set) s from metadata =
  match metadata.e_trustees with
  | None -> Lwt.return @@ Stdlib.Error `None
  | Some names -> (
      let* trustees = Public_archive.get_trustees s from in
      let version = se.se_version in
      let module G = (val Group.of_string ~version se.se_group : GROUP) in
      let module Trustees = (val Trustees.get_by_version version) in
      let module K = Trustees.MakeCombinator (G) in
      let trustees =
        trustees_of_string (sread G.of_string) (sread G.Zq.of_string) trustees
      in
      if not (K.check trustees) then Lwt.return @@ Stdlib.Error `Invalid
      else
        let import_pedersen t names =
          let* privs = Spool.get s from Spool.private_keys in
          let* x =
            match privs with
            | Some privs ->
                let rec loop ts pubs privs accu =
                  match (ts, pubs, privs) with
                  | ( stt_id :: ts,
                      (vo_public_key : _ trustee_public_key) :: pubs,
                      vo_private_key :: privs ) ->
                      let stt_name = vo_public_key.trustee_name in
                      let stt_token = generate_token () in
                      let stt_voutput = { vo_public_key; vo_private_key } in
                      let stt_voutput =
                        Some
                          (string_of_voutput (swrite G.to_string)
                             (swrite G.Zq.to_string) stt_voutput)
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
                    Some
                      (string_of_threshold_parameters (swrite G.to_string)
                         (swrite G.Zq.to_string) t);
                  dtp_error = None;
                }
              in
              se.se_trustees <- `Threshold dtp;
              let* () = set (Draft (v, se)) in
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
                             (swrite G.Zq.to_string) public_key
                         in
                         Lwt.return
                           ( "",
                             Some (`String (G.Zq.to_string private_key)),
                             public_key )
                       else
                         let st_token = generate_token () in
                         let public_key =
                           string_of_trustee_public_key (swrite G.to_string)
                             (swrite G.Zq.to_string) public_key
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
            let* () = set (Draft (v, se)) in
            Lwt.return @@ Ok `Basic)

let check_owner account s uuid cont =
  let* metadata = Web_persist.get_election_metadata s uuid in
  if Accounts.check account metadata.e_owners then cont metadata
  else unauthorized

let post_draft_status ~admin_id s uuid (Draft (v, se), set) = function
  | `SetDownloaded ->
      let* () = Web_persist.set_private_creds_downloaded s uuid in
      ok
  | `ValidateElection ->
      let* status = get_draft_status s uuid (Draft (v, se)) in
      let* () =
        Web_persist.validate_election ~admin_id s uuid
          (Draft (v, se), set)
          status
      in
      ok
  | `SetCredentialAuthorityVisited ->
      let* () =
        if se.se_credential_authority_visited <> true then (
          se.se_credential_authority_visited <- true;
          let* () = set (Draft (v, se)) in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `SetVoterAuthenticationVisited ->
      let* () =
        if se.se_voter_authentication_visited <> true then (
          se.se_voter_authentication_visited <- true;
          let* () = set (Draft (v, se)) in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `SetTrusteesSetupStep i ->
      let* () =
        if se.se_trustees_setup_step <> i then (
          se.se_trustees_setup_step <- i;
          let* () = set (Draft (v, se)) in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok

let post_trustee_basic ((Draft (_, se) as fse), set) ~token data =
  let ts =
    match se.se_trustees with
    | `Basic x -> x.dbp_trustees
    | `Threshold _ -> failwith "Wrong trustee mode"
  in
  let t =
    match List.find_opt (fun x -> token = x.st_token) ts with
    | Some t -> t
    | None -> failwith "Invalid token"
  in
  match t.st_public_key with
  | "" ->
      let version = se.se_version in
      let module G = (val Group.of_string ~version se.se_group : GROUP) in
      let module Trustees = (val Trustees.get_by_version version) in
      let pk =
        trustee_public_key_of_string (sread G.of_string) (sread G.Zq.of_string)
          data
      in
      let module K = Trustees.MakeCombinator (G) in
      if K.check [ `Single pk ] then (
        t.st_public_key <-
          string_of_trustee_public_key (swrite G.to_string)
            (swrite G.Zq.to_string) pk;
        set fse)
      else raise @@ Error `InvalidPublicKey
  | _ -> raise @@ Error `PublicKeyExists

let post_trustee_threshold ((Draft (_, se) as fse), set) ~token data =
  let version = se.se_version in
  let module G = (val Group.of_string ~version se.se_group : GROUP) in
  let se_trustees =
    se.se_trustees
    |> string_of_draft_trustees Yojson.Safe.write_json
    |> draft_trustees_of_string (sread G.Zq.of_string)
  in
  let dtp =
    match se_trustees with
    | `Basic _ -> failwith "Wrong trustee mode"
    | `Threshold x -> x
  in
  let ts = Array.of_list dtp.dtp_trustees in
  let threshold =
    match dtp.dtp_threshold with
    | Some t -> t
    | None -> failwith "No threshold set"
  in
  let i, t =
    match
      Array.findi
        (fun i x -> if token = x.stt_token then Some (i, x) else None)
        ts
    with
    | Some (i, t) -> (i, t)
    | None -> failwith "Trustee not found"
  in
  let context =
    { group = se.se_group; size = Array.length ts; threshold; index = i + 1 }
  in
  let get_certs () =
    Array.map
      (fun x ->
        match x.stt_cert with
        | None -> failwith "Missing certificate"
        | Some y -> y)
      ts
  in
  let get_polynomials () =
    Array.map
      (fun x ->
        match x.stt_polynomial with
        | None -> failwith "Missing polynomial"
        | Some y -> y)
      ts
  in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Trustees.MakePKI (G) (Random) in
  let module C = Trustees.MakeChannels (G) (Random) (P) in
  let module K = Trustees.MakePedersen (G) (Random) (P) (C) in
  let () =
    match t.stt_step with
    | Some 1 ->
        let cert = cert_of_string (sread G.Zq.of_string) data in
        if K.step1_check context cert then (
          t.stt_cert <- Some cert;
          t.stt_step <- Some 2)
        else failwith "Invalid certificate"
    | Some 3 ->
        let certs = get_certs () in
        let polynomial = polynomial_of_string (sread G.Zq.of_string) data in
        if K.step3_check certs i polynomial then (
          t.stt_polynomial <- Some polynomial;
          t.stt_step <- Some 4)
        else failwith "Invalid polynomial"
    | Some 5 ->
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutput =
          voutput_of_string (sread G.of_string) (sread G.Zq.of_string) data
        in
        if K.step5_check certs i polynomials voutput then (
          t.stt_voutput <-
            Some
              (string_of_voutput (swrite G.to_string) (swrite G.Zq.to_string)
                 voutput);
          t.stt_step <- Some 6)
        else failwith "Invalid voutput"
    | _ -> failwith "Invalid step"
  in
  let () =
    if Array.for_all (fun x -> x.stt_step = Some 2) ts then
      try
        let threshold = K.step2 (get_certs ()) in
        assert (dtp.dtp_threshold = Some threshold);
        Array.iter (fun x -> x.stt_step <- Some 3) ts
      with e -> dtp.dtp_error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun x -> x.stt_step = Some 4) ts then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let vinputs = K.step4 certs polynomials in
        for j = 0 to Array.length ts - 1 do
          ts.(j).stt_vinput <- Some vinputs.(j)
        done;
        Array.iter (fun x -> x.stt_step <- Some 5) ts
      with e -> dtp.dtp_error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun x -> x.stt_step = Some 6) ts then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutputs =
          Array.map
            (fun x ->
              match x.stt_voutput with
              | None -> failwith "Missing voutput"
              | Some y ->
                  voutput_of_string (sread G.of_string) (sread G.Zq.of_string) y)
            ts
        in
        let p = K.step6 certs polynomials voutputs in
        dtp.dtp_parameters <-
          Some
            (string_of_threshold_parameters (swrite G.to_string)
               (swrite G.Zq.to_string) p);
        Array.iter (fun x -> x.stt_step <- Some 7) ts
      with e -> dtp.dtp_error <- Some (Printexc.to_string e)
  in
  se.se_trustees <-
    se_trustees
    |> string_of_draft_trustees (swrite G.Zq.to_string)
    |> draft_trustees_of_string Yojson.Safe.read_json;
  set fse

let dispatch_credentials ~token endpoint method_ body s uuid (se, set) =
  let module S = (val s : Storage.BACKEND) in
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
          handle_get_option (fun () -> S.get (Election (uuid, Private_creds)))
      | _ -> method_not_allowed)
  | [ "public" ] -> (
      match method_ with
      | `GET ->
          handle_get_option (fun () ->
              Web_persist.get_draft_public_credentials s uuid)
      | `POST -> (
          let@ who = with_administrator_or_credential_authority token se in
          if Web_persist.get_credentials_status uuid se <> `None then forbidden
          else
            let@ x = body.run public_credentials_of_string in
            match (who, x) with
            | `Administrator account, [] -> (
                let@ () = handle_generic_error in
                let* x = generate_credentials_on_server account uuid se in
                match x with
                | Ok () -> ok
                | Error e ->
                    Lwt.fail @@ exn_of_generate_credentials_on_server_error e)
            | `CredentialAuthority, credentials ->
                let@ () = handle_generic_error in
                let* () =
                  submit_public_credentials s uuid (se, set) credentials
                in
                ok
            | _ -> forbidden)
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch_draft ~token ~ifmatch endpoint method_ body s uuid (se, set) =
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
          let se = draft_of_api account uuid se draft in
          let* () = set se in
          ok
      | `POST, `Administrator a ->
          let@ () = handle_ifmatch ifmatch get in
          let@ x = body.run draft_request_of_string in
          let@ () = handle_generic_error in
          post_draft_status ~admin_id:a.id s uuid (se, set) x
      | `DELETE, `Administrator _ ->
          let@ () = handle_ifmatch ifmatch get in
          let@ () = handle_generic_error in
          let* () = Web_persist.delete_draft s uuid in
          ok
      | _ -> method_not_allowed)
  | [ "election" ] -> (
      let get () =
        let (Draft (v, se)) = se in
        let version = se.se_version in
        let group = se.se_group in
        let module G = (val Group.of_string ~version group : GROUP) in
        let public_key = G.to_string G.g in
        Lwt.return
        @@ Election.make_raw_election ~version
             (Template (v, se.se_questions))
             ~uuid ~group ~public_key
      in
      match method_ with `GET -> handle_get get | _ -> method_not_allowed)
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
          if Web_persist.get_credentials_status uuid se <> `None then forbidden
          else
            let@ voters = body.run voter_list_of_string in
            let@ () = handle_generic_error in
            let* () = put_draft_voters (se, set) voters in
            ok
      | `POST, `Administrator account -> (
          let@ () = handle_ifmatch ifmatch get in
          let@ request = body.run voters_request_of_string in
          let@ () = handle_generic_error in
          match request with
          | `Import from -> (
              let@ _ = check_owner account s from in
              let* x = import_voters s uuid (se, set) from in
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
      let@ account = with_administrator token se in
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
            let (Draft (_, se)) = se in
            let title = se.se_questions.t_name in
            let langs = get_languages se.se_metadata.e_languages in
            let show_weight = has_explicit_weights se.se_voters in
            fun metadata id ->
              Mails_voter.generate_password_email metadata langs title uuid id
                show_weight
          in
          let* jobs = post_draft_passwords account generate (se, set) voters in
          let* () = Mails_voter.submit_bulk_emails jobs in
          ok
      | _ -> method_not_allowed)
  | "credentials" :: endpoint ->
      dispatch_credentials ~token endpoint method_ body s uuid (se, set)
  | [ "trustee" ] -> (
      let@ trustee = with_trustee token se in
      let get () =
        let@ () =
         fun cont ->
          Lwt.return
          @@ string_of_trustee_status Yojson.Safe.write_json
               Yojson.Safe.write_json
          @@ cont ()
        in
        match trustee with
        | `Basic b -> `Basic (if b.st_public_key = "" then `Init else `Done)
        | `Threshold (index, t, dtp) -> (
            let@ () = fun cont -> `Threshold (cont ()) in
            match dtp.dtp_threshold with
            | None -> `Init
            | Some threshold -> (
                let (Draft (_, draft)) = se in
                let pedersen_context =
                  {
                    group = draft.se_group;
                    size = List.length dtp.dtp_trustees;
                    threshold;
                    index;
                  }
                in
                match t.stt_cert with
                | None -> `WaitingForCertificate pedersen_context
                | Some _ -> (
                    try
                      let pedersen_certs =
                        List.map
                          (fun x ->
                            match x.stt_cert with
                            | None -> raise Exit
                            | Some c -> c)
                          dtp.dtp_trustees
                        |> Array.of_list
                      in
                      `Pedersen
                        {
                          pedersen_context;
                          pedersen_step = Option.value ~default:0 t.stt_step;
                          pedersen_certs;
                          pedersen_vinput = t.stt_vinput;
                          pedersen_voutput =
                            Option.map
                              (voutput_of_string Yojson.Safe.read_json
                                 Yojson.Safe.read_json)
                              t.stt_voutput;
                        }
                    with Exit -> `WaitingForOtherCertificates)))
      in
      match method_ with
      | `GET -> handle_get get
      | `POST -> (
          let@ data = body.run Fun.id in
          let@ () = handle_generic_error in
          match trustee with
          | `Basic b ->
              let* () = post_trustee_basic (se, set) ~token:b.st_token data in
              ok
          | `Threshold (_, t, _) ->
              let* () =
                post_trustee_threshold (se, set) ~token:t.stt_token data
              in
              ok)
      | _ -> method_not_allowed)
  | [ "trustees" ] -> (
      let@ who = with_administrator_or_nobody token se in
      let get is_admin () =
        let open Belenios_api.Serializable_j in
        let x = get_draft_trustees ~is_admin se in
        Lwt.return
        @@ string_of_draft_trustees Yojson.Safe.write_json
             Yojson.Safe.write_json x
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
              let* () = post_draft_trustees (se, set) trustee in
              ok
          | `SetBasic ->
              let* () = put_draft_trustees_mode (se, set) `Basic in
              ok
          | `SetThreshold t ->
              let* () = put_draft_trustees_mode (se, set) (`Threshold t) in
              ok
          | `Import from -> (
              let@ metadata = check_owner account s from in
              let* x = import_trustees (se, set) s from metadata in
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
          let* x = delete_draft_trustee (se, set) trustee in
          if x then ok else not_found
      | _ -> method_not_allowed)
  | [ "status" ] -> (
      let@ _ = with_administrator token se in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = get_draft_status s uuid se in
          Lwt.return (200, string_of_draft_status x)
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch s ~token ~ifmatch endpoint method_ body =
  match endpoint with
  | [] -> (
      let@ token = Option.unwrap unauthorized token in
      let@ account = Option.unwrap unauthorized (lookup_token token) in
      let get () =
        let* elections = Storage.get_elections_by_owner account.id in
        let elections =
          List.fold_left
            (fun accu ({ state; _ } as x) ->
              if state = `Draft then x :: accu else accu)
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
          let* uuid = post_drafts account s draft in
          match uuid with
          | Some uuid -> Lwt.return (200, string_of_uuid uuid)
          | None -> forbidden)
      | _ -> method_not_allowed)
  | uuid :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.wrap uuid) in
      let* se = Spool.update s uuid Spool.draft in
      let@ se = Option.unwrap not_found se in
      dispatch_draft ~token ~ifmatch endpoint method_ body s uuid se
