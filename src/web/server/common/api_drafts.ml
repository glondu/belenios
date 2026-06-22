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
open Belenios_web_api
open Belenios_storage_api
open Belenios_server_core
open Web_common
open Api_generic

let with_administrator token (Draft (_, se)) f =
  let@ token = Option.unwrap unauthorized token in
  match lookup_token token with
  | Some (a, _) when Accounts.check a se.owners -> f a
  | _ -> not_found

let with_administrator_or_credential_authority token (Draft (_, se)) f =
  let@ token = Option.unwrap unauthorized token in
  if token = se.public_creds then f `CredentialAuthority
  else
    match lookup_token token with
    | Some (a, _) when Accounts.check a se.owners -> f (`Administrator a)
    | _ -> not_found

let with_administrator_or_nobody token (Draft (_, se)) f =
  match token with
  | None -> f `Nobody
  | Some token -> (
      match lookup_token token with
      | Some (a, _) when Accounts.check a se.owners -> f (`Administrator a)
      | _ -> not_found)

let with_trustee token (Draft (_, se)) f =
  let@ token = Option.unwrap unauthorized token in
  match se.trustees with
  | `Basic b -> (
      match
        List.find_opt (fun (x : _ draft_trustee) -> x.token = token) b.trustees
      with
      | Some x -> f (`Basic x)
      | None -> unauthorized)
  | `Threshold t -> (
      match
        List.findi
          (fun i (x : _ draft_threshold_trustee) ->
            if x.token = token then Some (i, x) else None)
          t.trustees
      with
      | Some (i, x) -> f (`Threshold (i + 1, x, t))
      | None -> unauthorized)

let authentication_of_auth_config = function
  | Some [ { auth_system = "cas"; auth_config; _ } ] ->
      Some (`CAS (List.assoc "server" auth_config))
  | Some [ { auth_system = "import"; auth_instance; _ } ] ->
      Some (`Configured auth_instance)
  | _ -> None

let get_authentication se =
  authentication_of_auth_config se.metadata.auth_config

let auth_config_of_authentication = function
  | Some (`CAS server) ->
      Some
        [
          {
            auth_system = "cas";
            auth_instance = "cas";
            auth_config = [ ("server", server) ];
            auth_portal = None;
          };
        ]
  | Some (`Configured auth_instance) ->
      Some
        [
          {
            auth_system = "import";
            auth_instance;
            auth_config = [];
            auth_portal = None;
          };
        ]
  | None -> None

let api_of_draft (Draft (v, se)) =
  let questions =
    {
      se.questions with
      credential_authority =
        Some (Option.value se.metadata.cred_authority ~default:"");
      administrator = Some (Option.value se.administrator ~default:"");
    }
  in
  Lwt.return
    (Belenios_web_api.Draft
       ( v,
         {
           version = se.version;
           owners = se.owners;
           questions;
           languages = Option.value se.metadata.languages ~default:[];
           contact = se.metadata.contact;
           booth = Option.value se.metadata.booth_version ~default:1;
           authentication = get_authentication se;
           group = se.group;
           cred_authority_info = se.metadata.cred_authority_info;
         } ))

let assert_ msg b f = if b then f () else raise (Error msg)

let draft_of_api a uuid (Draft (v, se) as fse) (Belenios_web_api.Draft (v', d))
    =
  let version = se.version in
  let@ Equal =
   fun cont ->
    match Belenios.Election.compare_version v v' with
    | Some x -> cont x
    | None -> raise (Error (`CannotChange "version"))
  in
  let@ () =
    assert_ (`Invalid "booth version")
      (List.mem d.booth Defaults.supported_booth_versions)
  in
  let@ () = assert_ (`Invalid "languages") (List.length d.languages >= 1) in
  let@ () = assert_ (`Invalid "owners") (List.mem a.id d.owners) in
  let e_cred_authority = d.questions.credential_authority in
  let () =
    let old = se.metadata.cred_authority in
    if e_cred_authority <> old then
      if
        Web_persist.get_credentials_status uuid fse <> `None
        && (old = Some "server" || e_cred_authority = Some "server")
      then raise (Error (`CannotChange "credential authority"))
  in
  let e_cred_authority_info = d.cred_authority_info in
  let () =
    let old = se.metadata.cred_authority_info in
    if e_cred_authority_info <> old && se.public_creds_received then
      raise (Error (`CannotChange "credential authority info"))
  in
  let se_group = d.group in
  let () =
    let old = se.group in
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
      Belenios.Election.has_nh_questions (Template (v, d.questions))
    in
    if has_nh_questions then
      match se_group with
      | "RFC-3526-2048" | "Ed25519" -> ()
      | _ -> raise (Error (`Invalid "NH group"))
  in
  let se_metadata =
    {
      se.metadata with
      contact = d.contact;
      languages = Some d.languages;
      booth_version = Some d.booth;
      cred_authority = e_cred_authority;
      cred_authority_info = e_cred_authority_info;
      auth_config = auth_config_of_authentication d.authentication;
    }
  in
  {
    se with
    metadata = se_metadata;
    owners = d.owners;
    questions = d.questions;
    administrator = d.questions.administrator;
    group = se_group;
  }
  |> fun x -> Draft (v, x)

let post_drafts account draft =
  let@ () =
   fun cont -> if !Web_config.deny_newelection then Lwt.return_none else cont ()
  in
  let owners = [ account.id ] in
  let token = generate_token 22 in
  let se_metadata =
    {
      owners;
      auth_config = None;
      cred_authority = None;
      cred_authority_info = None;
      trustees = None;
      languages = None;
      contact = None;
      booth_version = None;
      billing_request = None;
      sealed = None;
      logo = None;
    }
  in
  let se_questions =
    {
      description = "";
      name = "";
      questions = [||];
      administrator = None;
      credential_authority = None;
      language = None;
    }
  in
  let se_version = Defaults.version in
  let (Version v) = Belenios.Election.version_of_int se_version in
  let se =
    {
      version = se_version;
      owners;
      group = !Web_config.default_group;
      voters = [];
      questions = se_questions;
      trustees = `Basic { trustees = [] };
      metadata = se_metadata;
      public_creds = token;
      public_creds_received = false;
      public_creds_certificate = None;
      creation_date = Unix.gettimeofday ();
      administrator = None;
      credential_authority_visited = false;
      voter_authentication_visited = false;
      trustees_setup_step = 1;
      pending_credentials = false;
      private_creds_downloaded = false;
    }
  in
  let* uuid = Storage.new_election () in
  let&* uuid = uuid in
  let se = draft_of_api account uuid (Draft (v, se)) draft in
  let* () =
    let@ s = Storage.E.with_transaction uuid in
    Web_persist.create_draft s se
  in
  Lwt.return_some uuid

let get_draft_voters (Draft (_, se)) =
  se.voters |> List.map (fun (x : draft_voter) -> x.id)

let put_draft_voters ((Draft (v, se), set) : _ updatable_with_billing) voters =
  let fail e = raise @@ Error (`VoterListError e) in
  let existing_voters =
    List.fold_left
      (fun accu (v : draft_voter) ->
        let login = Voter.get v.id in
        SMap.add (String.lowercase_ascii login) v accu)
      SMap.empty se.voters
  in
  let se_voters =
    List.map
      (fun voter ->
        if not (Voter.validate voter) then
          fail @@ `Identity (Voter.to_string voter);
        let login = Voter.get voter in
        match SMap.find_opt (String.lowercase_ascii login) existing_voters with
        | None -> { id = voter }
        | Some v ->
            v.id <- voter;
            v)
      voters
  in
  let* total_weight, _, _ =
    Lwt_list.fold_left_s
      (fun (total_weight, shape, voters) (v : draft_voter) ->
        let shape =
          let shape' =
            let (typ, { login; weight; _ }) : Voter.t = v.id in
            match typ with
            | `Plain -> `Plain (login <> None, weight <> None)
            | `Json -> `Json
          in
          match shape with
          | Some x when x <> shape' -> fail `FormatMix
          | _ -> Some shape'
        in
        let login = Voter.get v.id in
        let weight = Voter.get_weight v.id in
        let login = String.lowercase_ascii login in
        let* voters =
          if SSet.mem login voters then fail @@ `Duplicate login
          else Lwt.return (SSet.add login voters)
        in
        Lwt.return (Weight.(total_weight + weight), shape, voters))
      (Weight.zero, None, SSet.empty)
      se_voters
  in
  let* () =
    let expanded = Weight.expand ~total:total_weight total_weight in
    if Z.compare expanded Weight.max_expanded_weight > 0 then
      fail @@ `TotalWeightTooBig (expanded, Weight.max_expanded_weight)
    else Lwt.return_unit
  in
  let se = { se with voters = se_voters } in
  set (Draft (v, se))

let get_credentials_token (Draft (_, se)) =
  if se.metadata.cred_authority = Some "server" then Lwt.return_none
  else Lwt.return_some @@ `String se.public_creds

type generate_credentials_on_server_error =
  [ `NoVoters | `TooManyVoters | `Already | `NoServer ]

let generate_credentials_on_server account uuid (Draft (_, se) as draft) =
  let nvoters = List.length se.voters in
  if nvoters > Accounts.max_voters account then
    Lwt.return (Stdlib.Error `TooManyVoters)
  else if nvoters = 0 then Lwt.return (Stdlib.Error `NoVoters)
  else if Web_persist.get_credentials_status uuid draft <> `None then
    Lwt.return (Stdlib.Error `Already)
  else if se.metadata.cred_authority <> Some "server" then
    Lwt.return (Stdlib.Error `NoServer)
  else
    let () = Web_persist.generate_credentials_on_server_async uuid draft in
    Lwt.return (Ok ())

let exn_of_generate_credentials_on_server_error = function
  | `NoVoters -> Error (`ValidationError `NoVoters)
  | `TooManyVoters -> Error (`ValidationError `TooManyVoters)
  | `Already -> Error (`GenericError "already done")
  | `NoServer -> Error (`GenericError "credential authority is not the server")

let submit_public_credentials s
    ((Draft (v, se), set) : _ updatable_with_billing) ?certificate credentials =
  let () = if se.voters = [] then raise (Error (`ValidationError `NoVoters)) in
  let () =
    if not (List.length se.voters = List.length credentials) then
      raise (Error (`ValidationError `WrongLength))
  in
  let version = se.version in
  let module G = (val Group.of_string ~version se.group : GROUP) in
  let () =
    match certificate with
    | None -> ()
    | Some certificate ->
        let public_creds_ok =
          let public_creds_hash =
            List.map strip_public_credential credentials
            |> yojson_of_public_credentials |> Hash.hash_yojson
          in
          public_creds_hash = certificate.message.public_creds_hash
        in
        let certificate_ok =
          match
            certificate
            |> !+(yojson_of_credentials_certificate Fun.id Fun.id)
            |> !*(credentials_certificate_of_yojson !$G.of_string
                    !$G.Zq.of_string)
          with
          | x ->
              let module C = Credentials_certificate (G) in
              C.check x
          | exception _ -> false
        in
        if not (public_creds_ok && certificate_ok) then
          raise (Error (`GenericError "bad certificate"))
  in
  let usernames =
    List.fold_left
      (fun accu ({ id; _ } : draft_voter) ->
        let username = Voter.get id in
        let weight = Voter.get_weight id in
        if SMap.mem username accu then
          raise
            (Error
               (`GenericError (Printf.sprintf "duplicate username %s" username)))
        else SMap.add username (weight, ref false) accu)
      SMap.empty se.voters
  in
  let _, _ =
    List.fold_left
      (fun (i, creds) x ->
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
              else if not (G.check p.credential) then
                invalid "public credential"
              else used := true
        in
        (i + 1, SSet.add cred_s creds))
      (0, SSet.empty) credentials
  in
  let* () = Storage.E.set s Public_creds Value credentials in
  se.public_creds_received <- true;
  se.public_creds_certificate <- certificate;
  set (Draft (v, se))

let get_draft_trustees ~is_admin (Draft (_, se)) =
  match se.trustees with
  | `Basic x ->
      let trustees =
        List.filter_map
          (fun (t : _ draft_trustee) ->
            if t.id = "server" then None
            else
              let trustee_state, trustee_key =
                if t.public_key = "" then (Some 0, None)
                else
                  ( Some 1,
                    Some
                      (!*(trustee_public_key_of_yojson Fun.id Fun.id)
                         t.public_key) )
              in
              let trustee_address, trustee_token, trustee_state =
                if is_admin then (Some t.id, Some t.token, trustee_state)
                else (None, None, None)
              in
              Some
                {
                  address = trustee_address;
                  name = t.name;
                  token = trustee_token;
                  state = trustee_state;
                  key = trustee_key;
                })
          x.trustees
      in
      `Basic ({ trustees } : _ basic_trustees)
  | `Threshold x ->
      let trustees =
        List.map
          (fun (t : _ draft_threshold_trustee) ->
            let trustee_address, trustee_token, trustee_state =
              if is_admin then
                (Some t.id, Some t.token, Some (Option.value t.step ~default:0))
              else (None, None, None)
            in
            {
              address = trustee_address;
              name = t.name;
              token = trustee_token;
              state = trustee_state;
              key = t.cert;
            })
          x.trustees
      in
      `Threshold ({ threshold = x.threshold; trustees } : _ threshold_trustees)

let check_address address =
  if not @@ is_email address then raise (Error (`Invalid "e-mail address"))

let ensure_none label x =
  if x <> None then
    raise (Error (`GenericError (Printf.sprintf "%s must not be set" label)))

let generate_server_trustee (Draft (_, se)) =
  let st_id = "server" and st_token = "" in
  let version = se.version in
  let module G = (val Group.of_string ~version se.group) in
  let module Trustees = (val Trustees.get_by_version version) in
  let module K = Trustees.MakeSimple (G) in
  let private_key = K.generate () in
  let name = "server" in
  let public_key = K.prove ~name private_key in
  let st_public_key =
    !+(yojson_of_trustee_public_key !&G.to_string !&G.Zq.to_string) public_key
  in
  let st_private_key = Some (`String (private_key |> G.Zq.to_string)) in
  Lwt.return
    {
      id = st_id;
      token = st_token;
      public_key = st_public_key;
      private_key = st_private_key;
      name;
    }

let post_draft_trustees ((Draft (v, se), set) : _ updatable_with_billing)
    (t : _ trustee) =
  let address =
    match t.address with
    | Some x ->
        check_address x;
        x
    | None -> raise (Error (`Missing "address"))
  in
  let () = ensure_none "token" t.token in
  let () = ensure_none "state" t.state in
  let () = ensure_none "key" t.key in
  match se.trustees with
  | `Basic x ->
      let* ts =
        let ts = x.trustees in
        if List.exists (fun (x : _ draft_trustee) -> x.id = "server") ts then
          Lwt.return ts
        else
          let* server = generate_server_trustee (Draft (v, se)) in
          Lwt.return (ts @ [ server ])
      in
      let () =
        if List.exists (fun (x : _ draft_trustee) -> x.id = address) ts then
          raise (Error (`GenericError "address already used"))
      in
      let st_token = generate_token 22 in
      let t =
        {
          id = address;
          name = t.name;
          public_key = "";
          private_key = None;
          token = st_token;
        }
      in
      x.trustees <- ts @ [ t ];
      set (Draft (v, se))
  | `Threshold x ->
      let ts = x.trustees in
      let () =
        if
          List.exists (fun (x : _ draft_threshold_trustee) -> x.id = address) ts
        then raise (Error (`GenericError "address already used"))
      in
      let stt_token = generate_token 22 in
      let t =
        {
          id = address;
          name = t.name;
          token = stt_token;
          step = None;
          cert = None;
          polynomial = None;
          vinput = None;
          voutput = None;
        }
      in
      x.trustees <- ts @ [ t ];
      set (Draft (v, se))

let rec filter_out_first f = function
  | [] -> (false, [])
  | x :: xs ->
      if f x then (true, xs)
      else
        let touched, xs = filter_out_first f xs in
        (touched, x :: xs)

let delete_draft_trustee ((Draft (v, se), set) : _ updatable_with_billing)
    trustee =
  match se.trustees with
  | `Basic x ->
      let ts = x.trustees in
      let touched, ts =
        filter_out_first (fun (x : _ draft_trustee) -> x.id = trustee) ts
      in
      if touched then (
        x.trustees <- ts;
        let* () = set (Draft (v, se)) in
        Lwt.return_true)
      else Lwt.return_false
  | `Threshold x ->
      let ts = x.trustees in
      let touched, ts =
        filter_out_first
          (fun (x : _ draft_threshold_trustee) -> x.id = trustee)
          ts
      in
      if touched then (
        x.trustees <- ts;
        let* () = set (Draft (v, se)) in
        Lwt.return_true)
      else Lwt.return_false

let set_threshold ((Draft (v, se), set) : _ updatable_with_billing) threshold =
  match se.trustees with
  | `Basic _ -> Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x when x.trustees = [] -> Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x ->
      let ts = x.trustees in
      let maybe_threshold, step =
        if threshold = 0 then (None, None) else (Some threshold, Some 1)
      in
      if 0 <= threshold && threshold < List.length ts then (
        List.iter (fun t -> t.step <- step) ts;
        x.threshold <- maybe_threshold;
        let* () = set (Draft (v, se)) in
        Lwt.return @@ Ok ())
      else Lwt.return @@ Stdlib.Error `OutOfBounds

let get_draft_trustees_mode (Draft (_, se)) =
  match se.trustees with
  | `Basic _ -> `Basic
  | `Threshold x -> `Threshold (Option.value x.threshold ~default:0)

let put_draft_trustees_mode ((Draft (v, se), set) : _ updatable_with_billing)
    mode =
  match (get_draft_trustees_mode (Draft (v, se)), mode) with
  | a, b when a = b -> Lwt.return_unit
  | _, `Basic ->
      se.trustees <- `Basic { trustees = [] };
      set (Draft (v, se))
  | `Basic, `Threshold 0 ->
      let dtp =
        {
          algorithm = default_algorithm;
          threshold = None;
          trustees = [];
          parameters = None;
          error = None;
        }
      in
      se.trustees <- `Threshold dtp;
      set (Draft (v, se))
  | `Threshold _, `Threshold threshold -> (
      let* x = set_threshold (Draft (v, se), set) threshold in
      match x with
      | Ok () -> Lwt.return_unit
      | Error `NoTrustees -> Lwt.fail (Error (`GenericError "no trustees"))
      | Error `OutOfBounds ->
          Lwt.fail (Error (`GenericError "threshold out of bounds")))
  | _, _ -> Lwt.fail (Error (`GenericError "change not allowed"))

let reset_draft_trustees ((Draft (v, se), set) : _ updatable_with_billing) =
  se.trustees <- `Basic { trustees = [] };
  se.trustees_setup_step <- 1;
  set (Draft (v, se))

let get_draft_status uuid (Draft (v, se)) =
  let* private_credentials_downloaded =
    if se.metadata.cred_authority = Some "server" then
      Lwt.return_some se.private_creds_downloaded
    else Lwt.return_none
  in
  let credentials_ready, credentials_left =
    match Web_persist.get_credentials_status uuid (Draft (v, se)) with
    | `None -> (false, None)
    | `Pending n -> (false, Some n)
    | `Done -> (true, None)
  in
  let has_weights = has_explicit_weights se.voters in
  let restricted_mode_error =
    if !Web_config.restricted_mode then
      if se.metadata.cred_authority = Some "server" then Some `AutoCredentials
      else if
        match se.metadata.auth_config with
        | Some [ { auth_system = "import"; _ } ] -> false
        | _ -> true
      then Some `VoterAuthentication
      else if Belenios.Election.has_nh_questions (Template (v, se.questions))
      then Some `ForbiddenQuestions
      else if has_weights then Some `HasWeights
      else if se.group <> "Ed25519" then Some `BadGroup
      else
        match se.trustees with
        | `Basic _ -> Some `NoThreshold
        | `Threshold { trustees = _ :: _ :: _; _ } -> None
        | _ -> Some `NotEnoughTrustees
    else None
  in
  Lwt.return
    {
      num_voters = List.length se.voters;
      credentials_ready;
      credentials_left;
      private_credentials_downloaded;
      trustees_ready =
        (match se.trustees with
        | `Basic x ->
            List.for_all
              (fun (t : _ draft_trustee) -> t.public_key <> "")
              x.trustees
        | `Threshold x ->
            List.for_all
              (fun (t : _ draft_threshold_trustee) -> t.step = Some 7)
              x.trustees);
      nh_and_weights_compatible =
        (let has_nh =
           Belenios.Election.has_nh_questions (Template (v, se.questions))
         in
         not (has_weights && has_nh));
      credential_authority_visited = se.credential_authority_visited;
      voter_authentication_visited = se.voter_authentication_visited;
      trustees_setup_step = se.trustees_setup_step;
      restricted_mode_error;
    }

let merge_voters a b =
  let weights =
    List.fold_left
      (fun accu (sv : draft_voter) ->
        let login = Voter.get sv.id |> String.lowercase_ascii in
        let weight = Voter.get_weight sv.id in
        SMap.add login weight accu)
      SMap.empty a
  in
  let rec loop weights accu = function
    | [] ->
        Ok (List.rev accu, Weight.(SMap.fold (fun _ x y -> x + y) weights zero))
    | id :: xs ->
        let login = Voter.get id |> String.lowercase_ascii in
        let weight = Voter.get_weight id in
        if SMap.mem login weights then Stdlib.Error id
        else
          loop
            (SMap.add login weight weights)
            (({ id } : draft_voter) :: accu)
            xs
  in
  loop weights (List.rev a) b

let import_voters uuid ((Draft (v, se), set) : _ updatable_with_billing) from =
  let@ voters cont =
    let* x = Web_persist.get_all_voters from in
    match x with
    | [] -> (
        let* se = Storage.E.get from Draft in
        match Lopt.get_value se with
        | None -> Lwt.return @@ Stdlib.Error `NotFound
        | Some se -> cont @@ get_draft_voters se)
    | _ -> cont x
  in
  if Web_persist.get_credentials_status uuid (Draft (v, se)) <> `None then
    Lwt.return @@ Stdlib.Error `Forbidden
  else
    match merge_voters se.voters voters with
    | Ok (voters, total_weight) ->
        let expanded = Weight.expand ~total:total_weight total_weight in
        if Z.compare expanded Weight.max_expanded_weight <= 0 then (
          se.voters <- voters;
          let* () = set (Draft (v, se)) in
          Lwt.return @@ Ok ())
        else Lwt.return @@ Stdlib.Error (`TotalWeightTooBig total_weight)
    | Error x ->
        let login = Voter.get x in
        Lwt.return @@ Stdlib.Error (`Duplicate login)

let import_trustees ((Draft (v, se), set) : _ updatable_with_billing) from
    (metadata : metadata) =
  match metadata.trustees with
  | None -> Lwt.return @@ Stdlib.Error `None
  | Some names -> (
      let* trustees = Public_archive.get_trustees from in
      let version = se.version in
      let module G = (val Group.of_string ~version se.group : GROUP) in
      let module Trustees = (val Trustees.get_by_version version) in
      let module K = Trustees.MakeCombinator (G) in
      let trustees =
        !*(trustees_of_yojson !$G.of_string !$G.Zq.of_string) trustees
      in
      if not (K.check trustees) then Lwt.return @@ Stdlib.Error `Invalid
      else
        let import_pedersen (t : _ threshold_parameters) names =
          let* privs = Storage.E.get from Private_keys in
          let* x =
            match Lopt.get_value privs with
            | Some privs ->
                let rec loop ts certs pubs privs accu =
                  match (ts, certs, pubs, privs) with
                  | ( stt_id :: ts,
                      cert :: certs,
                      (vo_public_key : _ threshold_verification_key) :: pubs,
                      vo_private_key :: privs ) ->
                      let stt_name = vo_public_key.message.message.name in
                      let stt_token = generate_token 22 in
                      let vo_private_key =
                        vo_private_key
                        |> !+(yojson_of_sent_partial_decryption_key Fun.id
                                Fun.id)
                        |> !*(sent_partial_decryption_key_of_yojson
                                !$G.of_string !$G.Zq.of_string)
                      in
                      let stt_voutput =
                        {
                          public_key = vo_public_key;
                          private_key = vo_private_key;
                        }
                      in
                      let stt_voutput =
                        Some
                          (!+(yojson_of_voutput !&G.to_string !&G.Zq.to_string)
                             stt_voutput)
                      in
                      let stt =
                        {
                          id = stt_id;
                          token = stt_token;
                          voutput = stt_voutput;
                          step = Some 7;
                          cert = Some cert;
                          polynomial = None;
                          vinput = None;
                          name = stt_name;
                        }
                      in
                      loop ts certs pubs privs (stt :: accu)
                  | [], [], [], [] -> Lwt.return @@ Ok (List.rev accu)
                  | _ -> Lwt.return @@ Stdlib.Error `Inconsistent
                in
                loop names (Array.to_list t.certs)
                  (Array.to_list t.verification_keys)
                  privs []
            | None -> Lwt.return @@ Stdlib.Error `MissingPrivateKeys
          in
          match x with
          | Ok se_threshold_trustees ->
              let se_threshold_trustees =
                se_threshold_trustees
                |> List.map
                     (!+(yojson_of_draft_threshold_trustee !&G.to_string
                           !&G.Zq.to_string)
                     >> !*(draft_threshold_trustee_of_yojson Fun.id Fun.id))
              in
              let dtp =
                {
                  algorithm = t.context.algorithm;
                  threshold = Some t.context.threshold;
                  trustees = se_threshold_trustees;
                  parameters =
                    Some
                      (!+(yojson_of_threshold_parameters !&G.to_string
                            !&G.Zq.to_string)
                         t);
                  error = None;
                }
              in
              se.trustees <- `Threshold dtp;
              let* () = set (Draft (v, se)) in
              Lwt.return @@ Ok `Threshold
          | Stdlib.Error _ as x -> Lwt.return x
        in
        match trustees with
        | [ `Pedersen t ] -> import_pedersen t names
        | [ `Single x; `Pedersen t ] when x.message.name = "server" ->
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
              let module KG = Trustees.MakeSimple (G) in
              List.combine names ts
              |> Lwt_list.map_p (fun (st_id, public_key) ->
                  let* st_token, st_private_key, st_public_key =
                    if st_id = "server" then
                      let private_key = KG.generate () in
                      let public_key = KG.prove ~name:"server" private_key in
                      let public_key =
                        !+(yojson_of_trustee_public_key !&G.to_string
                             !&G.Zq.to_string)
                          public_key
                      in
                      Lwt.return
                        ( "",
                          Some (`String (G.Zq.to_string private_key)),
                          public_key )
                    else
                      let st_token = generate_token 22 in
                      let public_key =
                        !+(yojson_of_trustee_public_key !&G.to_string
                             !&G.Zq.to_string)
                          public_key
                      in
                      Lwt.return (st_token, None, public_key)
                  in
                  let st_name = public_key.message.name in
                  Lwt.return
                    {
                      id = st_id;
                      token = st_token;
                      public_key = st_public_key;
                      private_key = st_private_key;
                      name = st_name;
                    })
            in
            se.trustees <- `Basic { trustees = ts };
            let* () = set (Draft (v, se)) in
            Lwt.return @@ Ok `Basic)

let check_owner account s cont =
  let* metadata = Web_persist.get_election_metadata s in
  if Accounts.check account metadata.owners then cont metadata else unauthorized

let initiate_credential_authority_protocol ~uuid ~info ~admin_id ~token () =
  let body =
    `NewRequest
      { belenios_url = !Web_config.prefix ^ "/"; uuid; info; token; admin_id }
    |> !+Belenios_web_api.yojson_of_credentials_request
    |> Cohttp_lwt.Body.of_string
  in
  let prefix =
    Printf.sprintf "initiate_credential_authority_protocol[%s]%s"
      (Uuid.to_string uuid)
      (!+yojson_of_cred_authority_info info)
  in
  Lwt.catch
    (fun () ->
      let@ _check_cred_server cont =
        if !Web_config.credentials_client_allowed info.server then cont ()
        else failwith "cred_server not allowed"
      in
      let* x, body =
        Cohttp_lwt_unix.Client.post ~body (Uri.of_string info.server)
      in
      let* () = Cohttp_lwt.Body.drain_body body in
      let code = Cohttp.Code.code_of_status x.status in
      let msg = Printf.sprintf "%s: %d" prefix code in
      Ocsigen_messages.warning msg;
      Lwt.return_unit)
    (fun e ->
      let msg = Printf.sprintf "%s: %s" prefix (Printexc.to_string e) in
      Ocsigen_messages.errlog msg;
      Lwt.return_unit)

let post_draft_status ~admin_id s uuid
    ((Draft (v, se), set) : _ updatable_with_billing) = function
  | `SetDownloaded ->
      let* () =
        if se.private_creds_downloaded then Lwt.return_unit
        else (
          se.private_creds_downloaded <- true;
          set (Draft (v, se)))
      in
      ok
  | `ValidateElection ->
      let* status = get_draft_status uuid (Draft (v, se)) in
      let* () =
        Web_persist.validate_election ~admin_id s (Draft (v, se), set) status
      in
      ok
  | `SetCredentialAuthorityVisited ->
      let* () =
        if se.credential_authority_visited <> true then (
          se.credential_authority_visited <- true;
          let* () = set (Draft (v, se)) in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `SetVoterAuthenticationVisited ->
      let* () =
        if se.voter_authentication_visited <> true then (
          se.voter_authentication_visited <- true;
          let* () = set (Draft (v, se)) in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `SetTrusteesSetupStep i ->
      let* () =
        if se.trustees_setup_step <> i then (
          se.trustees_setup_step <- i;
          let* () = set (Draft (v, se)) in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `InitiateCredentialAuthorityProtocol -> (
      match se.metadata.cred_authority_info with
      | Some info when not se.public_creds_received ->
          Lwt.async
            (initiate_credential_authority_protocol ~uuid ~info ~admin_id
               ~token:se.public_creds);
          ok
      | _ -> forbidden)

let () =
  Billing.validate :=
    fun ~admin_id uuid ->
      let@ s = Storage.E.with_transaction uuid in
      let@ se, set = Storage.E.update s Draft in
      match Lopt.get_value se with
      | None -> not_found
      | Some se ->
          let set ?billing:_ x = set Value x in
          post_draft_status ~admin_id s uuid (se, set) `ValidateElection

let post_trustee_basic
    (((Draft (_, se) as fse), set) : _ updatable_with_billing) ~token data =
  let ts =
    match se.trustees with
    | `Basic x -> x.trustees
    | `Threshold _ -> failwith "Wrong trustee mode"
  in
  let t =
    match List.find_opt (fun (x : _ draft_trustee) -> token = x.token) ts with
    | Some t -> t
    | None -> failwith "Invalid token"
  in
  match t.public_key with
  | "" ->
      let version = se.version in
      let module G = (val Group.of_string ~version se.group : GROUP) in
      let module Trustees = (val Trustees.get_by_version version) in
      let pk =
        !*(trustee_public_key_of_yojson !$G.of_string !$G.Zq.of_string) data
      in
      let module K = Trustees.MakeCombinator (G) in
      if pk.message.name = t.name && K.check [ `Single pk ] then (
        t.public_key <-
          !+(yojson_of_trustee_public_key !&G.to_string !&G.Zq.to_string) pk;
        set fse)
      else raise @@ Error `InvalidPublicKey
  | _ -> raise @@ Error `PublicKeyExists

let post_trustee_threshold
    (((Draft (_, se) as fse), set) : _ updatable_with_billing) ~token data =
  let version = se.version in
  let module G = (val Group.of_string ~version se.group : GROUP) in
  let se_trustees =
    se.trustees
    |> !+(yojson_of_draft_trustees Fun.id Fun.id)
    |> !*(draft_trustees_of_yojson !$G.of_string !$G.Zq.of_string)
  in
  let dtp =
    match se_trustees with
    | `Basic _ -> failwith "Wrong trustee mode"
    | `Threshold x -> x
  in
  let ts = Array.of_list dtp.trustees in
  let threshold =
    match dtp.threshold with Some t -> t | None -> failwith "No threshold set"
  in
  let i, t =
    match
      Array.findi
        (fun i (x : _ draft_threshold_trustee) ->
          if token = x.token then Some (i, x) else None)
        ts
    with
    | Some (i, t) -> (i, t)
    | None -> failwith "Trustee not found"
  in
  let names = Array.map (fun (x : _ draft_threshold_trustee) -> x.name) ts in
  let context =
    { algorithm = dtp.algorithm; group = se.group; names; threshold }
  in
  let full_context = { context; index = i + 1 } in
  let get_certs () =
    Array.map
      (fun (x : _ draft_threshold_trustee) ->
        match x.cert with None -> failwith "Missing certificate" | Some y -> y)
      ts
  in
  let get_polynomials () =
    Array.map
      (fun (x : _ draft_threshold_trustee) ->
        match x.polynomial with
        | None -> failwith "Missing polynomial"
        | Some y -> y)
      ts
  in
  let module Trustees = (val Trustees.get_by_version version) in
  let module P = Pki.Make (G) in
  let module C = Pki.MakeChannels (P) in
  let module K = Trustees.MakePedersen (C) in
  let () =
    match t.step with
    | Some 1 ->
        let cert = !*(cert_of_yojson !$G.of_string !$G.Zq.of_string) data in
        if K.step1_check full_context cert then (
          t.cert <- Some cert;
          t.step <- Some 2)
        else failwith "Invalid certificate"
    | Some 3 ->
        let certs = get_certs () in
        let polynomial =
          !*(polynomial_of_yojson !$G.of_string !$G.Zq.of_string) data
        in
        if K.step3_check { context; certs } i polynomial then (
          t.polynomial <- Some polynomial;
          t.step <- Some 4)
        else failwith "Invalid polynomial"
    | Some 5 ->
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutput =
          !*(voutput_of_yojson !$G.of_string !$G.Zq.of_string) data
        in
        if K.step5_check { context; certs } i polynomials voutput then (
          t.voutput <-
            Some (!+(yojson_of_voutput !&G.to_string !&G.Zq.to_string) voutput);
          t.step <- Some 6)
        else failwith "Invalid voutput"
    | _ -> failwith "Invalid step"
  in
  let () =
    if Array.for_all (fun (x : _ draft_threshold_trustee) -> x.step = Some 2) ts
    then
      try
        let certs = { context; certs = get_certs () } in
        let threshold = K.step2 certs in
        assert (dtp.threshold = Some threshold);
        Array.iter (fun x -> x.step <- Some 3) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun x -> x.step = Some 4) ts then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let vinputs = K.step4 { context; certs } polynomials in
        for j = 0 to Array.length ts - 1 do
          ts.(j).vinput <- Some vinputs.(j)
        done;
        Array.iter (fun x -> x.step <- Some 5) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun x -> x.step = Some 6) ts then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutputs =
          Array.map
            (fun x ->
              match x.voutput with
              | None -> failwith "Missing voutput"
              | Some y -> !*(voutput_of_yojson !$G.of_string !$G.Zq.of_string) y)
            ts
        in
        let p = K.step6 { context; certs } polynomials voutputs in
        dtp.parameters <-
          Some
            (!+(yojson_of_threshold_parameters !&G.to_string !&G.Zq.to_string)
               p);
        Array.iter (fun x -> x.step <- Some 7) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  se.trustees <-
    se_trustees
    |> !+(yojson_of_draft_trustees !&G.to_string !&G.Zq.to_string)
    |> !*(draft_trustees_of_yojson Fun.id Fun.id);
  set fse

let dispatch_credentials ~token endpoint method_ body s uuid
    ((se, set) : _ updatable_with_billing) =
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
          let@ () = handle_get_option in
          let* x = Storage.E.get s Private_creds in
          x |> Lopt.get_string |> Option.map Json.of_string |> Lwt.return
      | _ -> method_not_allowed)
  | [ "public" ] -> (
      match method_ with
      | `GET ->
          handle_get_option (fun () ->
              Web_persist.get_draft_public_credentials s)
      | `POST -> (
          let@ who = with_administrator_or_credential_authority token se in
          if Web_persist.get_credentials_status uuid se <> `None then forbidden
          else
            let@ x = body.run !*public_credentials_of_yojson in
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
                let* () = submit_public_credentials s (se, set) credentials in
                ok
            | _ -> forbidden)
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch_draft ~token ~ifmatch endpoint method_ body s uuid
    ((se, set) : _ updatable_with_billing) =
  match endpoint with
  | [] -> (
      let@ who = with_administrator_or_nobody token se in
      let get () =
        let* x = api_of_draft se in
        Lwt.return @@ yojson_of_draft x
      in
      match (method_, who) with
      | `GET, _ -> handle_get get
      | `PUT, `Administrator account ->
          let@ () = handle_ifmatch ifmatch get in
          let@ draft = body.run !*draft_of_yojson in
          let@ () = handle_generic_error in
          let se = draft_of_api account uuid se draft in
          let* () = set se in
          ok
      | `POST, `Administrator a ->
          let@ () = handle_ifmatch ifmatch get in
          let@ x = body.run !*draft_request_of_yojson in
          let@ () = handle_generic_error in
          post_draft_status ~admin_id:a.id s uuid (se, set) x
      | _ -> method_not_allowed)
  | [ "voters" ] -> (
      let@ who = with_administrator_or_credential_authority token se in
      let get () =
        let x = get_draft_voters se in
        Lwt.return @@ yojson_of_voter_list x
      in
      match (method_, who) with
      | `GET, _ -> handle_get get
      | `PUT, `Administrator _ ->
          let@ () = handle_ifmatch ifmatch get in
          if Web_persist.get_credentials_status uuid se <> `None then forbidden
          else
            let@ voters = body.run !*voter_list_of_yojson in
            let@ () = handle_generic_error in
            let* () = put_draft_voters (se, set) voters in
            ok
      | `POST, `Administrator account -> (
          let@ () = handle_ifmatch ifmatch get in
          let@ request = body.run !*voters_request_of_yojson in
          let@ () = handle_generic_error in
          match request with
          | `Import from -> (
              let@ from = Storage.E.with_transaction from in
              let@ _ = check_owner account from in
              let* x = import_voters uuid (se, set) from in
              match x with
              | Ok () -> ok
              | Stdlib.Error `Forbidden -> forbidden
              | Stdlib.Error `NotFound -> not_found
              | Stdlib.Error (`TotalWeightTooBig _) ->
                  Lwt.fail (Error (`GenericError "total weight too big"))
              | Stdlib.Error (`Duplicate x) ->
                  Lwt.fail (Error (`GenericError ("duplicate: " ^ x)))))
      | _ -> method_not_allowed)
  | "credentials" :: endpoint ->
      dispatch_credentials ~token endpoint method_ body s uuid (se, set)
  | [ "trustee" ] -> (
      let@ trustee = with_trustee token se in
      let get () =
        let@ () =
         fun cont ->
          Lwt.return @@ yojson_of_trustee_status Fun.id Fun.id @@ cont ()
        in
        match trustee with
        | `Basic b -> `Basic (if b.public_key = "" then `Init b.name else `Done)
        | `Threshold (index, t, dtp) -> (
            let@ () = fun cont -> `Threshold (cont ()) in
            match dtp.threshold with
            | None -> `Init
            | Some threshold -> (
                let (Draft (_, draft)) = se in
                let names =
                  List.map
                    (fun (x : _ draft_threshold_trustee) -> x.name)
                    dtp.trustees
                  |> Array.of_list
                in
                let context =
                  {
                    algorithm = dtp.algorithm;
                    group = draft.group;
                    names;
                    threshold;
                  }
                in
                let pedersen_context = { context; index } in
                match t.cert with
                | None -> `WaitingForCertificate pedersen_context
                | Some _ -> (
                    try
                      let pedersen_certs =
                        List.map
                          (fun x ->
                            match x.cert with None -> raise Exit | Some c -> c)
                          dtp.trustees
                        |> Array.of_list
                      in
                      `Pedersen
                        {
                          context = pedersen_context;
                          step = Option.value ~default:0 t.step;
                          certs = pedersen_certs;
                          vinput = t.vinput;
                          voutput =
                            Option.map
                              !*(voutput_of_yojson Fun.id Fun.id)
                              t.voutput;
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
              let* () = post_trustee_basic (se, set) ~token:b.token data in
              ok
          | `Threshold (_, t, _) ->
              let* () = post_trustee_threshold (se, set) ~token:t.token data in
              ok)
      | _ -> method_not_allowed)
  | [ "trustees" ] -> (
      let@ who = with_administrator_or_nobody token se in
      let get is_admin () =
        let open Belenios_web_api in
        let x = get_draft_trustees ~is_admin se in
        Lwt.return @@ yojson_of_draft_trustees Fun.id Fun.id x
      in
      match (method_, who) with
      | `GET, `Nobody -> handle_get (get false)
      | `GET, `Administrator _ -> handle_get (get true)
      | `POST, `Administrator account -> (
          let@ () = handle_ifmatch ifmatch (get true) in
          let@ request = body.run !*trustees_request_of_yojson in
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
          | `Reset ->
              let* () = reset_draft_trustees (se, set) in
              ok
          | `Import from -> (
              let@ from = Storage.E.with_transaction from in
              let@ metadata = check_owner account from in
              let* x = import_trustees (se, set) from metadata in
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
          let* x = get_draft_status uuid se in
          return_json 200 (!+yojson_of_draft_status x)
      | _ -> method_not_allowed)
  | _ -> not_found
