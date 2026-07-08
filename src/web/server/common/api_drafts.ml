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

let with_administrator token metadata f =
  match get_account_user token with
  | Some (a, _) when Accounts.check a metadata.owners -> f a
  | _ -> unauthorized

let with_administrator_or_credential_authority (token : token_user)
    (Draft (_, se)) metadata f =
  let@ token = Option.unwrap unauthorized token.token in
  if token = se.public_creds then f `CredentialAuthority
  else
    match lookup_token token with
    | Some (a, _) when Accounts.check a metadata.owners -> f (`Administrator a)
    | _ -> not_found

let with_administrator_or_nobody (token : token_user) metadata f =
  match token.token with
  | None -> f `Nobody
  | Some token -> (
      match lookup_token token with
      | Some (a, _) when Accounts.check a metadata.owners ->
          f (`Administrator a)
      | _ -> not_found)

let with_trustee (token : token_user) (Draft (_, se)) f =
  let@ token = Option.unwrap unauthorized token.token in
  match se.trustees.mode with
  | `Basic b -> (
      match
        List.find_opt
          (fun (x : _ draft_basic_trustee) -> x.token = token)
          b.trustees
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

let get_authentication metadata =
  authentication_of_auth_config metadata.auth_config

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

let api_of_draft (Draft (v, se)) metadata =
  Lwt.return
    (Belenios_web_api.Draft
       ( v,
         {
           version = se.version;
           owners = metadata.owners;
           questions = se.questions;
           languages = Option.value metadata.languages ~default:[];
           contact = metadata.contact;
           booth = Option.value metadata.booth_version ~default:1;
           authentication = get_authentication metadata;
           group = se.group;
           cred_authority_info = metadata.cred_authority_info;
         } ))

let assert_ msg b f = if b then f () else raise (Error msg)

let draft_of_api a uuid (Draft (v, se) as fse) metadata
    (Belenios_web_api.Draft (v', d)) =
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
    let old = se.questions.credential_authority in
    if e_cred_authority <> old then
      if
        Web_persist.get_credentials_status uuid fse <> `None
        && (old = `Server || e_cred_authority = `Server)
      then raise (Error (`CannotChange "credential authority"))
  in
  let e_cred_authority_info = d.cred_authority_info in
  let () =
    let old = metadata.cred_authority_info in
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
          let module G = (val Group.make { version; group = se_group }) in
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
  let metadata =
    {
      metadata with
      owners = d.owners;
      contact = d.contact;
      languages = Some d.languages;
      booth_version = Some d.booth;
      cred_authority_info = e_cred_authority_info;
      auth_config = auth_config_of_authentication d.authentication;
    }
  in
  { se with questions = d.questions; group = se_group } |> fun x ->
  (Draft (v, x), metadata)

let post_drafts account draft =
  let@ () =
   fun cont -> if !Web_config.deny_newelection then Lwt.return_none else cont ()
  in
  let owners = [ account.id ] in
  let token = generate_token 22 in
  let metadata =
    {
      owners;
      auth_config = None;
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
      administrator = "";
      credential_authority = `Server;
      language = None;
    }
  in
  let version = Defaults.version in
  let group = !Web_config.default_group in
  let module G = (val Group.make { version; group }) in
  let (Version v) = Belenios.Election.version_of_int version in
  let se : _ raw_draft_election =
    {
      version;
      group;
      voters = [];
      questions = se_questions;
      trustees = { step = 1; mode = `Basic { trustees = [] } };
      public_creds = token;
      public_creds_received = false;
      public_creds_certificate = None;
      credential_authority_visited = false;
      voter_authentication_visited = false;
      pending_credentials = false;
      private_creds_downloaded = false;
    }
  in
  let dates =
    {
      Belenios_storage_api.default_election_dates with
      creation = datetime_now ();
    }
  in
  let* uuid = Storage.E.new_election () in
  let se, metadata = draft_of_api account uuid (Draft (v, se)) metadata draft in
  let* () =
    let@ s = Storage.E.with_transaction uuid in
    let* () = Storage.E.set s Dates Value dates in
    let* () = Storage.E.set s Metadata Value metadata in
    Web_persist.create_draft s (W ((module G), se))
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
  if se.questions.credential_authority = `Server then Lwt.return_none
  else Lwt.return_some @@ se.public_creds

type generate_credentials_on_server_error =
  [ `NoVoters | `TooManyVoters | `Already | `NoServer ]

let generate_credentials_on_server account uuid (Draft (_, se) as draft) =
  let nvoters = List.length se.voters in
  if nvoters > Accounts.max_voters account then
    Lwt.return (Stdlib.Error `TooManyVoters)
  else if nvoters = 0 then Lwt.return (Stdlib.Error `NoVoters)
  else if Web_persist.get_credentials_status uuid draft <> `None then
    Lwt.return (Stdlib.Error `Already)
  else if se.questions.credential_authority <> `Server then
    Lwt.return (Stdlib.Error `NoServer)
  else
    let () = Web_persist.generate_credentials_on_server_async uuid draft in
    Lwt.return (Ok ())

let exn_of_generate_credentials_on_server_error = function
  | `NoVoters -> Error (`ValidationError `NoVoters)
  | `TooManyVoters -> Error (`ValidationError `TooManyVoters)
  | `Already -> Error (`GenericError "already done")
  | `NoServer -> Error (`GenericError "credential authority is not the server")

let submit_public_credentials s (type a b) (w : (a, b) group)
    ((Draft (v, se), set) : _ updatable_with_billing)
    ?(certificate : (a, b) credentials_certificate option)
    (credentials : a public_credentials_with_id) =
  let module G = (val w) in
  let () = if se.voters = [] then raise (Error (`ValidationError `NoVoters)) in
  let () =
    if not (List.length se.voters = List.length credentials) then
      raise (Error (`ValidationError `WrongLength))
  in
  let () =
    match certificate with
    | None -> ()
    | Some certificate ->
        let public_creds_ok =
          let public_creds_hash =
            List.map
              (fun (x : _ public_credential_with_id) -> x.credential)
              credentials
            |> yojson_of_public_credentials !&G.to_string
            |> Hash.hash_yojson
          in
          public_creds_hash = certificate.message.public_creds_hash
        in
        let certificate_ok =
          let module C = Credentials_certificate (G) in
          C.check certificate
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
      (fun (i, creds) (p : _ public_credential_with_id) ->
        let invalid fmt =
          Printf.ksprintf
            (fun x ->
              raise
                (Error
                   (`GenericError (Printf.sprintf "invalid %s at index %d" x i))))
            fmt
        in
        let weight = Option.value ~default:Weight.one p.credential.weight in
        let username = p.id in
        let cred_s = G.to_string p.credential.credential in
        let () =
          match SMap.find_opt username usernames with
          | None -> invalid "username %s" username
          | Some (w, used) ->
              if !used then invalid "duplicate username %s" username
              else if Weight.compare w weight <> 0 then
                invalid "differing weight"
              else if SSet.mem cred_s creds then invalid "duplicate credential"
              else if not (G.check p.credential.credential) then
                invalid "public credential"
              else used := true
        in
        (i + 1, SSet.add cred_s creds))
      (0, SSet.empty) credentials
  in
  let* () = Storage.E.set s (Public_creds (module G)) Value credentials in
  se.public_creds_received <- true;
  se.public_creds_certificate <- certificate;
  set (Draft (v, se))

let get_draft_trustees ~is_admin (Draft (_, se)) :
    _ Belenios_web_api.draft_trustees =
  let mode =
    match se.trustees.mode with
    | `Basic x ->
        let trustees =
          List.filter_map
            (fun (t : _ draft_basic_trustee) ->
              let state, key =
                match t.parameters with
                | None -> (Some 0, None)
                | Some tpk -> (Some 1, Some tpk)
              in
              let address, token, state =
                if is_admin then (Some t.address, Some t.token, state)
                else (None, None, None)
              in
              Some { address; name = t.name; token; state; key })
            x.trustees
        in
        `Basic ({ trustees } : _ basic_trustees)
    | `Threshold x ->
        let trustees =
          List.map
            (fun (t : _ draft_threshold_trustee) ->
              let address, token, state =
                if is_admin then
                  ( Some t.address,
                    Some t.token,
                    Some (Option.value t.step ~default:0) )
                else (None, None, None)
              in
              { address; name = t.name; token; state; key = t.cert })
            x.trustees
        in
        `Threshold
          ({ threshold = x.threshold; trustees } : _ threshold_trustees)
  in
  { step = se.trustees.step; mode }

let check_address address =
  if not @@ is_email address then raise (Error (`Invalid "e-mail address"))

let post_draft_trustees
    ((Draft (v, se), set) : _ draft_election updatable_with_billing)
    ({ name; address } : addable_trustee) =
  check_address address;
  match se.trustees.mode with
  | `Basic x ->
      let ts = x.trustees in
      let () =
        if
          List.exists
            (fun (x : _ draft_basic_trustee) -> x.address = address)
            ts
        then raise (Error (`GenericError "address already used"))
      in
      let token = generate_token 22 in
      x.trustees <- ts @ [ { name; address; token; parameters = None } ];
      set (Draft (v, se))
  | `Threshold x ->
      let ts = x.trustees in
      let () =
        if
          List.exists
            (fun (x : _ draft_threshold_trustee) -> x.address = address)
            ts
        then raise (Error (`GenericError "address already used"))
      in
      let token = generate_token 22 in
      let t =
        {
          address;
          name;
          token;
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
  match se.trustees.mode with
  | `Basic x ->
      let ts = x.trustees in
      let touched, ts =
        filter_out_first
          (fun (x : _ draft_basic_trustee) -> x.address = trustee)
          ts
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
          (fun (x : _ draft_threshold_trustee) -> x.address = trustee)
          ts
      in
      if touched then (
        x.trustees <- ts;
        let* () = set (Draft (v, se)) in
        Lwt.return_true)
      else Lwt.return_false

let set_threshold ((Draft (v, se), set) : _ updatable_with_billing) threshold =
  match se.trustees.mode with
  | `Basic _ -> Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x when x.trustees = [] -> Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x ->
      let ts = x.trustees in
      let maybe_threshold, step =
        if threshold = 0 then (None, None) else (Some threshold, Some 1)
      in
      if 0 <= threshold && threshold < List.length ts then (
        List.iter (fun (t : _ draft_threshold_trustee) -> t.step <- step) ts;
        x.threshold <- maybe_threshold;
        let* () = set (Draft (v, se)) in
        Lwt.return @@ Ok ())
      else Lwt.return @@ Stdlib.Error `OutOfBounds

let get_draft_trustees_mode (Draft (_, se)) =
  match se.trustees.mode with
  | `Basic _ -> `Basic
  | `Threshold x -> `Threshold (Option.value x.threshold ~default:0)

let put_draft_trustees_mode ((Draft (v, se), set) : _ updatable_with_billing)
    mode =
  match (get_draft_trustees_mode (Draft (v, se)), mode) with
  | a, b when a = b -> Lwt.return_unit
  | _, `Basic ->
      se.trustees <- { step = 1; mode = `Basic { trustees = [] } };
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
      se.trustees <- { step = 1; mode = `Threshold dtp };
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
  se.trustees <- { step = 1; mode = `Basic { trustees = [] } };
  set (Draft (v, se))

let get_draft_status uuid (Draft (v, se)) metadata =
  let* private_credentials_downloaded =
    if se.questions.credential_authority = `Server then
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
      if se.questions.credential_authority = `Server then Some `AutoCredentials
      else if
        match metadata.auth_config with
        | Some [ { auth_system = "import"; _ } ] -> false
        | _ -> true
      then Some `VoterAuthentication
      else if Belenios.Election.has_nh_questions (Template (v, se.questions))
      then Some `ForbiddenQuestions
      else if has_weights then Some `HasWeights
      else if se.group <> "Ed25519" then Some `BadGroup
      else
        match se.trustees.mode with
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
        (match se.trustees.mode with
        | `Basic x ->
            List.for_all
              (fun (t : _ draft_basic_trustee) -> t.parameters <> None)
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
        | Some (W (_, se)) -> cont @@ get_draft_voters se)
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

let import_trustees (type a b) (w : (a, b) group)
    ((Draft (v, se), set) : (a, b) draft_election updatable_with_billing) from
    (metadata : metadata) =
  let module G = (val w) in
  match metadata.trustees with
  | None -> Lwt.return @@ Stdlib.Error `None
  | Some metadata_trustees -> (
      let@ trustees cont =
        let* x = Public_archive.get_trustees from (module G) in
        match Lopt.get_value x with
        | None -> Lwt.return @@ Stdlib.Error `Invalid
        | Some x -> cont x
      in
      let module Trustees = (val Trustees.get_by_version se.version) in
      let module K = Trustees.MakeCombinator (G) in
      if not (K.check trustees) then Lwt.return @@ Stdlib.Error `Invalid
      else
        let import_pedersen (t : (a, b) threshold_parameters) ts =
          let* privs = Storage.E.get from (Private_keys (module G)) in
          let* x =
            match Lopt.get_value privs with
            | Some privs ->
                let rec loop ts certs pubs privs accu =
                  match (ts, certs, pubs, privs) with
                  | ( Some ({ address; token } : external_trustee) :: ts,
                      cert :: certs,
                      (public_key : _ threshold_verification_key) :: pubs,
                      private_key :: privs ) ->
                      let stt_name = public_key.message.message.name in
                      let stt_voutput = { public_key; private_key } in
                      let stt =
                        {
                          address;
                          token;
                          voutput = Some stt_voutput;
                          step = Some 7;
                          cert = Some cert;
                          polynomial = None;
                          vinput = None;
                          name = Option.value ~default:"N/A" stt_name;
                        }
                      in
                      loop ts certs pubs privs (stt :: accu)
                  | [], [], [], [] -> Lwt.return @@ Ok (List.rev accu)
                  | _ -> Lwt.return @@ Stdlib.Error `Inconsistent
                in
                loop ts (Array.to_list t.certs)
                  (Array.to_list t.verification_keys)
                  privs []
            | None -> Lwt.return @@ Stdlib.Error `MissingPrivateKeys
          in
          match x with
          | Ok se_threshold_trustees ->
              let dtp =
                {
                  algorithm = t.context.algorithm;
                  threshold = Some t.context.threshold;
                  trustees = se_threshold_trustees;
                  parameters = Some t;
                  error = None;
                }
              in
              se.trustees <- { se.trustees with mode = `Threshold dtp };
              let* () = set (Draft (v, se)) in
              Lwt.return @@ Ok `Threshold
          | Stdlib.Error _ as x -> Lwt.return x
        in
        match trustees with
        | [ `Pedersen t ] -> import_pedersen t metadata_trustees
        | [ `Single x; `Pedersen t ]
          when x.verification_key.message.message.name = None ->
            import_pedersen t (List.tl metadata_trustees)
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
            let ts =
              let module KG = Trustees.MakeBasic (G) in
              List.combine metadata_trustees ts
              |> List.filter_map (fun (t, (parameters : _ basic_parameters)) ->
                  match t with
                  | None -> None
                  | Some ({ address; token } : external_trustee) ->
                      let name =
                        match
                          parameters.verification_key.message.message.name
                        with
                        | None -> failwith __FUNCTION__
                        | Some x -> x
                      in
                      Some
                        { name; address; token; parameters = Some parameters })
            in
            se.trustees <- { se.trustees with mode = `Basic { trustees = ts } };
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
    (((W (w, Draft (v, se)) as wse), set) :
      wrapped_draft_election updatable_with_billing)
    ((metadata, set_metadata) : _ updatable) = function
  | `SetDownloaded ->
      let* () =
        if se.private_creds_downloaded then Lwt.return_unit
        else (
          se.private_creds_downloaded <- true;
          set (W (w, Draft (v, se))))
      in
      ok
  | `ValidateElection ->
      let* status = get_draft_status uuid (Draft (v, se)) metadata in
      let* () =
        Web_persist.validate_election ~admin_id s wse (metadata, set_metadata)
          status
      in
      ok
  | `SetCredentialAuthorityVisited ->
      let* () =
        if se.credential_authority_visited <> true then (
          se.credential_authority_visited <- true;
          let* () = set wse in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `SetVoterAuthenticationVisited ->
      let* () =
        if se.voter_authentication_visited <> true then (
          se.voter_authentication_visited <- true;
          let* () = set wse in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      ok
  | `InitiateCredentialAuthorityProtocol -> (
      match metadata.cred_authority_info with
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
      let@ se cont =
        let@ x, set = Storage.E.update s Draft in
        let set ?billing:_ x = set Value x in
        match Lopt.get_value x with None -> not_found | Some x -> cont (x, set)
      in
      let@ metadata cont =
        let@ x, set = Storage.E.update s Metadata in
        match Lopt.get_value x with
        | None -> assert false
        | Some x -> cont (x, fun x -> set Value x)
      in
      post_draft_status ~admin_id s uuid se metadata `ValidateElection

let post_trustee_basic (type a b) (w : (a, b) group)
    (((Draft (_, se) as fse), set) : _ updatable_with_billing) ~token data =
  let module G = (val w) in
  let ts =
    match se.trustees.mode with
    | `Basic x -> x.trustees
    | `Threshold _ -> failwith "Wrong trustee mode"
  in
  let t =
    match
      List.find_opt (fun (x : _ draft_basic_trustee) -> x.token = token) ts
    with
    | Some t -> t
    | None -> failwith "Invalid token"
  in
  match t.parameters with
  | None ->
      let module Trustees = (val Trustees.get_by_version se.version) in
      let parameters = !*[%group_of_yojson: _ basic_parameters] data in
      let module K = Trustees.MakeCombinator (G) in
      if
        parameters.verification_key.message.message.name = Some t.name
        && K.check [ `Single parameters ]
      then (
        t.parameters <- Some parameters;
        set fse)
      else raise @@ Error `InvalidPublicKey
  | _ -> raise @@ Error `PublicKeyExists

let post_trustee_threshold (type a b) (w : (a, b) group)
    (((Draft (_, se) as fse), set) :
      (a, b) draft_election updatable_with_billing) ~token data =
  let module G = (val w) in
  let dtp =
    match se.trustees.mode with
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
  let module Trustees = (val Trustees.get_by_version se.version) in
  let module P = Pki.Make (G) in
  let module C = Pki.MakeChannels (P) in
  let module K = Trustees.MakePedersen (C) in
  let () =
    match t.step with
    | Some 1 ->
        let cert = !*[%group_of_yojson: _ pedersen_cert] data in
        if K.step1_check full_context cert then (
          t.cert <- Some cert;
          t.step <- Some 2)
        else failwith "Invalid certificate"
    | Some 3 ->
        let certs = get_certs () in
        let polynomial = !*[%group_of_yojson: _ polynomial] data in
        if K.step3_check { context; certs } i polynomial then (
          t.polynomial <- Some polynomial;
          t.step <- Some 4)
        else failwith "Invalid polynomial"
    | Some 5 ->
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutput = !*[%group_of_yojson: _ voutput] data in
        if K.step5_check { context; certs } i polynomials voutput then (
          t.voutput <- Some voutput;
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
        Array.iter (fun (x : _ draft_threshold_trustee) -> x.step <- Some 3) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun (x : _ draft_threshold_trustee) -> x.step = Some 4) ts
    then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let vinputs = K.step4 { context; certs } polynomials in
        for j = 0 to Array.length ts - 1 do
          ts.(j).vinput <- Some vinputs.(j)
        done;
        Array.iter (fun (x : _ draft_threshold_trustee) -> x.step <- Some 5) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun (x : _ draft_threshold_trustee) -> x.step = Some 6) ts
    then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutputs =
          Array.map
            (fun x ->
              match x.voutput with
              | None -> failwith "Missing voutput"
              | Some y -> y)
            ts
        in
        let p = K.step6 { context; certs } polynomials voutputs in
        dtp.parameters <- Some p;
        Array.iter (fun (x : _ draft_threshold_trustee) -> x.step <- Some 7) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  set fse

let dispatch_credentials ~token endpoint method_ body s uuid
    ((wse, wset) : _ updatable_with_billing) metadata =
  let (W (w, se) : wrapped_draft_election) = wse in
  let module G = (val w) in
  let set ?billing x = wset ?billing (W (w, x)) in
  match endpoint with
  | [ "token" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET -> (
          let* x = get_credentials_token se in
          match x with
          | None -> not_found
          | Some x -> return_generic { mime = "text/plain"; content = String x }
          )
      | _ -> method_not_allowed)
  | [ "private" ] -> (
      let@ _ = with_administrator token metadata in
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
              let* x = Web_persist.get_draft_public_credentials s (module G) in
              match x with
              | None -> Lwt.return_none
              | Some x ->
                  Lwt.return_some
                  @@ yojson_of_public_credentials !&G.to_string x)
      | `POST -> (
          let@ who =
            with_administrator_or_credential_authority token se metadata
          in
          if Web_persist.get_credentials_status uuid se <> `None then forbidden
          else
            let@ x =
              body.run !*(public_credentials_with_id_of_yojson !$G.of_string)
            in
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
                let* () = submit_public_credentials s w (se, set) credentials in
                ok
            | _ -> forbidden)
      | _ -> method_not_allowed)
  | _ -> not_found

let handle_trustee ~token method_ body ((wse, wset) : _ updatable_with_billing)
    =
  let (W (w, se) : wrapped_draft_election) = wse in
  let set ?billing x = wset ?billing (W (w, x)) in
  let module G = (val w) in
  let@ trustee = with_trustee token se in
  let get () =
    let@ () =
     fun cont ->
      Lwt.return @@ [%yojson_of_group: _ trustee_status] @@ `Draft (cont ())
    in
    match trustee with
    | `Basic b ->
        let state =
          match b.parameters with None -> `Init b.name | Some _ -> `Done
        in
        `Basic state
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
                      voutput = t.voutput;
                    }
                with Exit -> `WaitingForOtherCertificates)))
  in
  match method_ with
  | `GET -> handle_get get
  | `POST -> (
      let@ data = body.run Fun.id in
      let@ token = Option.unwrap unauthorized token.token in
      let@ () = handle_generic_error in
      match trustee with
      | `Basic _ ->
          let* () = post_trustee_basic w (se, set) ~token data in
          ok
      | `Threshold _ ->
          let* () = post_trustee_threshold w (se, set) ~token data in
          ok)
  | _ -> method_not_allowed

let dispatch_draft ~token ~ifmatch endpoint method_ body s uuid
    ((wse, wset) : _ updatable_with_billing)
    ((metadata, set_metadata) : _ updatable) =
  let (W (w, se) : wrapped_draft_election) = wse in
  let module G = (val w) in
  let set ?billing x = wset ?billing (W (w, x)) in
  match endpoint with
  | [] -> (
      let@ who = with_administrator_or_nobody token metadata in
      let get () =
        let* x = api_of_draft se metadata in
        Lwt.return @@ yojson_of_draft x
      in
      match (method_, who) with
      | `GET, _ -> handle_get get
      | `PUT, `Administrator account ->
          let@ () = handle_ifmatch ifmatch get in
          let@ draft = body.run !*draft_of_yojson in
          let@ () = handle_generic_error in
          let se, metadata = draft_of_api account uuid se metadata draft in
          let* () = set se in
          let* () = set_metadata metadata in
          ok
      | `POST, `Administrator a ->
          let@ () = handle_ifmatch ifmatch get in
          let@ x = body.run !*draft_request_of_yojson in
          let@ () = handle_generic_error in
          post_draft_status ~admin_id:a.id s uuid (wse, wset)
            (metadata, set_metadata) x
      | _ -> method_not_allowed)
  | [ "voters" ] -> (
      let@ who = with_administrator_or_credential_authority token se metadata in
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
      dispatch_credentials ~token endpoint method_ body s uuid (wse, wset)
        metadata
  | [ "trustees" ] -> (
      let@ who = with_administrator_or_nobody token metadata in
      let get is_admin () =
        let open Belenios_web_api in
        let x = get_draft_trustees ~is_admin se in
        Lwt.return @@ [%yojson_of_group: _ draft_trustees] x
      in
      match (method_, who) with
      | `GET, `Nobody -> handle_get (get false)
      | `GET, `Administrator _ -> handle_get (get true)
      | `POST, `Administrator account -> (
          let@ () = handle_ifmatch ifmatch (get true) in
          let@ request = body.run !*trustees_request_of_yojson in
          let@ () = handle_generic_error in
          match request with
          | `SetStep step ->
              let* () =
                let (Draft (v, se)) = se in
                if step <> se.trustees.step then (
                  se.trustees <- { se.trustees with step };
                  set (Draft (v, se)))
                else Lwt.return_unit
              in
              ok
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
              let* x = import_trustees w (se, set) from metadata in
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
      let@ _ = with_administrator token metadata in
      match method_ with
      | `DELETE ->
          let@ () = handle_generic_error in
          let* x = delete_draft_trustee (se, set) trustee in
          if x then ok else not_found
      | _ -> method_not_allowed)
  | [ "status" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = get_draft_status uuid se metadata in
          return_json 200 (!+yojson_of_draft_status x)
      | _ -> method_not_allowed)
  | _ -> not_found
