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
        if not (Voter.validate voter) then fail @@ `BadVoter voter;
        let login = Voter.get voter in
        match SMap.find_opt (String.lowercase_ascii login) existing_voters with
        | None -> { id = voter }
        | Some v ->
            v.id <- voter;
            v)
      voters
  in
  let* total_weight, _ =
    Lwt_list.fold_left_s
      (fun (total_weight, voters) (v : draft_voter) ->
        let login = Voter.get v.id in
        let weight = Voter.get_weight v.id in
        let login = String.lowercase_ascii login in
        let* voters =
          if SSet.mem login voters then fail @@ `Duplicate login
          else Lwt.return (SSet.add login voters)
        in
        Lwt.return (Weight.(total_weight + weight), voters))
      (Weight.zero, SSet.empty) se_voters
  in
  let* () =
    if Weight.(compare total_weight max_weight) > 0 then
      fail @@ `TotalWeightTooBig (total_weight, Weight.max_weight)
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
  let* () = Storage.E.set s (Public_creds G.spec) Value credentials in
  se.public_creds_received <- true;
  se.public_creds_certificate <- certificate;
  set (Draft (v, se))

let get_draft_status uuid (Draft (v, se)) (metadata : metadata) =
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
  let { version; group; _ } : _ raw_draft_election = se in
  let module G = (val Group.make { version; group }) in
  let* trustees =
    let&* uuid = metadata.trustees in
    let@ s = Storage.T.with_transaction uuid in
    let* x = Storage.T.get s (Trustees G.spec) in
    let&* x = Lopt.get_value x in
    Lwt.return_some x
  in
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
        match trustees with
        | Some trustees -> (
            let t =
              List.find_map
                (function
                  | `Single _ -> None
                  | `Pedersen (t : _ threshold_parameters) -> Some t)
                trustees
            in
            match t with
            | None -> Some `NoThreshold
            | Some t ->
                if Array.length t.certs >= 3 then None
                else Some `NotEnoughTrustees)
        | None -> Some `NotEnoughTrustees
    else None
  in
  Lwt.return
    {
      num_voters = List.length se.voters;
      credentials_ready;
      credentials_left;
      private_credentials_downloaded;
      trustees_ready = trustees <> None;
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
        if Weight.(compare total_weight max_weight) <= 0 then (
          se.voters <- voters;
          let* () = set (Draft (v, se)) in
          Lwt.return @@ Ok ())
        else Lwt.return @@ Stdlib.Error (`TotalWeightTooBig total_weight)
    | Error x ->
        let login = Voter.get x in
        Lwt.return @@ Stdlib.Error (`Duplicate login)

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

let set_trustees election (se : _ raw_draft_election) uuid
    ((metadata, set_metadata) : metadata updatable) =
  let@ s = Storage.T.with_transaction uuid in
  let* x = Storage.T.get s Trustees_metadata in
  match Lopt.get_value x with
  | Some m when m.version = se.version && m.group = se.group ->
      let* () = Storage.T.add_election s election in
      let* () = set_metadata { metadata with trustees = Some uuid } in
      ok
  | _ -> forbidden

let post_draft_status ~admin_id s uuid
    (((W (w, Draft (v, se)) as wse), set) :
      wrapped_draft_election updatable_with_billing)
    ((metadata, set_metadata) : metadata updatable) = function
  | `SetDownloaded ->
      let* () =
        if se.private_creds_downloaded then Lwt.return_unit
        else (
          se.private_creds_downloaded <- true;
          set (W (w, Draft (v, se))))
      in
      ok
  | `SetTrustees trustees -> (
      match trustees with
      | None ->
          let* () = set_metadata { metadata with trustees } in
          ok
      | Some trustees -> set_trustees uuid se trustees (metadata, set_metadata))
  | `ImportTrustees from -> (
      let@ s = Storage.E.with_transaction from in
      let* x = Storage.E.get s Metadata in
      match Lopt.get_value x with
      | Some { trustees = Some trustees; _ } ->
          set_trustees uuid se trustees (metadata, set_metadata)
      | _ -> forbidden)
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
  | [ "status" ] -> (
      let@ _ = with_administrator token metadata in
      match method_ with
      | `GET ->
          let@ () = handle_generic_error in
          let* x = get_draft_status uuid se metadata in
          return_json 200 (!+yojson_of_draft_status x)
      | _ -> method_not_allowed)
  | _ -> not_found
