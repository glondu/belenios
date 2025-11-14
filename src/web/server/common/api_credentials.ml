(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2025-2025 Inria                                           *)
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
open Belenios_web_api
open Api_generic

let process_request_new (r : draft_credentials_new_request) (Draft (_, draft))
    voter_list () =
  let seed = Belenios_server_core.generate_token ~length:22 () in
  let module G =
    (val Belenios.Group.of_string ~version:draft.draft_version draft.draft_group)
  in
  let module P = Pki.Make (G) (Belenios_server_core.Random) in
  let decryption_key = P.derive_dk seed in
  let encryption_key = G.(g **~ decryption_key) in
  let signature_key = P.derive_sk seed in
  let verification_key = G.(g **~ signature_key) in
  let module Cred =
    Credential.Make
      (G)
      (struct
        type 'a t = 'a Lwt.t

        let return = Lwt.return
        let bind = Lwt.bind
        let pause = Lwt.pause
        let uuid = r.uuid
      end)
  in
  let* creds = Cred.generate voter_list in
  let* credentials_records =
    let map =
      List.fold_left
        (fun accu (v : voter) ->
          let voter = Voter.get v in
          let v = snd v in
          SMap.add voter (v.weight, v.address) accu)
        SMap.empty voter_list
    in
    Lwt_list.map_s
      (fun (voter, x) ->
        let* credential = P.encrypt encryption_key x in
        let credential =
          credential
          |> string_of_encrypted_msg (swrite G.to_string)
          |> encrypted_msg_of_string Yojson.Safe.read_json
        in
        let weight, address =
          match SMap.find_opt voter map with
          | None -> (None, None)
          | Some x -> x
        in
        let x : _ credentials_record = { credential; weight; address } in
        Lwt.return (voter, x))
      creds.private_creds
  in
  let public_creds_hash =
    creds.public_creds |> string_of_public_credentials |> Hash.hash_string
  in
  let certificate_raw : (_, _) credentials_certificate =
    {
      uuid = r.uuid;
      voter_list_length = List.length voter_list;
      public_creds_hash;
      verification_key;
      encryption_key;
      signature = None;
    }
  in
  let hash =
    certificate_raw
    |> string_of_credentials_certificate (swrite G.to_string)
         (swrite G.Zq.to_string)
    |> Hash.hash_string
  in
  let certificate =
    let signature = P.sign signature_key (Hash.to_hex hash) in
    { certificate_raw with signature = Some signature }
    |> string_of_credentials_certificate (swrite G.to_string)
         (swrite G.Zq.to_string)
    |> credentials_certificate_of_string Yojson.Safe.read_json
         Yojson.Safe.read_json
  in
  let* () =
    let@ s = Storage.with_transaction in
    let* () =
      {
        belenios_url = r.belenios_url;
        version = draft.draft_version;
        group = draft.draft_group;
        certificate;
      }
      |> Storage.set s (Election (r.uuid, Credentials_params)) Value
    in
    let* () =
      { seed; token = r.token }
      |> Storage.set s (Election (r.uuid, Credentials_seed)) Value
    in
    Storage.set s
      (Election (r.uuid, Credentials_records))
      Value credentials_records
  in
  let* () =
    let* x =
      Send_message.send
      @@ `Credentials_seed
           {
             lang = "en";
             admin_id = r.admin_id;
             uuid = r.uuid;
             belenios_url = r.belenios_url;
             cred_authority_info = r.info;
             seed;
           }
    in
    match x with
    | Ok _ -> Lwt.return_unit
    | Error () ->
        let msg =
          Printf.sprintf "sending seed for %s to %s failed" (Uuid.unwrap r.uuid)
            r.info.cred_operator
        in
        Ocsigen_messages.errlog msg;
        Lwt.return_unit
  in
  let url = Printf.sprintf "%sapi/credentials/belenios" r.belenios_url in
  let body =
    { certificate; token = r.token; public_credentials = creds.public_with_ids }
    |> string_of_draft_credentials_response |> Cohttp_lwt.Body.of_string
  in
  let* x, body = Cohttp_lwt_unix.Client.post ~body (Uri.of_string url) in
  let* () = Cohttp_lwt.Body.drain_body body in
  match Cohttp.Code.code_of_status x.status with
  | 200 -> Lwt.return_unit
  | code ->
      let msg =
        Printf.sprintf
          "submitting public credendials for %s to %s failed with status code \
           %d"
          (Uuid.unwrap r.uuid) url code
      in
      Ocsigen_messages.errlog msg;
      Lwt.return_unit

let get_missing_voters ~belenios_url ~seed uuid credentials_records =
  let@ roots cont =
    let e = Belenios_web_api.Endpoints.election_roots uuid in
    let url = Printf.sprintf "%sapi/%s" belenios_url e.path in
    let* r, x = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    let* x = Cohttp_lwt.Body.to_string x in
    match Cohttp.Code.code_of_status r.status with
    | 200 -> cont @@ e.of_string x
    | code ->
        let msg = Printf.sprintf "GET %s returned status %d" url code in
        Ocsigen_messages.errlog msg;
        Lwt.return_nil
  in
  let get_object h =
    let e = Belenios_web_api.Endpoints.election_object uuid h in
    let url = Printf.sprintf "%sapi/%s" belenios_url e.path in
    let* r, x = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    let* x = Cohttp_lwt.Body.to_string x in
    match Cohttp.Code.code_of_status r.status with
    | 200 -> Lwt.return_some x
    | _ -> Lwt.return_none
  in
  let@ setup cont =
    match roots.roots_setup_data with
    | None -> Lwt.return_nil
    | Some h -> (
        let* x = get_object h in
        match x with
        | None -> Lwt.return_nil
        | Some x -> cont @@ setup_data_of_string x)
  in
  let@ election cont =
    let* x = get_object setup.setup_election in
    match x with
    | None -> Lwt.return_nil
    | Some x -> cont @@ Election.of_string (module Dummy_random) x
  in
  let module E = (val election) in
  let module G = E.G in
  let module GMap = Map.Make (G) in
  let* credentials_records =
    let module X = struct
      type 'a t = 'a Lwt.t

      let return = Lwt.return
      let bind = Lwt.bind
      let pause = Lwt.pause
      let uuid = uuid
    end in
    let module C = Credential.Make (G) (X) in
    let module P = Pki.Make (G) (Dummy_random) in
    let decryption_key = P.derive_dk seed in
    credentials_records
    |> Lwt_list.fold_left_s
         (fun accu (v, c) ->
           let encrypted_msg =
             c.credential
             |> string_of_encrypted_msg Yojson.Safe.write_json
             |> encrypted_msg_of_string (sread G.of_string)
           in
           let* x = P.decrypt decryption_key encrypted_msg in
           match x with
           | None -> Lwt.return accu
           | Some credential -> (
               let* priv = C.derive credential in
               match priv with
               | Ok priv ->
                   Lwt.return
                   @@ GMap.add G.(g **~ priv) (v, { c with credential }) accu
               | Error _ -> Lwt.return accu))
         GMap.empty
  in
  let get_ballot_event h =
    let* x = get_object h in
    match x with
    | None -> Lwt.return_none
    | Some x -> (
        let event = event_of_string x in
        match event.event_typ with
        | `Ballot -> (
            match event.event_payload with
            | None -> Lwt.return_none
            | Some h' -> (
                let* b = get_object h' in
                match b with
                | None -> Lwt.return_none
                | Some b -> (
                    let ballot = E.read_ballot ++ b in
                    match E.get_credential ballot with
                    | None -> Lwt.return_none
                    | Some c -> Lwt.return_some (c, event.event_parent))))
        | _ -> Lwt.return_none)
  in
  let rec loop accu = function
    | None -> Lwt.return accu
    | Some h -> (
        let* x = get_ballot_event h in
        match x with
        | None -> Lwt.return accu
        | Some (c, p) -> loop (GMap.remove c accu) p)
  in
  let* credentials_records =
    loop credentials_records roots.roots_last_ballot_event
  in
  GMap.fold (fun _ x accu -> x :: accu) credentials_records [] |> Lwt.return

let process_resend_request (r : draft_credentials_resend)
    (params : credentials_params) metadata credentials_records () =
  let voters =
    match r.spec with
    | `All_voters ->
        `Some_voters
          (List.fold_left
             (fun accu (x, _) -> SSet.add x accu)
             SSet.empty credentials_records)
    | `Some_voters xs ->
        `Some_voters
          (List.fold_left (fun accu x -> SSet.add x accu) SSet.empty xs)
    | `Missing_voters -> `Missing_voters
  in
  let* credentials_records =
    match voters with
    | `Missing_voters ->
        get_missing_voters ~belenios_url:params.belenios_url ~seed:r.seed r.uuid
          credentials_records
    | `Some_voters voters ->
        let module G =
          (val Belenios.Group.of_string ~version:params.version params.group)
        in
        let module P = Pki.Make (G) (Dummy_random) in
        let decryption_key = P.derive_dk r.seed in
        credentials_records
        |> Lwt_list.filter_map_s (fun (v, c) ->
               if SSet.mem v voters then
                 let encrypted_msg =
                   c.credential
                   |> string_of_encrypted_msg Yojson.Safe.write_json
                   |> encrypted_msg_of_string (sread G.of_string)
                 in
                 let* x = P.decrypt decryption_key encrypted_msg in
                 match x with
                 | None -> Lwt.return_none
                 | Some credential -> Lwt.return_some (v, { c with credential })
               else Lwt.return_none)
  in
  let send = Mails_voter.generate_credential_email metadata in
  let* jobs =
    credentials_records
    |> Lwt_list.map_s (fun (login, (c : _ credentials_record)) ->
           send ?recipient:c.address ~login ?weight:c.weight c.credential)
  in
  Mails_voter_bulk.submit_bulk_emails jobs

let process_request s : draft_credentials_request -> _ = function
  | `NewRequest r ->
      let@ _check_info cont =
        if
          r.info.cred_server
          = Printf.sprintf "%s/api/credentials/server" !Web_config.prefix
          && is_email r.info.cred_operator
        then cont ()
        else return_yojson 400 `Null
      in
      let uuid = Uuid.unwrap r.uuid in
      let@ draft cont =
        let url =
          Printf.sprintf "%sapi/elections/%s/draft" r.belenios_url uuid
        in
        let* x, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
        let* body = Cohttp_lwt.Body.to_string body in
        match Cohttp.Code.code_of_status x.status with
        | 200 -> (
            match draft_of_string body with
            | exception _ -> return_yojson 502 `Null
            | x -> cont x)
        | code -> return_yojson 502 (`Int code)
      in
      let@ voter_list cont =
        let url =
          Printf.sprintf "%sapi/elections/%s/draft/voters" r.belenios_url uuid
        in
        let authorization = Printf.sprintf "Bearer %s" r.token in
        let headers =
          [ ("Authorization", authorization) ] |> Cohttp.Header.of_list
        in
        let* x, body =
          Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url)
        in
        let* body = Cohttp_lwt.Body.to_string body in
        match Cohttp.Code.code_of_status x.status with
        | 200 -> (
            match voter_list_of_string body with
            | exception _ -> return_yojson 502 `Null
            | x -> cont x)
        | code -> return_yojson 502 (`Int code)
      in
      Lwt.async (process_request_new r draft voter_list);
      ok
  | `Validate r ->
      let* () =
        Storage.set s (Election (r.uuid, Credentials_metadata)) Value r.metadata
      in
      let@ seed cont =
        let* x = Storage.get s (Election (r.uuid, Credentials_seed)) in
        match Lopt.get_value x with
        | Some x -> if r.token = x.token then cont x.seed else forbidden
        | None -> not_found
      in
      let@ params cont =
        let* p = Storage.get s (Election (r.uuid, Credentials_params)) in
        match Lopt.get_value p with Some p -> cont p | None -> not_found
      in
      let@ credentials_records cont =
        let* x = Storage.get s (Election (r.uuid, Credentials_records)) in
        match Lopt.get_value x with Some x -> cont x | None -> not_found
      in
      let* credentials_records =
        let module G =
          (val Belenios.Group.of_string ~version:params.version params.group)
        in
        let module P = Pki.Make (G) (Belenios_server_core.Random) in
        let decryption_key = P.derive_dk seed in
        Lwt_list.filter_map_s
          (fun (v, (c : _ credentials_record)) ->
            let encrypted_msg =
              c.credential
              |> string_of_encrypted_msg Yojson.Safe.write_json
              |> encrypted_msg_of_string (sread G.of_string)
            in
            let* x = P.decrypt decryption_key encrypted_msg in
            match x with
            | None -> Lwt.return_none
            | Some credential -> Lwt.return_some (v, { c with credential }))
          credentials_records
      in
      let send = Mails_voter.generate_credential_email r.metadata in
      let* jobs =
        Lwt_list.map_s
          (fun (login, (c : _ credentials_record)) ->
            send ?recipient:c.address ~login ?weight:c.weight c.credential)
          credentials_records
      in
      let* () = Mails_voter_bulk.submit_bulk_emails jobs in
      let* () = Storage.del s (Election (r.uuid, Credentials_seed)) in
      ok
  | `Resend r ->
      let@ params cont =
        let* p = Storage.get s (Election (r.uuid, Credentials_params)) in
        match Lopt.get_value p with Some p -> cont p | None -> not_found
      in
      let module G =
        (val Belenios.Group.of_string ~version:params.version params.group)
      in
      let certificate =
        params.certificate
        |> string_of_credentials_certificate Yojson.Safe.write_json
             Yojson.Safe.write_json
        |> credentials_certificate_of_string (sread G.of_string)
             (sread G.Zq.of_string)
      in
      let module P = Pki.Make (G) (Dummy_random) in
      let decryption_key = P.derive_dk r.seed in
      if G.(certificate.encryption_key =~ g **~ decryption_key) then (
        let@ credentials_records cont =
          let* x = Storage.get s (Election (r.uuid, Credentials_records)) in
          match Lopt.get_value x with Some x -> cont x | None -> not_found
        in
        let@ metadata cont =
          let* x = Storage.get s (Election (r.uuid, Credentials_metadata)) in
          match Lopt.get_value x with Some x -> cont x | None -> not_found
        in
        Lwt.async (process_resend_request r params metadata credentials_records);
        ok)
      else unauthorized

let dispatch s endpoint method_ body =
  match endpoint with
  | [ "server" ] -> (
      match method_ with
      | `POST ->
          let@ request = body.run draft_credentials_request_of_string in
          let@ () = handle_generic_error in
          process_request s request
      | _ -> method_not_allowed)
  | [ "belenios" ] -> (
      match method_ with
      | `POST -> (
          let@ response = body.run draft_credentials_response_of_string in
          let certificate = response.certificate in
          let uuid = certificate.uuid in
          let@ () = handle_generic_error in
          let@ se, set = Storage.update s (Election (uuid, Draft)) in
          let set ?billing:_ x = set Value x in
          match Lopt.get_value se with
          | None -> not_found
          | Some (Draft (_, se) as x) ->
              if se.se_public_creds = response.token then
                let* () =
                  Api_drafts.submit_public_credentials s uuid (x, set)
                    ~certificate response.public_credentials
                in
                ok
              else forbidden)
      | _ -> method_not_allowed)
  | _ -> not_found
