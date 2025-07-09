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
