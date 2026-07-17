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

let process_request_new (r : credentials_new_request) (Draft (_, draft))
    voter_list () =
  let algorithm = default_algorithm in
  let seed = generate_token 44 in
  let { version; group; _ } : raw_draft = draft in
  let module G = (val Belenios.Group.make { version; group }) in
  let module P = Pki.Make (G) in
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
      end) in
  let* creds = Cred.generate voter_list in
  let* records =
    let map =
      List.fold_left
        (fun accu (v : voter) ->
          let voter = v.login in
          SMap.add voter (v.weight, v.address) accu)
        SMap.empty voter_list
    in
    Lwt_list.map_s
      (fun (voter, x) ->
        let* credential =
          P.encrypt ~algorithm xch_encrypted_credential encryption_key x
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
    creds.public_creds
    |> yojson_of_public_credentials !&G.to_string
    |> Hash.hash_yojson
  in
  let raw_certificate : (_, _) raw_credentials_certificate =
    {
      uuid = r.uuid;
      voter_list_length = List.length voter_list;
      public_creds_hash;
      verification_key;
      encryption_key;
    }
  in
  let open Credentials_certificate (G) in
  let certificate =
    P.sign xch_credentials_certificate signature_key raw_certificate
  in
  let@ () =
   fun cont ->
    Lwt.try_bind
      (fun () -> Storage.C.new_election r.uuid)
      (fun () ->
        let@ s = Storage.C.with_transaction r.uuid in
        let* () =
          W
            ( (module G),
              {
                belenios_url = r.belenios_url;
                version = draft.version;
                group = draft.group;
                certificate;
              } )
          |> Storage.C.set s Credentials_params Value
        in
        let* () =
          { seed; token = r.token } |> Storage.C.set s Credentials_seed Value
        in
        let* () =
          Storage.C.set s (Credentials_records G.spec) Value
            { algorithm; records }
        in
        cont ())
      (fun e ->
        let msg =
          Printf.sprintf "could not create credentials spool dir for %s [%s]"
            (Uuid.to_string r.uuid) (Printexc.to_string e)
        in
        Ocsigen_messages.errlog msg;
        Lwt.return_unit)
  in
  let* () =
    let* x =
      Send_message.send
      @@ `Credentials_seed
           {
             lang = Language.(unwrap default);
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
          Printf.sprintf "sending seed for %s to %s failed"
            (Uuid.to_string r.uuid) r.info.operator
        in
        Ocsigen_messages.errlog msg;
        Lwt.return_unit
  in
  let url = Printf.sprintf "%sapi/credentials/belenios" r.belenios_url in
  let body =
    { certificate; token = r.token; public_credentials = creds.public_with_ids }
    |> !+[%yojson_of_group: _ credentials_response]
    |> Cohttp_lwt.Body.of_string
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
          (Uuid.to_string r.uuid) url code
      in
      Ocsigen_messages.errlog msg;
      Lwt.return_unit

let get_missing_voters ~belenios_url ~seed uuid (type a b) (w : (a, b) group)
    (credentials_records : (a, b) credentials_records) =
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
  let module G = (val w) in
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
    let module P = Pki.Make (G) in
    let decryption_key = P.derive_dk seed in
    let { algorithm; records } = credentials_records in
    records
    |> Lwt_list.fold_left_s
         (fun accu (v, c) ->
           let encrypted_msg = c.credential in
           let* x =
             P.decrypt ~algorithm xch_encrypted_credential decryption_key
               encrypted_msg
           in
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
        let event = !*event_of_yojson x in
        match event.typ with
        | `Ballot -> (
            match event.payload with
            | None -> Lwt.return_none
            | Some h' -> (
                let* b = get_object h' in
                match b with
                | None -> Lwt.return_none
                | Some b ->
                    let ballot = !*[%group_of_yojson: _ ballot] b in
                    let c = ballot.message.credential in
                    Lwt.return_some (c, event.parent)))
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
  let* credentials_records = loop credentials_records roots.last_ballot_event in
  GMap.fold (fun _ x accu -> x :: accu) credentials_records [] |> Lwt.return

let process_resend_request (r : credentials_resend) (type a b)
    (w : (a, b) group) (params : (a, b) credentials_params) metadata
    (credentials_records : (a, b) credentials_records) () =
  let { algorithm; records } = credentials_records in
  let voters =
    match r.spec with
    | `All_voters ->
        `Some_voters
          (List.fold_left
             (fun accu (x, _) -> SSet.add x accu)
             SSet.empty records)
    | `Some_voters xs ->
        `Some_voters
          (List.fold_left (fun accu x -> SSet.add x accu) SSet.empty xs)
    | `Missing_voters -> `Missing_voters
  in
  let* credentials_records =
    match voters with
    | `Missing_voters ->
        get_missing_voters ~belenios_url:params.belenios_url ~seed:r.seed r.uuid
          w credentials_records
    | `Some_voters voters ->
        let module G = (val w) in
        let module P = Pki.Make (G) in
        let decryption_key = P.derive_dk r.seed in
        records
        |> Lwt_list.filter_map_s (fun (v, c) ->
            if SSet.mem v voters then
              let encrypted_msg = c.credential in
              let* x =
                P.decrypt ~algorithm xch_encrypted_credential decryption_key
                  encrypted_msg
              in
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
  let credit : credentials_credit =
    {
      credits = -List.length jobs;
      success = true;
      kind =
        `Resend
          (match r.spec with
          | `All_voters -> `All_voters
          | `Missing_voters -> `Missing_voters
          | `Some_voters _ -> `Some_voters);
      timestamp = Unix.gettimeofday ();
    }
  in
  let* success =
    let@ s = Storage.C.with_transaction r.uuid in
    let@ x, set = Storage.C.update s Credentials_credits in
    match Lopt.get_value x with
    | None -> Lwt.return_false
    | Some xs ->
        let success =
          Belenios_web_api.remaining_credits xs + credit.credits >= 0
        in
        let* () = set Value ({ credit with success } :: xs) in
        Lwt.return success
  in
  if success then Mails_voter_bulk.submit_bulk_emails jobs else Lwt.return_unit

let check_seed ~(params : wrapped_credentials_params) ~seed =
  let (W (w, params)) = params in
  let module G = (val w) in
  let certificate = params.certificate in
  let module P = Pki.Make (G) in
  let decryption_key = P.derive_dk seed in
  G.(certificate.message.encryption_key =~ g **~ decryption_key)

let process_request : credentials_request -> _ = function
  | `NewRequest r ->
      let@ _check_belenios_url cont =
        if !Web_config.credentials_server_allowed r.belenios_url then cont ()
        else return_yojson 400 `Null
      in
      let@ _check_info cont =
        if
          r.info.server
          = Printf.sprintf "%s/api/credentials/server" !Web_config.prefix
          && is_email r.info.operator
        then cont ()
        else return_yojson 400 `Null
      in
      let uuid = Uuid.to_string r.uuid in
      let@ draft cont =
        let url =
          Printf.sprintf "%sapi/elections/%s/draft" r.belenios_url uuid
        in
        let* x, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
        let* body = Cohttp_lwt.Body.to_string body in
        match Cohttp.Code.code_of_status x.status with
        | 200 -> (
            match !*draft_of_yojson body with
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
            match !*voter_list_of_yojson body with
            | exception _ -> return_yojson 502 `Null
            | x -> cont x)
        | code -> return_yojson 502 (`Int code)
      in
      Lwt.async (process_request_new r draft voter_list);
      ok
  | `Validate r ->
      let@ s = Storage.C.with_transaction r.uuid in
      let* () = Storage.C.set s Credentials_metadata Value r.metadata in
      let@ seed cont =
        let* x = Storage.C.get s Credentials_seed in
        match Lopt.get_value x with
        | Some x -> if r.token = x.token then cont x.seed else forbidden
        | None -> not_found
      in
      let@ params cont =
        let* p = Storage.C.get s Credentials_params in
        match Lopt.get_value p with Some p -> cont p | None -> not_found
      in
      let (W (w, _)) = params in
      let module G = (val w) in
      let@ credentials_records cont =
        let* x = Storage.C.get s (Credentials_records G.spec) in
        match Lopt.get_value x with Some x -> cont x | None -> not_found
      in
      let { algorithm; records } = credentials_records in
      let n = List.length records in
      let* credentials_records =
        let module G = (val w) in
        let module P = Pki.Make (G) in
        let decryption_key = P.derive_dk seed in
        Lwt_list.filter_map_s
          (fun (v, (c : _ credentials_record)) ->
            let encrypted_msg = c.credential in
            let* x =
              P.decrypt ~algorithm xch_encrypted_credential decryption_key
                encrypted_msg
            in
            match x with
            | None -> Lwt.return_none
            | Some credential -> Lwt.return_some (v, { c with credential }))
          records
      in
      let send = Mails_voter.generate_credential_email r.metadata in
      let* jobs =
        Lwt_list.map_s
          (fun (login, (c : _ credentials_record)) ->
            send ?recipient:c.address ~login ?weight:c.weight c.credential)
          credentials_records
      in
      let* () = Mails_voter_bulk.submit_bulk_emails jobs in
      let* () = Storage.C.del s Credentials_seed in
      let* () =
        let timestamp = Unix.gettimeofday () in
        let credits =
          !Web_config.credentials_server_multiplier *. float n
          |> Float.floor |> Float.to_int
        in
        let credit : credentials_credit =
          { credits; success = true; kind = `Initial; timestamp }
        in
        Storage.C.set s Credentials_credits Value [ credit ]
      in
      ok
  | `Resend r ->
      let@ s = Storage.C.with_transaction r.uuid in
      let@ params cont =
        let* p = Storage.C.get s Credentials_params in
        match Lopt.get_value p with Some p -> cont p | None -> not_found
      in
      if check_seed ~params ~seed:r.seed then (
        let (W (w, params)) = params in
        let module G = (val w) in
        let@ credentials_records cont =
          let* x = Storage.C.get s (Credentials_records G.spec) in
          match Lopt.get_value x with Some x -> cont x | None -> not_found
        in
        let@ metadata cont =
          let* x = Storage.C.get s Credentials_metadata in
          match Lopt.get_value x with Some x -> cont x | None -> not_found
        in
        Lwt.async
          (process_resend_request r w params metadata credentials_records);
        ok)
      else unauthorized

let dispatch endpoint method_ body =
  match endpoint with
  | [ "server" ] -> (
      match method_ with
      | `POST ->
          let@ request = body.run !*credentials_request_of_yojson in
          let@ () = handle_generic_error in
          process_request request
      | _ -> method_not_allowed)
  | [ "server"; "credits"; uuid ] -> (
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.C.with_transaction uuid in
      match method_ with
      | `GET ->
          let@ seed cont =
            let sp = Eliom_common.get_sp () in
            match
              Ocsigen_request.header sp.sp_request.request_info
                (Ocsigen_header.Name.of_string "Authorization")
            with
            | None -> unauthorized
            | Some x -> (
                match String.drop_prefix ~prefix:"Bearer " x with
                | None -> unauthorized
                | Some x -> cont x)
          in
          let@ params cont =
            let* p = Storage.C.get s Credentials_params in
            match Lopt.get_value p with Some p -> cont p | None -> not_found
          in
          if check_seed ~params ~seed then
            let* p = Storage.C.get s Credentials_credits in
            match Lopt.get_string p with
            | Some p -> return_json 200 p
            | None -> not_found
          else unauthorized
      | _ -> method_not_allowed)
  | [ "belenios" ] -> (
      match method_ with
      | `POST -> (
          let@ response_s = body.run Fun.id in
          let response =
            !*(credentials_response_of_yojson Fun.id Fun.id) response_s
          in
          let certificate = response.certificate in
          let uuid = certificate.message.uuid in
          let@ s = Storage.E.with_transaction uuid in
          let@ () = handle_generic_error in
          let@ se, set = Storage.E.update s Draft in
          match Lopt.get_value se with
          | None -> not_found
          | Some (W (w, (Draft (_, se) as x))) ->
              let module G = (val w) in
              let response =
                !*[%group_of_yojson: _ credentials_response] response_s
              in
              if se.public_creds = response.token then
                let set ?billing:_ x = set Value (W (w, x)) in
                let certificate = response.certificate in
                let* () =
                  Api_drafts.submit_public_credentials s w (x, set) ~certificate
                    response.public_credentials
                in
                ok
              else forbidden)
      | _ -> method_not_allowed)
  | _ -> not_found
