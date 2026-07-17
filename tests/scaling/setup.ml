(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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
open Question.Homomorphic
open Belenios_web_api
open Common

module type PARAMS = sig
  val prefix : string
  val nb_candidates : int
  val nb_voters : int
  val admin_id : int
  val api_token : string
  val nh : bool
  val validate : bool
end

module Make (P : PARAMS) = struct
  open P

  let api_root = prefix ^ "api"
  let group = "Ed25519"
  let version = 2

  let generate_candidates n =
    Array.init n (fun i -> Printf.sprintf "Candidate %d" (i + 1))

  let questions =
    let open Question in
    if nh then
      let value : Non_homomorphic.question =
        {
          answers = generate_candidates nb_candidates;
          question = "Rank the candidates";
        }
      in
      [| Q { type_ = (module Non_homomorphic); value; extra = None } |]
    else
      let value : Homomorphic.question =
        {
          answers = generate_candidates nb_candidates;
          blank = false;
          min = 1;
          max = 1;
          question = "Which candidate do you prefer?";
        }
      in
      [| Q { type_ = (module Homomorphic); value; extra = None } |]

  let questions =
    {
      description = "This is a big election to test scaling capabilities.";
      name = "Scaling Test Election";
      questions;
      administrator = "Election Initiator";
      credential_authority = `External "Election Registrar";
      language = Some ("en", `Ltr);
    }

  let draft =
    {
      version;
      owners = [ admin_id ];
      questions;
      languages = [ "en" ];
      contact = Some "Election Initiator <election.initiator@example.org>";
      booth = 2;
      authentication = Some (`Configured "demo");
      group;
      cred_authority_info = None;
    }

  let headers =
    [ ("Authorization", Printf.sprintf "Bearer %s" api_token) ]
    |> Cohttp.Header.of_list

  let create to_yojson endpoint x =
    let body = !+to_yojson x |> Cohttp_lwt.Body.of_string in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~headers ~body
        (Printf.ksprintf Uri.of_string endpoint api_root)
    in
    let* x = Cohttp_lwt.Body.to_string x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> (
        match Json.of_string x with
        | `String uuid -> Lwt.return (Uuid.of_string uuid)
        | _ | (exception _) ->
            Printf.ksprintf failwith "unexpected body in create(%s)"
              (string_of_format endpoint))
    | code ->
        Printf.ksprintf failwith "unexpected status %d in create(%s)" code
          (string_of_format endpoint)

  let create_draft () = create yojson_of_raw_draft "%s/elections" draft

  let create_trustees () =
    create yojson_of_group_specification "%s/trustees" { version; group }

  let setup_voters uuid nb_voters =
    let nb_length = String.length (string_of_int nb_voters) in
    let rec loop n accu =
      if n > 0 then
        let login = Printf.sprintf "voter%0*d@example.org" nb_length n in
        let v : voter = { address = Some login; login; weight = None } in
        loop (n - 1) (v :: accu)
      else accu
    in
    let voters = loop nb_voters [] in
    let body = voters |> !+yojson_of_voter_list |> Cohttp_lwt.Body.of_string in
    let* response, x =
      Cohttp_lwt_unix.Client.put ~headers ~body
        (Printf.ksprintf Uri.of_string "%s/elections/%s/draft/voters" api_root
           (Uuid.to_string uuid))
    in
    let* () = Cohttp_lwt.Body.drain_body x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return voters
    | code ->
        Printf.ksprintf failwith "unexpected status %d in setup_voters" code

  let get_credentials_token uuid =
    let* response, x =
      Cohttp_lwt_unix.Client.get ~headers
        (Printf.ksprintf Uri.of_string "%s/elections/%s/draft/credentials/token"
           api_root (Uuid.to_string uuid))
    in
    let* x = Cohttp_lwt.Body.to_string x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return x
    | code ->
        Printf.ksprintf failwith "unexpected status %d in get_credentials_token"
          code

  let setup_credentials uuid voters =
    let* token = get_credentials_token uuid in
    let module G = (val Belenios.Group.make { version; group }) in
    let module Cred =
      Credential.Make
        (G)
        (struct
          type 'a t = 'a Lwt.t

          let return = Lwt.return
          let bind = Lwt.bind
          let pause = Lwt.pause
          let uuid = uuid
        end) in
    let* x = Cred.generate voters in
    let private_creds = x.private_creds in
    let headers =
      [ ("Authorization", Printf.sprintf "Bearer %s" token) ]
      |> Cohttp.Header.of_list
    in
    let body =
      x.public_with_ids
      |> !+(yojson_of_public_credentials_with_id !&G.to_string)
      |> Cohttp_lwt.Body.of_string
    in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~headers ~body
        (Printf.ksprintf Uri.of_string
           "%s/elections/%s/draft/credentials/public" api_root
           (Uuid.to_string uuid))
    in
    let* () = Cohttp_lwt.Body.drain_body x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return private_creds
    | code ->
        Printf.ksprintf failwith "unexpected status %d in setup_credentials"
          code

  let send_request to_yojson endpoint uuid r =
    let body = r |> !+to_yojson |> Cohttp_lwt.Body.of_string in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~headers ~body
        (Printf.ksprintf Uri.of_string endpoint api_root (Uuid.to_string uuid))
    in
    let* () = Cohttp_lwt.Body.drain_body x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return_unit
    | code ->
        Printf.ksprintf failwith "unexpected status %d on send_request(%s)" code
          (string_of_format endpoint)

  let validate_election uuid =
    let* trustees = create_trustees () in
    let* () =
      send_request yojson_of_trustees_request "%s/trustees/%s" trustees
        `Validate
    in
    let* () =
      send_request yojson_of_draft_request "%s/elections/%s/draft" uuid
        (`SetTrustees (Some trustees))
    in
    let* () =
      send_request yojson_of_draft_request "%s/elections/%s/draft" uuid
        `ValidateElection
    in
    let* () =
      send_request yojson_of_admin_request "%s/elections/%s" uuid `Open
    in
    Lwt.return_unit

  let main () =
    let* uuid = create_draft () in
    print_endline (Uuid.to_string uuid);
    let* voters = setup_voters uuid nb_voters in
    let* private_creds = setup_credentials uuid voters in
    let* () =
      send_request yojson_of_draft_request "%s/elections/%s/draft" uuid
        `SetCredentialAuthorityVisited
    in
    let* () =
      let open Lwt_io in
      let@ f =
        with_file ~mode:output
          (Printf.sprintf "scaling.%s.privcreds.json" (Uuid.to_string uuid))
      in
      write f (!+yojson_of_private_credentials private_creds)
    in
    if validate then validate_election uuid else Lwt.return_unit
end

let perform_login ~prefix ~username =
  let@ token cont =
    let body =
      { username } |> !+yojson_of_auth_dummy_info |> Cohttp_lwt.Body.of_string
    in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~body
        (Printf.ksprintf Uri.of_string "%sapi/login/admin/demo" prefix)
    in
    let* x = Cohttp_lwt.Body.to_string x in
    let { token; _ } = !*auth_token_of_yojson x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> cont token
    | code ->
        Printf.ksprintf failwith "unexpected status %d in perform_login" code
  in
  let@ id cont =
    let headers =
      [ ("Authorization", Printf.sprintf "Bearer %s" token) ]
      |> Cohttp.Header.of_list
    in
    let* response, x =
      Cohttp_lwt_unix.Client.get ~headers
        (Printf.ksprintf Uri.of_string "%sapi/account" prefix)
    in
    let* x = Cohttp_lwt.Body.to_string x in
    let { id; _ } = !*api_account_of_yojson x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> cont id
    | code ->
        Printf.ksprintf failwith
          "unexpected status %d when retrieving account id" code
  in
  Lwt.return (token, id)

open Cmdliner

let candidates_t =
  let doc = "Use $(docv) candidates." in
  Arg.(value & opt int 5 & info [ "candidates" ] ~docv:"CANDIDATES" ~doc)

let voters_t =
  let doc = "Use $(docv) voters." in
  Arg.(value & opt int 1000 & info [ "voters" ] ~docv:"VOTERS" ~doc)

let admin_login_t =
  let doc = "Use $(docv) as admin login for dummy authentication." in
  Arg.(
    value
    & opt (some string) None
    & info [ "admin-login" ] ~docv:"ADMIN-LOGIN" ~doc)

let nh_t =
  let doc = "Use a non-homomorphic question." in
  Arg.(value & flag & info [ "nh-question" ] ~doc)

let validate_t =
  let doc = "Validate the election." in
  Arg.(value & flag & info [ "validate" ] ~doc)

let main url candidates voters admin_login nh validate =
  let@ () = wrap_main in
  let prefix = get_mandatory "url" url in
  let username = get_mandatory "admin-login" admin_login in
  let@ () = fun cont -> Lwt_main.run @@ cont () in
  let* api_token, admin_id = perform_login ~prefix ~username in
  let module X = Make (struct
    let prefix = prefix
    let nb_candidates = candidates
    let nb_voters = voters
    let admin_id = admin_id
    let api_token = api_token
    let nh = nh
    let validate = validate
  end) in
  X.main ()

let cmd =
  let doc = "set up a scaling test election" in
  let man = [] in
  Cmd.v
    (Cmd.info "setup" ~doc ~man)
    Term.(
      ret
        (const main $ url_t $ candidates_t $ voters_t $ admin_login_t $ nh_t
       $ validate_t))
