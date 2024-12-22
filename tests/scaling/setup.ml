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
open Belenios_question.Homomorphic.Syntax
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
  let version = 1

  let generate_candidates n =
    Array.init n (fun i -> Printf.sprintf "Candidate %d" (i + 1))

  let t_questions =
    if nh then
      let open Belenios_question.Non_homomorphic in
      let value : t =
        {
          q_answers = generate_candidates nb_candidates;
          q_question = "Rank the candidates";
        }
      in
      [| make ~value ~extra:None |]
    else
      let open Belenios_question.Homomorphic in
      let value : t =
        {
          q_answers = generate_candidates nb_candidates;
          q_blank = None;
          q_min = 1;
          q_max = 1;
          q_question = "Which candidate do you prefer?";
        }
      in
      [| make ~value ~extra:None |]

  let draft_questions =
    {
      t_description = "This is a big election to test scaling capabilities.";
      t_name = "Scaling Test Election";
      t_questions;
      t_administrator = Some "Election Initiator";
      t_credential_authority = Some "Election Registrar";
    }

  let draft =
    {
      draft_version = version;
      draft_owners = [ admin_id ];
      draft_questions;
      draft_languages = [ "en" ];
      draft_contact = Some "Election Initiator <election.initiator@example.org>";
      draft_booth = 2;
      draft_authentication = `Configured "demo";
      draft_group = group;
    }

  let headers =
    [ ("Authorization", Printf.sprintf "Bearer %s" api_token) ]
    |> Cohttp.Header.of_list

  let create_draft () =
    let body =
      write_raw_draft Belenios_question.write_question -- draft
      |> Cohttp_lwt.Body.of_string
    in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~headers ~body
        (Printf.ksprintf Uri.of_string "%s/drafts" api_root)
    in
    let* x = Cohttp_lwt.Body.to_string x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> (
        match Yojson.Safe.from_string x with
        | `String uuid -> Lwt.return (Uuid.wrap uuid)
        | _ | (exception _) -> failwith "unexpected body in create_draft")
    | code ->
        Printf.ksprintf failwith "unexpected status %d in create_draft" code

  let setup_voters uuid nb_voters =
    let nb_length = String.length (string_of_int nb_voters) in
    let rec loop n accu =
      if n > 0 then
        loop (n - 1) (Printf.sprintf "voter%0*d@example.org" nb_length n :: accu)
      else accu
    in
    let voters = loop nb_voters [] |> List.map Voter.of_string in
    let body = voters |> string_of_voter_list |> Cohttp_lwt.Body.of_string in
    let* response, x =
      Cohttp_lwt_unix.Client.put ~headers ~body
        (Printf.ksprintf Uri.of_string "%s/drafts/%s/voters" api_root
           (Uuid.unwrap uuid))
    in
    let* () = Cohttp_lwt.Body.drain_body x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return voters
    | code ->
        Printf.ksprintf failwith "unexpected status %d in setup_voters" code

  let get_credentials_token uuid =
    let* response, x =
      Cohttp_lwt_unix.Client.get ~headers
        (Printf.ksprintf Uri.of_string "%s/drafts/%s/credentials/token" api_root
           (Uuid.unwrap uuid))
    in
    let* x = Cohttp_lwt.Body.to_string x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return x
    | code ->
        Printf.ksprintf failwith "unexpected status %d in get_credentials_token"
          code

  let setup_credentials uuid voters =
    let* token = get_credentials_token uuid in
    let module G = (val Belenios.Group.of_string ~version group) in
    let module Cred =
      Credential.Make
        (G)
        (struct
          type 'a t = 'a Lwt.t

          let return = Lwt.return
          let bind = Lwt.bind
          let pause = Lwt.pause
          let uuid = uuid
        end)
    in
    let* x = Cred.generate voters in
    let private_creds = x.private_creds in
    let headers =
      [ ("Authorization", Printf.sprintf "Bearer %s" token) ]
      |> Cohttp.Header.of_list
    in
    let body =
      x.public_with_ids |> string_of_public_credentials
      |> Cohttp_lwt.Body.of_string
    in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~headers ~body
        (Printf.ksprintf Uri.of_string "%s/drafts/%s/credentials/public"
           api_root (Uuid.unwrap uuid))
    in
    let* () = Cohttp_lwt.Body.drain_body x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return private_creds
    | code ->
        Printf.ksprintf failwith "unexpected status %d in setup_credentials"
          code

  let validate_election uuid =
    let body =
      `ValidateElection |> string_of_draft_request |> Cohttp_lwt.Body.of_string
    in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~headers ~body
        (Printf.ksprintf Uri.of_string "%s/drafts/%s" api_root
           (Uuid.unwrap uuid))
    in
    let* () = Cohttp_lwt.Body.drain_body x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return_unit
    | code ->
        Printf.ksprintf failwith "unexpected status %d in validate_election"
          code

  let main () =
    let* uuid = create_draft () in
    print_endline (Uuid.unwrap uuid);
    let* voters = setup_voters uuid nb_voters in
    let* private_creds = setup_credentials uuid voters in
    let* () =
      let open Lwt_io in
      let@ f =
        with_file ~mode:output
          (Printf.sprintf "scaling.%s.privcreds.json" (Uuid.unwrap uuid))
      in
      write f (string_of_private_credentials private_creds)
    in
    if validate then validate_election uuid else Lwt.return_unit
end

open Cmdliner

let candidates_t =
  let doc = "Use $(docv) candidates." in
  Arg.(value & opt int 5 & info [ "candidates" ] ~docv:"CANDIDATES" ~doc)

let voters_t =
  let doc = "Use $(docv) voters." in
  Arg.(value & opt int 1000 & info [ "voters" ] ~docv:"VOTERS" ~doc)

let admin_id_t =
  let doc = "Use admin account id $(docv)." in
  Arg.(value & opt (some int) None & info [ "admin-id" ] ~docv:"ADMIN-ID" ~doc)

let api_token_t =
  let doc = "Use API token $(docv)." in
  Arg.(
    value & opt (some string) None & info [ "api-token" ] ~docv:"API-TOKEN" ~doc)

let nh_t =
  let doc = "Use a non-homomorphic question." in
  Arg.(value & flag & info [ "nh-question" ] ~doc)

let validate_t =
  let doc = "Validate the election." in
  Arg.(value & flag & info [ "validate" ] ~doc)

let main url candidates voters admin_id api_token nh validate =
  let@ () = wrap_main in
  let module X = Make (struct
    let prefix = get_mandatory "url" url
    let nb_candidates = candidates
    let nb_voters = voters
    let admin_id = get_mandatory "admin-id" admin_id
    let api_token = get_mandatory "api-token" api_token
    let nh = nh
    let validate = validate
  end) in
  Lwt_main.run (X.main ())

let cmd =
  let doc = "set up a scaling test election" in
  let man = [] in
  Cmd.v
    (Cmd.info "setup" ~doc ~man)
    Term.(
      ret
        (const main $ url_t $ candidates_t $ voters_t $ admin_id_t $ api_token_t
       $ nh_t $ validate_t))
