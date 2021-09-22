(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Belenios_core
open Serializable_builtin_t
open Common
open Belenios_api.Serializable_j
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common

let ( let& ) = Option.bind

type token = {
    expiration : datetime;
    account : account;
  }

module Make (Web_services : Web_services_sig.S) (Pages_voter : Pages_voter_sig.S) = struct

  let tokens = ref SMap.empty

  let new_token account =
    let* token = generate_token ~length:22 () in
    let expiration = datetime_add (now ()) (day 1) in
    tokens := SMap.add token {expiration; account} !tokens;
    Lwt.return token

  let filter tokens =
    let now = now () in
    SMap.filter (fun _ {expiration; _} -> datetime_compare now expiration < 0) tokens

  let lookup_token token =
    tokens := filter !tokens;
    let& {account; _} = SMap.find_opt token !tokens in
    Some account

  let invalidate_token token =
    tokens := SMap.remove token !tokens

  let () =
    let@ a = Accounts.add_update_hook in
    let f {expiration; account} =
      let account = if a.account_id = account.account_id then a else account in
      {expiration; account}
    in
    tokens := SMap.map f !tokens;
    Lwt.return_unit

  let dispatch endpoint method_ _params body =
    let token =
      let sp = Eliom_common.get_sp () in
      let& x = Ocsigen_request.header sp.Eliom_common.sp_request.request_info (Ocsigen_header.Name.of_string "Authorization") in
      String.drop_prefix ~prefix:"Bearer " x
    in
    let* code, response =
      let ok = Lwt.return (200, "{}") in
      let bad_request = Lwt.return (400, "\"Bad Request\"") in
      let unauthorized = Lwt.return (401, "\"Unauthorized\"") in
      let forbidden = Lwt.return (403, "\"Forbidden\"") in
      let not_found = Lwt.return (404, "\"Not Found\"") in
      let method_not_allowed = Lwt.return (405, "\"Method Not Allowed\"") in
      let with_body of_string f =
        let@ _, body = Option.unwrap bad_request body in
        let* x = Cohttp_lwt.Body.to_string body in
        let@ x = Option.unwrap bad_request (Option.wrap of_string x) in
        f x
      in
      let handle_generic_error f =
        Lwt.catch f
          (function
           | Api_generic.Error msg ->
              let json =
                `Assoc [
                    "status", `String "Bad Request";
                    "error", `String msg;
                  ]
              in
              Lwt.return (400, Yojson.Safe.to_string json)
           | _ -> bad_request
          )
      in
      let with_administrator se f =
        let@ token = Option.unwrap unauthorized token in
        match lookup_token token with
        | Some a when Accounts.check_account a se.se_owner -> f a
        | _ -> not_found
      in
      let with_administrator_or_credential_authority se f =
        let@ token = Option.unwrap unauthorized token in
        if token = se.se_public_creds then (
          f `CredentialAuthority
        ) else (
          match lookup_token token with
          | Some a when Accounts.check_account a se.se_owner -> f (`Administrator a)
          | _ -> not_found
        )
      in
      match endpoint with
      | ["configuration"] ->
         begin
           match method_ with
           | `GET ->
              let x = Api_generic.get_configuration () in
              Lwt.return (200, string_of_configuration x)
           | _ -> method_not_allowed
         end
      | ["account"] ->
         begin
           let@ token = Option.unwrap unauthorized token in
           let@ account = Option.unwrap unauthorized (lookup_token token) in
           match method_ with
           | `GET ->
              let x = Api_generic.get_account account in
              Lwt.return (200, string_of_api_account x)
           | `PUT ->
              let@ x = with_body api_account_of_string in
              let@ () = handle_generic_error in
              let* () = Api_generic.put_account account x in
              ok
           | _ -> method_not_allowed
         end
      | ["drafts"] ->
         begin
           let@ token = Option.unwrap unauthorized token in
           let@ account = Option.unwrap unauthorized (lookup_token token) in
           match method_ with
           | `GET ->
              let* elections = Web_persist.get_elections_by_owner account.account_id in
              let elections =
                List.fold_left
                  (fun accu (kind, summary_uuid, date, summary_name) ->
                    let summary_date = unixfloat_of_datetime date in
                    if kind = `Draft then
                      {summary_uuid; summary_name; summary_date} :: accu
                    else
                      accu
                  ) [] elections
              in
              Lwt.return (200, string_of_summary_list elections)
           | `POST ->
              let@ draft = with_body draft_of_string in
              let@ () = handle_generic_error in
              let* uuid = Api_drafts.post_drafts account draft in
              Lwt.return (200, string_of_uuid uuid)
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ who = with_administrator_or_credential_authority se in
           match method_, who with
           | `GET, _ ->
              let@ () = handle_generic_error in
              let x = Api_drafts.api_of_draft se in
              Lwt.return (200, string_of_draft x)
           | `PUT, `Administrator _ ->
              let@ draft = with_body draft_of_string in
              let@ () = handle_generic_error in
              let update_cache = draft.draft_questions.t_name <> se.se_questions.t_name in
              let se = Api_drafts.draft_of_api se draft in
              let* () = Web_persist.set_draft_election uuid se in
              let* () =
                if update_cache then
                  Web_persist.clear_elections_by_owner_cache ()
                else
                  Lwt.return_unit
              in
              ok
           | `DELETE, `Administrator _ ->
              let@ () = handle_generic_error in
              let* () = Api_drafts.delete_draft uuid in
              ok
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid; "voters"] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ who = with_administrator_or_credential_authority se in
           match method_, who with
           | `GET, _ ->
              let@ () = handle_generic_error in
              let x = Api_drafts.get_draft_voters se in
              Lwt.return (200, string_of_voter_list x)
           | `PUT, `Administrator _ ->
              if se.se_public_creds_received then (
                forbidden
              ) else (
                let@ voters = with_body voter_list_of_string in
                let@ () = handle_generic_error in
                let* () = Api_drafts.put_draft_voters uuid se voters in
                ok
              )
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid; "passwords"] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ _ = with_administrator se in
           match method_ with
           | `GET ->
              let@ () = handle_generic_error in
              let x = Api_drafts.get_draft_passwords se in
              Lwt.return (200, string_of_voter_list x)
           | `POST ->
              let@ voters = with_body voter_list_of_string in
              let@ () = handle_generic_error in
              let generate =
                let title = se.se_questions.t_name in
                let url =
                  Eliom_uri.make_string_uri ~absolute:true ~service:Web_services.election_home (uuid, ())
                  |> rewrite_prefix
                in
                let langs = get_languages se.se_metadata.e_languages in
                let show_weight =
                  List.exists
                    (fun id ->
                      let _, _, weight = split_identity_opt id.sv_id in
                      weight <> None
                    ) se.se_voters
                in
                fun metadata id ->
                Pages_voter.generate_password metadata langs title uuid url id show_weight
              in
              let* () = Api_drafts.post_draft_passwords generate uuid se voters in
              ok
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid; "credentials"] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ who = with_administrator_or_credential_authority se in
           match method_ with
           | `GET ->
              let@ () = handle_generic_error in
              let* x = Api_drafts.get_draft_credentials who uuid se in
              Lwt.return (200, string_of_credentials x)
           | `POST ->
              if se.se_public_creds_received then (
                forbidden
              ) else (
                let@ x = with_body credential_list_of_string in
                match who, x with
                | `Administrator _, [] ->
                   begin
                     let@ () = handle_generic_error in
                     let send = Pages_voter.send_mail_credential uuid se in
                     let* x = Api_drafts.generate_credentials_on_server send uuid se in
                     match x with
                     | Ok () -> ok
                     | Error e -> Lwt.fail @@ Api_drafts.exn_of_generate_credentials_on_server_error e
                   end
                | `CredentialAuthority, credentials ->
                   let@ () = handle_generic_error in
                   let* () = Api_drafts.submit_public_credentials uuid se credentials in
                   ok
                | _ -> forbidden
              )
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid; "trustees-mode"] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ _ = with_administrator se in
           match method_ with
           | `GET ->
              let@ () = handle_generic_error in
              let x = Api_drafts.get_draft_trustees_mode se in
              Lwt.return (200, string_of_trustees_mode x)
           | `PUT ->
              let@ mode = with_body trustees_mode_of_string in
              let@ () = handle_generic_error in
              let* () = Api_drafts.put_draft_trustees_mode uuid se mode in
              ok
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid; "trustees"] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ _ = with_administrator se in
           match method_ with
           | `GET ->
              let@ () = handle_generic_error in
              let x = Api_drafts.get_draft_trustees se in
              Lwt.return (200, string_of_trustees x)
           | `POST ->
              let@ trustee = with_body trustee_of_string in
              let@ () = handle_generic_error in
              let* () = Api_drafts.post_draft_trustees uuid se trustee in
              ok
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid; "trustees"; trustee] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ _ = with_administrator se in
           match method_ with
           | `DELETE ->
              let@ () = handle_generic_error in
              let* x = Api_drafts.delete_draft_trustee uuid se trustee in
              if x then ok else not_found
           | _ -> method_not_allowed
         end
      | ["drafts"; uuid; "status"] ->
         begin
           let@ uuid = Option.unwrap bad_request (Option.wrap uuid_of_raw_string uuid) in
           let* se = Web_persist.get_draft_election uuid in
           let@ se = Option.unwrap not_found se in
           let@ account = with_administrator se in
           match method_ with
           | `GET ->
              let@ () = handle_generic_error in
              let* x = Api_drafts.get_draft_status uuid se in
              Lwt.return (200, string_of_status x)
           | `POST ->
              let@ x = with_body status_request_of_string in
              let@ () = handle_generic_error in
              let* () = Api_drafts.post_draft_status account uuid se x in
              ok
           | _ -> method_not_allowed
         end
      | _ -> not_found
    in
    Eliom_registration.String.send ~code (response, "application/json")

  open Eliom_service
  open Eliom_parameter

  let api_get = create ~path:(Path ["api"]) ~meth:(Get (suffix_prod (all_suffix "endpoint") any)) ()
  let api_post = create ~path:(Path ["api"]) ~meth:(Post (suffix_prod (all_suffix "endpoint") any, raw_post_data)) ()
  let api_put = create ~path:(Path ["api"]) ~meth:(Put (suffix_prod (all_suffix "endpoint") any)) ()
  let api_delete = create ~path:(Path ["api"]) ~meth:(Delete (suffix_prod (all_suffix "endpoint") any)) ()

  open Eliom_registration.Any

  let () = register ~service:api_get (fun (endpoint, params) () -> dispatch endpoint `GET params None)
  let () = register ~service:api_post (fun (endpoint, params) x -> dispatch endpoint `POST params (Some x))
  let () = register ~service:api_put (fun (endpoint, params) x -> dispatch endpoint `PUT params (Some x))
  let () = register ~service:api_delete (fun (endpoint, params) x -> dispatch endpoint `DELETE params (Some x))

end
