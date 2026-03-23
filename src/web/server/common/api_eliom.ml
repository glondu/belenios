(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

let ( let& ) = Option.bind

[%%import "../../../platform/config.mlh"]
[%%if ocsigenserver_version < 7]

let return_response x =
  let* response, body = x in
  Lwt.return @@ Ocsigen_response.make ~body response

[%%else]

let return_response x =
  let* response, body = x in
  let body = Ocsigen_response.Body.of_cohttp ~encoding:Chunked body in
  Lwt.return @@ Ocsigen_response.make ~body response

[%%endif]

module Api_result = Eliom_mkreg.Make (struct
  type page = Api_generic.result
  type options = unit
  type result = Ocsigen_response.t

  let result_of_http_result = Fun.id
  let send_appl_content = Eliom_service.XNever

  let send ?options:_ ?charset:_ ?code:_ ?content_type:_ ?headers:_ response =
    let headers = Cohttp.Header.of_list [ ("cache-control", "no-cache") ] in
    match response with
    | `Json (code, body) ->
        let status = `Code code in
        let headers =
          Cohttp.Header.add headers "content-type" "application/json"
        in
        Cohttp_lwt_unix.Server.respond_string ~headers ~status ~body ()
        |> return_response
    | `Bel fname ->
        let headers =
          Cohttp.Header.add headers "content-type" "application/x-belenios"
        in
        Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()
        |> return_response
    | `Sealing_log fname ->
        let headers = Cohttp.Header.add headers "content-type" "text/plain" in
        Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()
        |> return_response
    | `Generic x ->
        let headers = Cohttp.Header.add headers "content-type" x.mime in
        Cohttp_lwt_unix.Server.respond_string ~headers ~body:x.content
          ~status:`OK ()
        |> return_response
end)

module Make () = struct
  (* Forward references set in Web_main *)
  let get_result = ref (fun ~state:_ -> None)
  let get_dispatch = ref (fun _ -> None)

  let dispatch endpoint method_ _params body =
    let sp = Eliom_common.get_sp () in
    let token =
      let& x =
        Ocsigen_request.header sp.Eliom_common.sp_request.request_info
          (Ocsigen_header.Name.of_string "Authorization")
      in
      String.drop_prefix ~prefix:"Bearer " x
    in
    let ifmatch =
      Ocsigen_request.header sp.Eliom_common.sp_request.request_info
        (Ocsigen_header.Name.of_string "If-Match")
    in
    let body =
      {
        run =
          (fun of_string f ->
            let@ _, body = Option.unwrap bad_request body in
            let* x = Cohttp_lwt.Body.to_string body in
            let@ x = Option.unwrap bad_request (Option.wrap of_string x) in
            f x);
      }
    in
    match endpoint with
    | [ "configuration" ] -> (
        match method_ with
        | `GET ->
            let x = Api_generic.get_configuration () in
            return_json 200 (string_of_configuration x)
        | _ -> method_not_allowed)
    | [ "send-message"; kind ] -> (
        let@ internal =
         fun cont ->
          match kind with
          | "internal" -> cont (Some true)
          | "external" -> cont (Some false)
          | "default" -> cont None
          | _ -> not_found
        in
        match (method_, !Web_config.internal_send_message) with
        | `POST, Some key ->
            let@ x = body.run Belenios_messages.message_payload_of_string in
            let@ () = handle_generic_error in
            Api_generic.post_send_message ?internal ~key x
        | _ -> method_not_allowed)
    | [ "account" ] -> (
        let@ token = Option.unwrap unauthorized token in
        let@ account, user = Option.unwrap unauthorized (lookup_token token) in
        let get () =
          let x = Api_generic.get_account account user in
          Lwt.return @@ string_of_api_account x
        in
        match method_ with
        | `GET -> handle_get get
        | `PUT ->
            let@ () = handle_ifmatch ifmatch get in
            let@ x = body.run api_account_of_string in
            let@ () = handle_generic_error in
            let@ s = Storage.A.with_transaction in
            let@ account, set = Accounts.update_account_by_id s account.id in
            let@ account =
              Option.unwrap unauthorized (Lopt.get_value account)
            in
            let* () = Api_generic.put_account (account, set) user x in
            ok
        | _ -> method_not_allowed)
    | "elections" :: endpoint ->
        Api_elections.dispatch ~token ~ifmatch endpoint method_ body
    | [ "cast-result"; state ] -> (
        match method_ with
        | `GET -> (
            match !get_result ~state with
            | None -> not_found
            | Some result ->
                return_json 200 (Belenios_web_api.string_of_cast_result result))
        | _ -> method_not_allowed)
    | "credentials" :: endpoint ->
        Api_credentials.dispatch endpoint method_ body
    | "billing" :: endpoint ->
        Billing.dispatch ~token ~ifmatch endpoint method_ body
    | "login" :: service :: endpoint -> (
        let rec find_instance xs cont =
          match xs with
          | [] -> not_found
          | (x : auth_config) :: _ when x.auth_instance = service -> cont x
          | _ :: xs -> find_instance xs cont
        in
        let@ a = find_instance !Web_config.site_auth_config in
        match !get_dispatch a.auth_system with
        | None -> not_found
        | Some dispatch -> dispatch a endpoint method_ body)
    | [ "login" ] -> (
        match method_ with
        | `DELETE ->
            let@ token = Option.unwrap unauthorized token in
            invalidate_token token;
            ok
        | _ -> method_not_allowed)
    | _ -> not_found

  open Eliom_service
  open Eliom_parameter

  let api_get =
    create ~path:(Path [ "api" ])
      ~meth:(Get (suffix_prod (all_suffix "endpoint") any))
      ()

  let api_post =
    create ~path:(Path [ "api" ])
      ~meth:(Post (suffix_prod (all_suffix "endpoint") any, raw_post_data))
      ()

  let api_put =
    create ~path:(Path [ "api" ])
      ~meth:(Put (suffix_prod (all_suffix "endpoint") any))
      ()

  let api_delete =
    create ~path:(Path [ "api" ])
      ~meth:(Delete (suffix_prod (all_suffix "endpoint") any))
      ()

  let () =
    Api_result.register ~service:api_get (fun (endpoint, params) () ->
        dispatch endpoint `GET params None)

  let () =
    Api_result.register ~service:api_post (fun (endpoint, params) x ->
        dispatch endpoint `POST params (Some x))

  let () =
    Api_result.register ~service:api_put (fun (endpoint, params) x ->
        dispatch endpoint `PUT params (Some x))

  let () =
    Api_result.register ~service:api_delete (fun (endpoint, params) x ->
        dispatch endpoint `DELETE params (Some x))
end
