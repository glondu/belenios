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
open Belenios_core
open Common
open Belenios_api.Serializable_j
open Api_generic

let ( let& ) = Option.bind

module Make () = struct
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
    let* code, response =
      match endpoint with
      | [ "configuration" ] -> (
          match method_ with
          | `GET ->
              let x = Api_generic.get_configuration () in
              Lwt.return (200, string_of_configuration x)
          | _ -> method_not_allowed)
      | [ "account" ] -> (
          let@ token = Option.unwrap unauthorized token in
          let@ account = Option.unwrap unauthorized (lookup_token token) in
          let get () =
            let x = Api_generic.get_account account in
            Lwt.return @@ string_of_api_account x
          in
          match method_ with
          | `GET -> handle_get get
          | `PUT ->
              let@ () = handle_ifmatch ifmatch get in
              let@ x = body.run api_account_of_string in
              let@ () = handle_generic_error in
              let* () = Api_generic.put_account account x in
              ok
          | _ -> method_not_allowed)
      | "drafts" :: endpoint ->
          Api_drafts.dispatch ~token ~ifmatch endpoint method_ body
      | "elections" :: endpoint ->
          Api_elections.dispatch ~token ~ifmatch endpoint method_ body
      | _ -> not_found
    in
    Eliom_registration.String.send ~code (response, "application/json")

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

  open Eliom_registration.Any

  let () =
    register ~service:api_get (fun (endpoint, params) () ->
        dispatch endpoint `GET params None)

  let () =
    register ~service:api_post (fun (endpoint, params) x ->
        dispatch endpoint `POST params (Some x))

  let () =
    register ~service:api_put (fun (endpoint, params) x ->
        dispatch endpoint `PUT params (Some x))

  let () =
    register ~service:api_delete (fun (endpoint, params) x ->
        dispatch endpoint `DELETE params (Some x))
end
