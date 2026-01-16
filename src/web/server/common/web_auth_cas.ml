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

open Lwt
open Lwt.Syntax
open Eliom_service
open Belenios
open Belenios_storage_api
open Web_common

module Make (Web_services : Web_services_sig.S) (Web_auth : Web_auth_sig.S) =
struct
  let next_lf str i = String.index_from_opt str i '\n'

  let login_cas =
    Eliom_service.create
      ~path:(Eliom_service.Path [ "auth"; "cas" ])
      ~meth:
        (Eliom_service.Get
           Eliom_parameter.(string "state" ** opt (string "ticket")))
      ()

  let cas_self ~state =
    Web_services.make_absolute_string_uri
      ~service:(preapply ~service:login_cas (state, None))
      ()

  let extract tag xs =
    let rec loop = function
      | [] -> None
      | x :: xs -> (
          match x with
          | Xml.Element (tag', _, children) when tag = tag' -> Some children
          | _ -> loop xs)
    in
    loop xs

  let extract_pcdata = function [ Xml.PCData x ] -> Some x | _ -> None

  let parse_cas_validation_v2 info =
    let ( >>= ) = Option.bind and ( let* ) = Option.bind in
    try
      let* info =
        Some [ Xml.parse_string info ]
        >>= extract "cas:serviceResponse"
        >>= extract "cas:authenticationSuccess"
      in
      let* login = extract "cas:user" info >>= extract_pcdata in
      let address =
        extract "cas:attributes" info >>= extract "cas:mail" >>= extract_pcdata
      in
      let info : Belenios_web_api.user_info =
        { login; name = None; address; timestamp = None }
      in
      Some info
    with _ -> None

  let get_cas_validation_v2 server ~state ticket =
    let url =
      let cas_validate =
        Eliom_service.extern ~prefix:server ~path:[ "serviceValidate" ]
          ~meth:
            (Eliom_service.Get
               Eliom_parameter.(string "service" ** string "ticket"))
          ()
      in
      let service = preapply ~service:cas_validate (cas_self ~state, ticket) in
      Eliom_uri.make_string_uri ~absolute:true ~service ()
    in
    let* r, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    if Cohttp.(Code.code_of_status (Response.status r)) = 200 then
      let* info = Cohttp_lwt.Body.to_string body in
      return @@ parse_cas_validation_v2 info
    else return_none

  let parse_cas_validation_v1 info =
    match next_lf info 0 with
    | Some i -> (
        match String.sub info 0 i with
        | "yes" ->
            let x =
              let& j = next_lf info (i + 1) in
              let info : Belenios_web_api.user_info =
                {
                  login = String.sub info (i + 1) (j - i - 1);
                  name = None;
                  address = None;
                  timestamp = None;
                }
              in
              Some info
            in
            `Yes x
        | "no" -> `No
        | _ -> `Error `Parsing)
    | None -> `Error `Parsing

  let get_cas_validation_v1 server ~state ticket =
    let url =
      let cas_validate =
        Eliom_service.extern ~prefix:server ~path:[ "validate" ]
          ~meth:
            (Eliom_service.Get
               Eliom_parameter.(string "service" ** string "ticket"))
          ()
      in
      let service = preapply ~service:cas_validate (cas_self ~state, ticket) in
      Eliom_uri.make_string_uri ~absolute:true ~service ()
    in
    let* _, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    let* info = Cohttp_lwt.Body.to_string body in
    return (parse_cas_validation_v1 info)

  let get_cas_validation server ~state ticket =
    let* v2 = get_cas_validation_v2 server ~state ticket in
    match v2 with
    | None -> get_cas_validation_v1 server ~state ticket
    | Some _ -> return @@ `Yes v2

  let handler _ a =
    let module X = struct
      let pre_login_handler _ ~state =
        match List.assoc_opt "server" a.auth_config with
        | Some server ->
            let cas_login =
              Eliom_service.extern ~prefix:server ~path:[ "login" ]
                ~meth:(Eliom_service.Get Eliom_parameter.(string "service"))
                ()
            in
            let service = preapply ~service:cas_login (cas_self ~state) in
            let url = Web_services.make_absolute_string_uri ~service () in
            return (Web_auth_sig.Redirection url)
        | _ -> failwith "cas_login_handler invoked with bad config"

      let direct _ = failwith "direct authentication not implemented for CAS"
    end in
    (module X : Web_auth_sig.AUTH_SYSTEM)

  let run_post_login_handler =
    Web_auth.register ~auth_system:"cas" { handler; extern = true }

  let cas_handler (state, ticket) () =
    run_post_login_handler ~state
      {
        Web_auth.post_login_handler =
          (fun _ a cont ->
            match (ticket, List.assoc_opt "server" a.auth_config) with
            | Some x, Some server -> (
                let* r = get_cas_validation server ~state x in
                match r with
                | `Yes (Some name_and_email) -> cont (Some name_and_email)
                | `No -> cont None
                | `Yes None | `Error _ -> fail_http `Bad_gateway)
            | None, _ -> cont None
            | _, None -> fail_http `Service_unavailable);
      }

  let () = Eliom_registration.Any.register ~service:login_cas cas_handler
end
