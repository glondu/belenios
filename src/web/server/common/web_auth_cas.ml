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

open Lwt
open Lwt.Syntax
open Eliom_service
open Web_common

module Make (Web_auth : Web_auth_sig.S) = struct

  let next_lf str i =
    String.index_from_opt str i '\n'

  let login_cas = Eliom_service.create
                    ~path:(Eliom_service.Path ["auth"; "cas"])
                    ~meth:(Eliom_service.Get Eliom_parameter.(string "state" ** opt (string "ticket")))
                    ()

  let cas_self ~state =
    Eliom_uri.make_string_uri
      ~absolute:true
      ~service:(preapply ~service:login_cas (state, None))
      () |> rewrite_prefix

  let extract tag xs =
    let rec loop = function
      | [] -> None
      | x :: xs ->
         match x with
         | Xml.Element (tag', _, children) when tag = tag' -> Some children
         | _ -> loop xs
    in
    loop xs

  let extract_pcdata = function
    | [Xml.PCData x] -> Some x
    | _ -> None

  let parse_cas_validation_v2 info =
    let ( >>= ) = Option.bind and ( let* ) = Option.bind in
    try
      let* info =
        Some [Xml.parse_string info]
        >>= extract "cas:serviceResponse"
        >>= extract "cas:authenticationSuccess"
      in
      let* user =
        extract "cas:user" info
        >>= extract_pcdata
      in
      let mail =
        extract "cas:attributes" info
        >>= extract "cas:mail"
        >>= extract_pcdata
      in
      let mail = match mail with Some x -> x | None -> "" in
      Some (user, mail)
    with _ -> None

  let get_cas_validation_v2 server ~state ticket =
    let url =
      let cas_validate = Eliom_service.extern
                           ~prefix:server
                           ~path:["serviceValidate"]
                           ~meth:(Eliom_service.Get Eliom_parameter.(string "service" ** string "ticket"))
                           ()
      in
      let service = preapply ~service:cas_validate (cas_self ~state, ticket) in
      Eliom_uri.make_string_uri ~absolute:true ~service ()
    in
    let* r, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in
    if Cohttp.(Code.code_of_status (Response.status r)) = 200 then (
      let* info = Cohttp_lwt.Body.to_string body in
      return @@ parse_cas_validation_v2 info
    ) else return_none

  let parse_cas_validation_v1 info =
    match next_lf info 0 with
    | Some i ->
       (match String.sub info 0 i with
        | "yes" -> `Yes
                     (match next_lf info (i+1) with
                      | Some j -> Some (String.sub info (i+1) (j-i-1), "")
                      | None -> None)
        | "no" -> `No
        | _ -> `Error `Parsing)
    | None -> `Error `Parsing

  let get_cas_validation_v1 server ~state ticket =
    let url =
      let cas_validate = Eliom_service.extern
                           ~prefix:server
                           ~path:["validate"]
                           ~meth:(Eliom_service.Get Eliom_parameter.(string "service" ** string "ticket"))
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

  let cas_login_handler _ _ a ~state =
    match List.assoc_opt "server" a.Web_serializable_t.auth_config with
    | Some server ->
       let cas_login = Eliom_service.extern
                         ~prefix:server
                         ~path:["login"]
                         ~meth:(Eliom_service.Get Eliom_parameter.(string "service"))
                         ()
       in
       let service = preapply ~service:cas_login (cas_self ~state) in
       return @@ Web_auth_sig.Redirection (Eliom_registration.Redirection service)
    | _ -> failwith "cas_login_handler invoked with bad config"

  let run_post_login_handler =
    Web_auth.register_pre_login_handler ~auth_system:"cas" cas_login_handler

  let cas_handler (state, ticket) () =
    run_post_login_handler ~state
      {
        Web_auth.post_login_handler =
          fun _ a cont ->
          match ticket, List.assoc_opt "server" a.Web_serializable_t.auth_config with
          | Some x, Some server ->
             let* r = get_cas_validation server ~state x in
             (match r with
              | `Yes (Some name_and_email) -> cont (Some name_and_email)
              | `No -> cont None
              | `Yes None | `Error _ -> fail_http `Bad_gateway
             )
          | None, _ -> cont None
          | _, None -> fail_http `Service_unavailable
      }

  let () = Eliom_registration.Any.register ~service:login_cas cas_handler

end
