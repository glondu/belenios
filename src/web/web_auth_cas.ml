(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Eliom_service
open Web_common

let next_lf str i =
  String.index_from_opt str i '\n'

let login_cas = Eliom_service.create
  ~path:(Eliom_service.Path ["auth"; "cas"])
  ~meth:(Eliom_service.Get Eliom_parameter.(string "state" ** opt (string "ticket")))
  ()

let cas_self ~state =
  Eliom_uri.make_string_uri
    ~absolute:true
    ~service:(preapply login_cas (state, None))
    () |> rewrite_prefix

let parse_cas_validation info =
  match next_lf info 0 with
  | Some i ->
     (match String.sub info 0 i with
     | "yes" -> `Yes
        (match next_lf info (i+1) with
        | Some j -> Some (String.sub info (i+1) (j-i-1))
        | None -> None)
     | "no" -> `No
     | _ -> `Error `Parsing)
  | None -> `Error `Parsing

let get_cas_validation server ~state ticket =
  let url =
    let cas_validate = Eliom_service.extern
      ~prefix:server
      ~path:["validate"]
      ~meth:(Eliom_service.Get Eliom_parameter.(string "service" ** string "ticket"))
      ()
    in
    let service = preapply cas_validate (cas_self ~state, ticket) in
    Eliom_uri.make_string_uri ~absolute:true ~service ()
  in
  let%lwt reply = Ocsigen_http_client.get_url url in
  match reply.Ocsigen_http_frame.frame_content with
  | Some stream ->
     let%lwt info = Ocsigen_stream.(string_of_stream 1000 (get stream)) in
     let%lwt () = Ocsigen_stream.finalize stream `Success in
     return (parse_cas_validation info)
  | None -> return (`Error `Http)

let cas_handler (state, ticket) () =
  Web_auth.run_post_login_handler ~auth_system:"cas" ~state
    (fun _ a authenticate ->
      match ticket, List.assoc_opt "server" a.Web_serializable_t.auth_config with
      | Some x, Some server ->
         (match%lwt get_cas_validation server ~state x with
          | `Yes (Some name) -> authenticate name
          | `No -> fail_http 401
          | `Yes None | `Error _ -> fail_http 502
         )
      | None, _ -> return_unit
      | _, None -> fail_http 503
    )

let () = Eliom_registration.Any.register ~service:login_cas cas_handler

let cas_login_handler a ~state =
  match List.assoc_opt "server" a.Web_serializable_t.auth_config with
  | Some server ->
     let cas_login = Eliom_service.extern
       ~prefix:server
       ~path:["login"]
       ~meth:(Eliom_service.Get Eliom_parameter.(string "service"))
       ()
     in
     let service = preapply cas_login (cas_self ~state) in
     Eliom_registration.(Redirection.send (Redirection service))
  | _ -> failwith "cas_login_handler invoked with bad config"

let () = Web_auth.register_pre_login_handler "cas" cas_login_handler
