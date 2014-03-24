(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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
open Web_signatures
open Web_common

let next_lf str i =
  try Some (String.index_from str i '\n')
  with Not_found -> None

type config = { server : string }

module type CONFIG = sig
  val server : string
end

module Make (C : CONFIG) (N : NAME) (T : LOGIN_TEMPLATES) : AUTH_HANDLERS = struct

  let scope = Eliom_common.default_session_scope

  let cas_login = Eliom_service.external_service
    ~prefix:C.server
    ~path:["login"]
    ~get_params:Eliom_parameter.(string "service")
    ()

  let cas_logout = Eliom_service.external_service
    ~prefix:C.server
    ~path:["logout"]
    ~get_params:Eliom_parameter.(string "service")
    ()

  let cas_validate = Eliom_service.external_service
    ~prefix:C.server
    ~path:["validate"]
    ~get_params:Eliom_parameter.(string "service" ** string "ticket")
    ()

  let login_cas = Eliom_service.service
    ~path:N.path
    ~get_params:Eliom_parameter.(opt (string "ticket"))
    ()

  let service = Eliom_service.preapply login_cas None

  let self =
    Eliom_uri.make_string_uri ~absolute:true ~service () |> rewrite_prefix

  let login_cont = Eliom_reference.eref ~scope None
  let logout_cont = Eliom_reference.eref ~scope None

  let () = Eliom_registration.Any.register
    ~service:login_cas
    (fun ticket () ->
      match ticket with
      | Some x ->
        let validation =
          let service = Eliom_service.preapply cas_validate (self, x) in
          Eliom_uri.make_string_uri ~absolute:true ~service ()
        in
        lwt reply = Ocsigen_http_client.get_url validation in
        (match reply.Ocsigen_http_frame.frame_content with
          | Some stream ->
            lwt info = Ocsigen_stream.(string_of_stream 1000 (get stream)) in
            Ocsigen_stream.finalize stream `Success >>
            (match next_lf info 0 with
              | Some i ->
                (match String.sub info 0 i with
                  | "yes" ->
                    (match next_lf info (i+1) with
                      | Some j ->
                        let user_name = String.sub info (i+1) (j-i-1) in
                        (match_lwt Eliom_reference.get login_cont with
                        | Some cont ->
                          Eliom_reference.unset login_cont >>
                          cont user_name ()
                        | None -> fail_http 400
                        )
                      | None -> fail_http 502
                    )
                  | "no" -> fail_http 401
                  | _ -> fail_http 502
                )
              | None -> fail_http 502
            )
          | None -> fail_http 502
        )
      | None ->
        match_lwt Eliom_reference.get logout_cont with
        | None ->
          lwt () = security_log (fun () ->
            Printf.sprintf
              "user is trying to log in, redirecting to CAS [%s]"
              C.server
          ) in
          Eliom_service.preapply cas_login self |>
          Eliom_registration.Redirection.send
        | Some cont ->
          Eliom_reference.unset logout_cont >>
          cont () ()
    )

  let login cont () =
    Eliom_reference.set login_cont (Some cont) >>
    Eliom_registration.Redirection.send service

  let logout cont () =
    security_log (fun () ->
      Printf.sprintf "user logged out, redirecting to CAS [%s]" C.server
    ) >>
    lwt () = Eliom_reference.set logout_cont (Some cont) in
    Eliom_service.preapply cas_logout self |>
    Eliom_registration.Redirection.send

end

let name = "cas"

let parse_config ~instance ~attributes =
  match attributes with
  | ["server", server] -> {server}
  | _ ->
    Printf.ksprintf failwith
      "invalid configuration for instance %s of auth/%s"
      instance name

let make {server} =
  let module C = struct let server = server end in
  (module Make (C) : AUTH_SERVICE)

type c = config

module A : AUTH_SYSTEM = struct
  type config = c
  let name = name
  let parse_config = parse_config
  let make = make
end

let () = Web_auth.register_auth_system (module A : AUTH_SYSTEM)
