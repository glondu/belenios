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

open Web_signatures
open Web_common
open Auth_common

let next_lf str i =
  try Some (String.index_from str i '\n')
  with Not_found -> None

module type CONFIG = sig
  val server : string
end

module Make (C : CONFIG) (N : NAME) (S : CONT_SERVICE) (T : TEMPLATES) : AUTH_INSTANCE = struct

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

  let on_success_ref = Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (fun ~user_name ~user_logout -> Lwt.return ())

  let () = Eliom_registration.Redirection.register
    ~service:login_cas
    (fun ticket () ->
      match ticket with
      | Some x ->
        let me =
          let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
          rewrite_prefix uri
        in
        let validation =
          let service = Eliom_service.preapply cas_validate (me, x) in
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
                        let module L : CONT_SERVICE = struct
                          let cont () =
                            lwt service = S.cont () in
                            let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
                            let uri = rewrite_prefix uri in
                            security_log (fun () ->
                              Printf.sprintf "%s:%s logged out, redirecting to CAS [%s]"
                                N.name user_name C.server
                            ) >> Lwt.return (Eliom_service.preapply cas_logout uri)
                        end in
                        let user_logout = (module L : CONT_SERVICE) in
                        lwt on_success = Eliom_reference.get on_success_ref in
                        on_success ~user_name ~user_logout >>
                        S.cont ()
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
        let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
        let uri = rewrite_prefix uri in
        Lwt.return (Eliom_service.preapply cas_login uri)
    )

  let handler ~on_success () =
    Eliom_reference.set on_success_ref on_success >>
    Eliom_registration.Redirection.send service

end

type instance = {
  mutable name : string option;
  mutable server : string option;
}

let init () =
  let instances = ref [] in
  let current_instance = ref None in
  let push_current loc =
    match !current_instance with
    | None -> ()
    | Some {name = Some name; server = Some server} ->
      let module C : CONFIG = struct
        let server = server
      end in
      instances := (name, (module C : CONFIG)) :: !instances;
      current_instance := None
    | _ -> failwith ("unexpected case in auth-cas/" ^ loc)
  in
  let spec =
    let open Ocsigen_extensions.Configuration in
    [
      let init () =
        push_current "init";
        current_instance := Some {name = None; server = None}
      and attributes = [
        attribute ~name:"name" ~obligatory:true (fun s ->
          match !current_instance with
          | Some ({name = None; _} as i) -> i.name <- Some s
          | _ -> failwith "unexpected case in auth-cas/name"
        );
        attribute ~name:"server" ~obligatory:true (fun s ->
          match !current_instance with
          | Some ({server = None; _} as i) -> i.server <- Some s
          | _ -> failwith "unexpected case in auth-cas/server"
        );
      ] in element ~name:"auth-cas" ~init ~attributes ();
    ]
  and exec ~instantiate =
    push_current "exec";
    List.iter (fun (name, config) ->
      let module X = Make ((val config : CONFIG)) in
      instantiate name (module X : AUTH_SERVICE)
    ) !instances
  in Auth_common.register_auth_system ~spec ~exec
