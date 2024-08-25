(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios_server_core
open Web_common
open Web_auth_sig

module Make
    (Web_state : Web_state_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S) =
struct
  open Web_services

  type Web_auth_sig.data += No_data

  type post_login_handler = {
    post_login_handler :
      'a.
      data:Web_auth_sig.data ->
      uuid option ->
      auth_config ->
      ((string * string option) option -> 'a Lwt.t) ->
      'a Lwt.t;
  }

  type auth_env = {
    extern : bool;
    timeout : float;
    uuid : uuid option;
    auth_config : auth_config;
    kind : [ `Election of Uuid.t | `Site of site_cont ];
    mutable data : data option;
  }

  let auth_env = ref SMap.empty

  let get_auth_env ~state =
    let now = Unix.gettimeofday () in
    let a = SMap.filter (fun _ x -> x.timeout > now) !auth_env in
    auth_env := a;
    SMap.find_opt state a

  let add_auth_env ~uuid ~auth_config ~kind ~extern =
    let now = Unix.gettimeofday () in
    let timeout = now +. 600. in
    let a = SMap.filter (fun _ x -> x.timeout > now) !auth_env in
    let rec find_state () =
      let state = generate_token () in
      if SMap.mem state a then find_state () else state
    in
    let state = find_state () in
    let x = { extern; timeout; uuid; auth_config; kind; data = None } in
    auth_env := SMap.add state x a;
    (state, x)

  let del_auth_env ~state = auth_env := SMap.remove state !auth_env

  let get_cont ~extern login_or_logout x =
    let open Eliom_registration in
    let redir =
      match x with
      | `Election uuid ->
          `R (Redirection (preapply ~service:election_cast_confirm uuid))
      | `Site { path = ContSiteHome; admin = admin_ui } -> (
          match admin_ui with
          | Classic -> `R (Redirection home)
          | Basic -> `R (Redirection (admin_basic ()))
          | New -> `R (Redirection (admin_new ())))
      | `Site { path = ContSiteElection uuid; admin = admin_ui } -> (
          match login_or_logout with
          | `Login -> (
              match admin_ui with
              | Classic ->
                  `R (Redirection (preapply ~service:election_admin uuid))
              | Basic ->
                  let base =
                    Eliom_uri.make_string_uri ~service:(admin_basic ())
                      ~absolute:true ()
                    |> rewrite_prefix
                  in
                  `S (Printf.sprintf "%s#elections/%s" base (Uuid.unwrap uuid))
              | New ->
                  let base =
                    Eliom_uri.make_string_uri ~service:(admin_new ())
                      ~absolute:true ()
                    |> rewrite_prefix
                  in
                  `S (Printf.sprintf "%s#%s" base (Uuid.unwrap uuid)))
          | `Logout ->
              `R (Redirection (preapply ~service:election_home (uuid, ()))))
    in
    fun () ->
      if extern then
        let uri =
          match redir with
          | `R (Redirection service) ->
              Eliom_uri.make_string_uri ~service ~absolute:true ()
              |> rewrite_prefix
          | `S s -> s
        in
        Pages_common.html_redirection uri >>= Html.send
      else
        match redir with
        | `R r -> Redirection.send r
        | `S s -> String_redirection.send s

  let restart_login service = function
    | `Election uuid ->
        preapply ~service:election_login ((uuid, ()), Some service)
    | `Site cont -> preapply ~service:site_login (Some service, cont)

  let run_post_login_handler ~auth_system ~state { post_login_handler } =
    match get_auth_env ~state with
    | Some { extern; uuid; auth_config = a; kind; data = Some data; _ } ->
        let restart_login () =
          let service = restart_login a.auth_instance kind in
          Pages_common.login_failed ~service ()
          >>= Eliom_registration.Html.send ~code:401
        in
        if auth_system = a.auth_system then
          let cont = function
            | Some (name, email) ->
                let@ () =
                 fun cont ->
                  match List.assoc_opt "allowlist" a.auth_config with
                  | None -> cont ()
                  | Some f ->
                      let* allowlist =
                        let@ s = Storage.with_transaction in
                        let module S = (val s) in
                        S.get (Auth_db f)
                      in
                      let allowlist =
                        match allowlist with
                        | None -> []
                        | Some x -> split_lines x
                      in
                      if List.mem name allowlist then cont ()
                      else restart_login ()
                in
                let () = del_auth_env ~state in
                let user =
                  { user_domain = a.auth_instance; user_name = name }
                in
                let* () =
                  match uuid with
                  | None ->
                      let* account =
                        let* id = Storage.get_user_id user in
                        match id with
                        | None ->
                            let@ s = Storage.with_transaction in
                            Accounts.create_account s ~email user
                        | Some id -> (
                            let@ s = Storage.with_transaction in
                            let* a = Accounts.update_account_by_id s id in
                            match a with
                            | None ->
                                Lwt.fail
                                @@ Failure "anomaly in post_login_handler"
                            | Some (x, set) ->
                                let last_connected = Datetime.now () in
                                let x = { x with last_connected } in
                                let* () = set x in
                                return x)
                      in
                      let* token = Api_generic.new_token account in
                      Eliom_reference.set Web_state.site_user
                        (Some (user, account, token))
                  | Some uuid ->
                      Eliom_reference.set Web_state.election_user
                        (Some (uuid, user))
                in
                get_cont ~extern `Login kind ()
            | None -> restart_login ()
          in
          post_login_handler ~data uuid a cont
        else restart_login ()
    | _ ->
        Pages_common.authentication_impossible ()
        >>= Eliom_registration.Html.send

  let auth_systems = ref ([] : (string * auth_system) list)

  let get_pre_login_handler uuid username_or_address kind a =
    match List.assoc_opt a.auth_system !auth_systems with
    | Some { handler; extern } ->
        let state, env = add_auth_env ~uuid ~auth_config:a ~kind ~extern in
        let module X = (val handler uuid a) in
        let* result, data = X.pre_login_handler username_or_address ~state in
        env.data <- Some data;
        Lwt.return result
    | None -> fail_http `Not_found

  let register ~auth_system handler =
    auth_systems := (auth_system, handler) :: !auth_systems;
    run_post_login_handler ~auth_system

  let rec find_auth_instance x = function
    | [] -> None
    | ({ auth_instance = i; _ } as y) :: _ when i = x -> Some y
    | _ :: xs -> find_auth_instance x xs

  let get_election_auth_configs s uuid =
    let* metadata = Web_persist.get_election_metadata s uuid in
    match metadata.e_auth_config with
    | None -> return []
    | Some x ->
        x
        |> List.map (function
             | { auth_system = "import"; auth_instance = name; _ } -> (
                 match
                   List.find_opt
                     (function
                       | `Export x -> x.auth_instance = name | _ -> false)
                     !Web_config.exported_auth_config
                 with
                 | Some (`Export x) -> [ x ]
                 | _ -> [])
             | x -> [ x ])
        |> List.flatten |> return

  let login_handler service kind =
    let uuid =
      match kind with `Site _ -> None | `Election uuid -> Some uuid
    in
    let myself service =
      match kind with
      | `Site cont -> preapply ~service:site_login (service, cont)
      | `Election uuid -> preapply ~service:election_login ((uuid, ()), service)
    in
    let* user =
      match uuid with
      | None ->
          let* x = Eliom_reference.get Web_state.site_user in
          let&* a, _, _ = x in
          return_some a
      | Some uuid -> Web_state.get_election_user uuid
    in
    match (user, uuid) with
    | Some _, None -> get_cont ~extern:false `Login kind ()
    | Some _, Some _ | None, _ -> (
        let@ storage = Storage.with_transaction in
        let* c =
          match uuid with
          | None -> return !Web_config.site_auth_config
          | Some uuid -> get_election_auth_configs storage uuid
        in
        match service with
        | Some s -> (
            let* site_or_election, username_or_address =
              match uuid with
              | None -> return (`Site, `Username)
              | Some uuid ->
                  let* username_or_address =
                    Web_persist.get_username_or_address storage uuid
                  in
                  return (`Election, username_or_address)
            in
            let* a =
              match find_auth_instance s c with
              | Some x -> return x
              | None -> fail_http `Not_found
            in
            let* x = get_pre_login_handler uuid username_or_address kind a in
            match x with
            | Html x ->
                let* title =
                  Pages_common.login_title site_or_election a.auth_instance
                in
                Pages_common.base ~title ~content:[ x ] ()
                >>= Eliom_registration.Html.send
            | Redirection x -> Eliom_registration.String_redirection.send x)
        | None -> (
            match c with
            | [ s ] ->
                Eliom_registration.(
                  Redirection.send (Redirection (myself (Some s.auth_instance))))
            | _ ->
                let builder =
                  match kind with
                  | `Site cont ->
                      fun s ->
                        preapply ~service:Web_services.site_login (Some s, cont)
                  | `Election uuid ->
                      fun s ->
                        preapply ~service:Web_services.election_login
                          ((uuid, ()), Some s)
                in
                Pages_common.login_choose
                  (List.map (fun x -> x.auth_instance) c)
                  builder ()
                >>= Eliom_registration.Html.send))

  let logout_handler cont =
    let* () =
      let* x = Eliom_reference.get Web_state.site_user in
      match x with
      | None -> Lwt.return_unit
      | Some (_, _, token) ->
          let () = Api_generic.invalidate_token token in
          Lwt.return_unit
    in
    let* () = Web_state.discard () in
    get_cont ~extern:false `Logout (`Site cont) ()

  let () =
    Eliom_registration.Any.register ~service:site_login
      (fun (service, cont) () -> login_handler service (`Site cont))

  let () =
    Eliom_registration.Any.register ~service:logout (fun cont () ->
        logout_handler cont)

  let () =
    Eliom_registration.Any.register ~service:election_login
      (fun ((uuid, ()), service) () -> login_handler service (`Election uuid))

  let get_site_login_handler service =
    match find_auth_instance service !Web_config.site_auth_config with
    | None -> return @@ Html (Eliom_content.Html.F.div [])
    | Some a ->
        get_pre_login_handler None `Username
          (`Site (default_admin ContSiteHome))
          a

  let direct_voter_auth s uuid x =
    let fail () = failwith "invalid direct auth" in
    let* c =
      let* cs = get_election_auth_configs s uuid in
      match cs with
      | [ c ] -> Lwt.return c
      | _ -> (
          match x with
          | `Assoc x -> (
              match List.assoc_opt "service" x with
              | Some (`String service) -> (
                  match find_auth_instance service cs with
                  | Some c -> Lwt.return c
                  | None -> fail ())
              | _ -> fail ())
          | _ -> fail ())
    in
    match List.assoc_opt c.auth_system !auth_systems with
    | Some { handler; _ } ->
        let module X = (val handler (Some uuid) c) in
        let* user_name = X.direct x in
        Lwt.return { user_name; user_domain = c.auth_instance }
    | None -> fail ()
end
