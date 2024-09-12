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

  type kind = [ `Election of uuid | `Site of site_cont ]

  type auth_env = {
    handler : uuid option -> auth_config -> (module AUTH_SYSTEM);
    username_or_address : [ `Username | `Address ];
    extern : bool;
    timeout : float;
    auth_config : auth_config;
    kind : kind;
    state : state option;
    mutable data : data option;
    mutable user : user option;
    credential : string option;
    mutable result : Belenios_api.Serializable_t.cast_result option;
  }

  let auth_env = ref SMap.empty
  let cred_env = ref SMap.empty

  let get_auth_env ~state =
    let now = Unix.gettimeofday () in
    let a, b = SMap.partition (fun _ x -> x.timeout > now) !auth_env in
    auth_env := a;
    SMap.iter
      (fun _ e ->
        match e.credential with
        | None -> ()
        | Some c -> cred_env := SMap.remove c !cred_env)
      b;
    SMap.find_opt state a

  let add_auth_env ?state ?user ?credential ~auth_config ~kind ~extern ~handler
      ~username_or_address () =
    let now = Unix.gettimeofday () in
    let timeout = now +. 900. in
    let a = SMap.filter (fun _ x -> x.timeout > now) !auth_env in
    let rec find_state () =
      let state = generate_token ~length:22 () in
      if SMap.mem state a then find_state () else state
    in
    let x =
      {
        handler;
        extern;
        timeout;
        auth_config;
        kind;
        state;
        data = None;
        user;
        username_or_address;
        credential;
        result = None;
      }
    in
    let state = find_state () in
    auth_env := SMap.add state x a;
    (state, x)

  let del_auth_env ~state =
    let e = get_auth_env ~state in
    auth_env := SMap.remove state !auth_env;
    match e with
    | Some { credential = Some c; _ } -> cred_env := SMap.remove c !cred_env
    | _ -> ()

  let get_cont ~extern login_or_logout ?state x =
    let open Eliom_registration in
    let redir =
      match x with
      | `Election _ -> (
          match state with
          | None -> `E `Forbidden
          | Some state ->
              `R (Redirection (preapply ~service:election_cast_confirm state)))
      | `Site { path = ContSiteHome; admin = admin_ui } -> (
          match admin_ui with
          | Default -> `R (Redirection home)
          | Basic -> `R (Redirection (admin_basic ())))
      | `Site { path = ContSiteElection uuid; admin = admin_ui } -> (
          match login_or_logout with
          | `Login -> (
              match admin_ui with
              | Default -> `S (make_admin_link (Some uuid))
              | Basic ->
                  let base =
                    make_absolute_string_uri
                      ~fragment:
                        (Printf.sprintf "elections/%s" (Uuid.unwrap uuid))
                      ~service:(admin_basic ()) ()
                  in
                  `S base)
          | `Logout ->
              `R
                (Redirection
                   (preapply ~service:election_home_redirect (uuid, ()))))
    in
    fun () ->
      if extern then
        let* uri =
          match redir with
          | `R (Redirection service) ->
              Lwt.return @@ make_absolute_string_uri ~service ()
          | `S s -> Lwt.return s
          | `E e -> fail_http e
        in
        Pages_common.html_redirection uri >>= Html.send
      else
        match redir with
        | `R r -> Redirection.send r
        | `S s -> String_redirection.send s
        | `E e -> fail_http e

  let restart_login service ~state = function
    | `Election _ -> preapply ~service:election_login (Some state)
    | `Site cont -> preapply ~service:site_login (Some service, cont)

  let run_post_login_handler ~auth_system ~state { post_login_handler } =
    match get_auth_env ~state with
    | Some ({ extern; auth_config = a; kind; data = Some data; _ } as env) ->
        let uuid = match kind with `Site _ -> None | `Election u -> Some u in
        let restart_login () =
          let service = restart_login a.auth_instance kind ~state in
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
                let user =
                  { user_domain = a.auth_instance; user_name = name }
                in
                let () = env.user <- Some user in
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
                  | Some _ -> Lwt.return_unit
                in
                get_cont ~extern `Login kind ~state ()
            | None -> restart_login ()
          in
          post_login_handler ~data uuid a cont
        else restart_login ()
    | _ ->
        Pages_common.authentication_impossible ()
        >>= Eliom_registration.Html.send

  let auth_systems = ref ([] : (string * auth_system) list)

  let get_pre_login_handler ~state env =
    let { kind; handler; auth_config; username_or_address; _ } = env in
    let uuid = match kind with `Site _ -> None | `Election u -> Some u in
    let module X = (val handler uuid auth_config) in
    let* result, data = X.pre_login_handler username_or_address ~state in
    env.data <- Some data;
    Lwt.return result

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

  let login_handler x =
    let@ state, env =
     fun cont ->
      match x with
      | `Election state -> (
          match get_auth_env ~state with
          | None -> fail_http `Gone
          | Some env -> cont (state, env))
      | `Site (service, site_cont) -> (
          let* user =
            let* x = Eliom_reference.get Web_state.site_user in
            let&* a, _, _ = x in
            Lwt.return_some a
          in
          let kind = `Site site_cont in
          let@ auth_config cont2 =
            let c = !Web_config.site_auth_config in
            match service with
            | Some s -> (
                match find_auth_instance s c with
                | Some x -> cont2 x
                | None -> fail_http `Not_found)
            | None -> (
                match c with
                | [ s ] -> cont2 s
                | _ ->
                    let builder s =
                      preapply ~service:Web_services.site_login
                        (Some s, site_cont)
                    in
                    let* page =
                      Pages_common.login_choose
                        (List.map (fun x -> x.auth_instance) c)
                        builder ()
                    in
                    Eliom_registration.Html.send page)
          in
          match List.assoc_opt auth_config.auth_system !auth_systems with
          | Some { extern; handler; _ } ->
              add_auth_env ?user ~auth_config ~kind ~extern ~handler
                ~username_or_address:`Username ()
              |> cont
          | None -> fail_http `Not_found)
    in
    let { kind; user; auth_config; _ } = env in
    let uuid = match kind with `Site _ -> None | `Election u -> Some u in
    match (user, uuid) with
    | Some _, None -> get_cont ~extern:false `Login kind ~state ()
    | Some _, Some _ | None, _ -> (
        let site_or_election =
          match uuid with None -> `Site | Some _ -> `Election
        in
        let* x = get_pre_login_handler ~state env in
        match x with
        | Html x ->
            let* title =
              Pages_common.login_title site_or_election
                auth_config.auth_instance
            in
            let* page = Pages_common.base ~title ~content:[ x ] () in
            Eliom_registration.Html.send page
        | Redirection x -> Eliom_registration.String_redirection.send x)

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
    Eliom_registration.Any.register ~service:site_login (fun x () ->
        login_handler (`Site x))

  let () =
    Eliom_registration.Any.register ~service:logout (fun cont () ->
        logout_handler cont)

  let () =
    Eliom_registration.Any.register ~service:election_login (fun x () ->
        match x with
        | None ->
            let page =
              Pages_common.html_js_exec
                {|
                  belenios.init({root: "../"});
                  belenios.initiateLogin();
                |}
            in
            Eliom_registration.Html.send page
        | Some x -> login_handler (`Election x))

  let get_site_login_handler service =
    match find_auth_instance service !Web_config.site_auth_config with
    | None -> return @@ Html (Eliom_content.Html.F.div [])
    | Some auth_config -> (
        match List.assoc_opt auth_config.auth_system !auth_systems with
        | Some { extern; handler; _ } ->
            let kind = `Site (default_admin ContSiteHome) in
            let state, env =
              add_auth_env ~auth_config ~kind ~extern ~handler
                ~username_or_address:`Username ()
            in
            get_pre_login_handler ~state env
        | None -> return @@ Html (Eliom_content.Html.F.div []))

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

  module State = struct
    let create storage uuid state =
      let credential = state.precast_data.credential in
      let () =
        match SMap.find_opt credential !cred_env with
        | None -> ()
        | Some state -> del_auth_env ~state
      in
      let kind = `Election uuid in
      let* c, username_or_address =
        let* c = get_election_auth_configs storage uuid in
        let* username_or_address =
          Web_persist.get_username_or_address storage uuid
        in
        Lwt.return (c, username_or_address)
      in
      match c with
      | [ auth_config ] -> (
          match List.assoc_opt auth_config.auth_system !auth_systems with
          | Some { extern; handler; _ } ->
              let state, _ =
                add_auth_env ~state ~auth_config ~kind ~extern ~handler
                  ~username_or_address ~credential ()
              in
              Lwt.return_some state
          | None -> Lwt.return_none)
      | _ -> Lwt.return_none

    let get ~state =
      let@ { state; user; kind; _ } = Option.bind (get_auth_env ~state) in
      match kind with
      | `Election uuid -> Some Web_auth_sig.{ state; user; uuid }
      | _ -> None

    let del = del_auth_env

    let get_result ~state =
      let@ { result; _ } = Option.bind (get_auth_env ~state) in
      let@ result = Option.bind result in
      Some result

    let set_result ~state x =
      match get_auth_env ~state with
      | None -> ()
      | Some env -> env.result <- Some x
  end
end
