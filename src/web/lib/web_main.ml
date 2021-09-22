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
open Belenios_core
open Serializable_builtin_t
open Web_serializable_j
open Web_common

module Make () = struct

  (** Parse configuration from <eliom> *)

  let locales_dir = ref None
  let spool_dir = ref None
  let accounts_dir = ref None
  let source_file = ref None
  let auth_instances = ref []
  let auth_instances_export = ref []
  let gdpr_uri = ref None
  let default_group_file = ref None
  let nh_group_file = ref None
  let domain = ref None

  let () =
    Eliom_config.get_config () |>
      let open Xml in
      List.iter @@ function
                  | PCData x ->
                     Ocsigen_extensions.Configuration.ignore_blank_pcdata ~in_tag:"belenios" x
                  | Element ("maxrequestbodysizeinmemory", ["value", m], []) ->
                     Ocsigen_config.set_maxrequestbodysizeinmemory (int_of_string m)
                  | Element ("log", ["file", file], []) ->
                     Lwt_main.run (open_security_log file)
                  | Element ("source", ["file", file], []) ->
                     source_file := Some file
                  | Element ("default-group", ["file", file], []) ->
                     default_group_file := Some (`File file)
                  | Element ("nh-group", ["file", file], []) ->
                     nh_group_file := Some (`File file)
                  | Element ("default-group", ["group", group], []) ->
                     default_group_file := Some (`Group group)
                  | Element ("nh-group", ["group", group], []) ->
                     nh_group_file := Some (`Group group)
                  | Element ("maxmailsatonce", ["value", limit], []) ->
                     Web_config.maxmailsatonce := int_of_string limit
                  | Element ("uuid", ["length", length], []) ->
                     let length = int_of_string length in
                     if length >= min_uuid_length then
                       Web_config.uuid_length := Some length
                     else
                       failwith "UUID length is too small"
                  | Element ("contact", ["uri", uri], []) ->
                     Web_config.contact_uri := Some uri
                  | Element ("gdpr", ["uri", uri], []) ->
                     gdpr_uri := Some uri
                  | Element ("server", attrs, []) ->
                     let set check_email attr setter =
                       match List.assoc_opt attr attrs with
                       | Some mail ->
                          if not check_email || is_email mail then setter mail
                          else Printf.ksprintf failwith "%s is not a valid e-mail address" mail
                       | None -> ()
                     in
                     set true "mail" (fun x -> Web_config.server_mail := x);
                     set true "return-path" (fun x -> Web_config.return_path := Some x);
                     set false "name" (fun x -> Web_config.server_name := x);
                  | Element ("locales", ["dir", dir], []) ->
                     locales_dir := Some dir
                  | Element ("spool", ["dir", dir], []) ->
                     spool_dir := Some dir
                  | Element ("accounts", ["dir", dir], []) ->
                     accounts_dir := Some dir
                  | Element ("warning", ["file", file], []) ->
                     Web_config.warning_file := Some file
                  | Element ("footer", ["file", file], []) ->
                     Web_config.footer_file := Some file
                  | Element ("admin-home", ["file", file], []) ->
                     Web_config.admin_home := Some file
                  | Element ("rewrite-prefix", ["src", src; "dst", dst], []) ->
                     set_rewrite_prefix ~src ~dst
                  | Element ("auth", ["name", auth_instance],
                             [Element (auth_system, auth_config, [])]) ->
                     let i = {auth_system; auth_instance; auth_config} in
                     auth_instances := i :: !auth_instances
                  | Element ("auth-export", ["name", "builtin-password"], []) ->
                     auth_instances_export := `BuiltinPassword :: !auth_instances_export
                  | Element ("auth-export", ["name", "builtin-cas"], []) ->
                     auth_instances_export := `BuiltinCAS :: !auth_instances_export
                  | Element ("auth-export", ["name", auth_instance],
                             [Element (auth_system, auth_config, [])]) ->
                     let i = {auth_system; auth_instance; auth_config} in
                     auth_instances_export := `Export i :: !auth_instances_export
                  | Element ("domain", ["name", name], []) ->
                     domain := Some name
                  | Element (tag, _, _) ->
                     Printf.ksprintf failwith
                       "invalid configuration for tag %s in belenios"
                       tag

  let () =
    match !gdpr_uri with
    | None -> failwith "You must provide a GDPR URI"
    | Some x -> Web_config.gdpr_uri := x

  (** Parse configuration from other sources *)

  let source_file =
    Lwt_main.run
      (match !source_file with
       | Some f ->
          let* b = file_exists f in
          if b then (
            return f
          ) else (
            Printf.ksprintf failwith "file %s does not exist" f
          )
       | None -> failwith "missing <source> in configuration"
      )

  let locales_dir =
    match !locales_dir with
    | Some d -> d
    | None -> failwith "missing <locales> in configuration"

  let spool_dir =
    match !spool_dir with
    | Some d -> d
    | None -> failwith "missing <spool> in configuration"

  let accounts_dir =
    match !accounts_dir with
    | Some d -> d
    | None -> failwith "missing <accounts> in configuration"

  let default_group =
    Lwt_main.run
      (match !default_group_file with
       | None -> failwith "missing <default-group> in configuration"
       | Some (`Group x) -> return x
       | Some (`File x) ->
          let* x = Lwt_io.lines_of_file x |> Lwt_stream.to_list in
          match x with
          | [x] -> return x
          | _ -> failwith "invalid default group file"
      )

  let nh_group =
    Lwt_main.run
      (match !nh_group_file with
       | None -> failwith "missing <nh-group> in configuration"
       | Some (`Group x) -> return x
       | Some (`File x) ->
          let* x = Lwt_io.lines_of_file x |> Lwt_stream.to_list in
          match x with
          | [x] -> return x
          | _ -> failwith "invalid NH group file"
      )

  let domain =
    match !domain with
    | Some d -> d
    | None -> failwith "missing <domain> in configuration"

  (** Build up the site *)

  let () = Web_config.source_file := source_file
  let () = Web_config.locales_dir := locales_dir
  let () = Web_config.spool_dir := spool_dir
  let () = Web_config.accounts_dir := accounts_dir
  let () = Web_config.default_group := default_group
  let () = Web_config.nh_group := nh_group
  let () = Web_config.site_auth_config := List.rev !auth_instances
  let () = Web_config.exported_auth_config := List.rev !auth_instances_export
  let () = Web_config.domain := domain
  let () = Lwt_main.run (Web_persist.convert_trustees ())

  module X : Pages_sig.S = struct
    module Web_state = Web_state.Make ()
    module Web_services = Web_services.Make ()
    module Web_i18n = Web_i18n.Make (Web_state)
    module Pages_common = Pages_common.Make (Web_i18n) (Web_services)
    module Pages_admin = Pages_admin.Make (Web_state) (Web_i18n) (Web_services) (Pages_common)
    module Pages_voter = Pages_voter.Make (Web_state) (Web_i18n) (Web_services) (Pages_common)
  end

  module Api = Api_eliom.Make ()
  module Web_captcha = Web_captcha.Make (X.Web_services)

  module Web_auth = Web_auth.Make (X.Web_state) (X.Web_services) (X.Pages_common)
  module Web_auth_dummy = Web_auth_dummy.Make (X.Web_services) (X.Pages_common) (Web_auth)
  module Web_auth_password = Web_auth_password.Make (X.Web_services) (X.Pages_common) (Web_auth)
  module Web_auth_email = Web_auth_email.Make (X.Web_services) (X.Pages_common) (Web_auth)
  module Web_auth_cas = Web_auth_cas.Make (Web_auth)
  module Web_auth_oidc = Web_auth_oidc.Make (Web_auth)

  module Site_common = Site_common.Make (X)
  module Site_admin = Site_admin.Make (X) (Site_common) (Web_auth)
  module Site_voter = Site_voter.Make (X) (Site_common) (Site_admin)

  let () = Lwt.async Site_admin.data_policy_loop

end

let main () =
  let module M = Make () in
  ()
