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
open Belenios
open Belenios_storage_api
open Belenios_server_core

module Make () = struct
  (** Parse configuration from <eliom> *)

  let prefix = ref None
  let rewrite_prefix = ref None
  let share_dir = ref None
  let source_file = ref None
  let auth_instances = ref []
  let auth_instances_export = ref []
  let tos = ref None
  let default_group_file = ref None
  let nh_group_file = ref None
  let domain = ref None

  let () =
    Eliom_config.get_config ()
    |>
    let open Xml in
    List.iter @@ function
    | PCData x ->
        Ocsigen_extensions.Configuration.ignore_blank_pcdata ~in_tag:"belenios"
          x
    | Element ("public-url", attrs, []) ->
        let prefix_ = List.assoc_opt "prefix" attrs in
        let rewrite = List.assoc_opt "rewrite" attrs in
        let () =
          match (prefix_, rewrite) with
          | Some dst, Some src -> rewrite_prefix := Some (src, dst)
          | _ -> ()
        in
        prefix := prefix_
    | Element ("maxrequestbodysizeinmemory", [ ("value", m) ], []) ->
        Ocsigen_config.set_maxrequestbodysizeinmemory (int_of_string m)
    | Element ("source", [ ("file", file) ], []) -> source_file := Some file
    | Element ("logo", [ ("file", file); ("mime-type", mime_type) ], []) ->
        Web_config.logo := Some (file, mime_type)
    | Element ("favicon", [ ("file", file); ("mime-type", mime_type) ], []) ->
        Web_config.favicon := Some (file, mime_type)
    | Element ("sealing", [ ("file", file); ("mime-type", mime_type) ], []) ->
        Web_config.sealing := Some (file, mime_type)
    | Element ("default-group", [ ("group", group) ], []) ->
        default_group_file := Some (`Group group)
    | Element ("nh-group", [ ("group", group) ], []) ->
        nh_group_file := Some (`Group group)
    | Element ("maxmailsatonce", [ ("value", limit) ], []) ->
        Web_config.maxmailsatonce := int_of_string limit
    | Element ("contact", [ ("uri", uri) ], []) ->
        Web_config.contact_uri := Some uri
    | Element ("tos", attrs, []) ->
        let () =
          match List.assoc_opt "last-update" attrs with
          | Some x -> Web_config.tos_last_update := float_of_string x
          | None -> ()
        in
        tos := List.assoc_opt "uri" attrs
    | Element ("vendor", [ ("name", name) ], []) -> Web_config.vendor := name
    | Element ("server", attrs, []) ->
        let set check_email attr setter =
          match List.assoc_opt attr attrs with
          | Some mail ->
              if (not check_email) || is_email mail then setter mail
              else
                Printf.ksprintf failwith "%s is not a valid e-mail address" mail
          | None -> ()
        in
        set true "mail" (fun x -> Web_config.server_mail := x);
        set true "return-path" (fun x -> Web_config.return_path := Some x);
        set false "name" (fun x -> Web_config.server_name := x)
    | Element ("share", [ ("dir", dir) ], []) -> share_dir := Some dir
    | Element ("warning", [ ("file", file) ], []) ->
        Web_config.warning_file := Some file
    | Element ("footer", [ ("file", file) ], []) ->
        Web_config.footer_file := Some file
    | Element ("admin-home", [ ("file", file) ], []) ->
        Web_config.admin_home := Some file
    | Element ("success-snippet", [ ("file", file) ], []) ->
        Web_config.success_snippet := Some file
    | Element
        ( "auth",
          [ ("name", auth_instance) ],
          [ Element (auth_system, auth_config, []) ] ) ->
        let i = { auth_system; auth_instance; auth_config } in
        auth_instances := i :: !auth_instances
    | Element ("auth-export", [ ("name", "builtin-password") ], []) ->
        auth_instances_export := `BuiltinPassword :: !auth_instances_export
    | Element ("auth-export", [ ("name", "builtin-cas") ], []) ->
        auth_instances_export := `BuiltinCAS :: !auth_instances_export
    | Element
        ( "auth-export",
          [ ("name", auth_instance) ],
          [ Element (auth_system, auth_config, []) ] ) ->
        let i = { auth_system; auth_instance; auth_config } in
        auth_instances_export := `Export i :: !auth_instances_export
    | Element ("domain", [ ("name", name) ], []) -> domain := Some name
    | Element ("deny-revote", [], []) -> Web_config.deny_revote := true
    | Element ("deny-newelection", [], []) ->
        Web_config.deny_newelection := true
    | Element ("blacklisted-domains", [ ("file", file) ], []) ->
        let ic = open_in file in
        let rec loop accu =
          match input_line ic with
          | exception End_of_file ->
              close_in ic;
              accu
          | x -> loop (SSet.add x accu)
        in
        Web_config.blacklisted_domains := loop !Web_config.blacklisted_domains
    | Element ("billing", [ ("url", url); ("callback", callback) ], []) ->
        Web_config.billing := Some (url, callback)
    | Element ("external-send-message", [ ("url", url); ("key", key) ], []) ->
        Web_config.send_message := Some (url, key)
    | Element ("restricted", [], []) -> Web_config.restricted_mode := true
    | Element ("election-sealing", [], []) ->
        Web_config.election_sealing := true
    | Element ("storage", [ ("backend", backend) ], config) ->
        Lwt_main.run (Storage.init_backend backend config)
    | Element (tag, _, _) ->
        Printf.ksprintf failwith "invalid configuration for tag %s in belenios"
          tag

  let () =
    match !prefix with
    | None -> failwith "missing <public-url> in configuration"
    | Some x ->
        let x =
          if String.ends_with ~suffix:"/" x then
            String.sub x 0 (String.length x - 1)
          else x
        in
        Web_config.prefix := x

  let () =
    match !tos with
    | None -> failwith "You must provide a ToS URI"
    | Some x -> Web_config.tos := x

  (** Parse configuration from other sources *)

  let source_file =
    Lwt_main.run
      (match !source_file with
      | Some f ->
          let* b = Filesystem.file_exists f in
          if b then return f
          else Printf.ksprintf failwith "file %s does not exist" f
      | None -> failwith "missing <source> in configuration")

  let share_dir =
    match !share_dir with
    | Some d -> d
    | None -> failwith "missing <share> in configuration"

  let default_group =
    Lwt_main.run
      (match !default_group_file with
      | None -> failwith "missing <default-group> in configuration"
      | Some (`Group x) -> return x)

  let nh_group =
    Lwt_main.run
      (match !nh_group_file with
      | None -> failwith "missing <nh-group> in configuration"
      | Some (`Group x) -> return x)

  let domain =
    match !domain with
    | Some d -> d
    | None -> failwith "missing <domain> in configuration"

  (** Build up the site *)

  let () = Web_config.source_file := source_file
  let () = Web_config.share_dir := share_dir
  let () = Web_config.default_group := default_group
  let () = Web_config.nh_group := nh_group
  let () = Web_config.site_auth_config := List.rev !auth_instances
  let () = Web_config.exported_auth_config := List.rev !auth_instances_export
  let () = Web_config.domain := domain

  (** Restricted mode checks *)

  let () =
    let@ () = fun cont -> if !Web_config.restricted_mode then cont () in
    if Version.debug then failwith "debug build not allowed in restricted mode";
    if !Web_config.default_group <> "Ed25519" then
      failwith "default group must be Ed25519 in restricted mode";
    if
      not
        (List.for_all
           (fun x -> x.auth_system = "password")
           !Web_config.site_auth_config)
    then
      failwith
        "only password authentication is allowed for administrators in \
         restricted mode";
    if
      List.exists
        (fun x -> List.mem_assoc "allowsignups" x.auth_config)
        !Web_config.site_auth_config
    then failwith "allowsignups is not allowed in restricted mode";
    if
      not
        (List.for_all
           (function `Export x -> x.auth_system = "email" | _ -> false)
           !Web_config.exported_auth_config)
    then
      failwith
        "only email authentication is allowed for voters in restricted mode"

  module X : Pages_sig.S = struct
    module Mails_admin = Belenios_ui.Mails_admin.Make (Web_i18n)
    module Web_state = Web_state.Make ()
    module Web_services = Web_services.Make ()

    let () =
      match !rewrite_prefix with
      | None -> ()
      | Some (src, dst) -> Web_services.set_rewrite_prefix ~src ~dst

    module Web_i18n = Web_i18n.Make ()
    module Pages_common = Pages_common.Make (Web_i18n) (Web_services)

    module Pages_admin =
      Pages_admin.Make (Web_state) (Web_i18n) (Web_services) (Pages_common)
        (Mails_admin)

    module Pages_voter =
      Pages_voter.Make (Web_state) (Web_i18n) (Web_services) (Pages_common)
  end

  module Api = Api_eliom.Make ()
  module Web_captcha = Web_captcha.Make (X.Web_services)
  module Web_cont = Web_cont.Make (X.Web_services)

  module Web_auth =
    Web_auth.Make (X.Web_state) (X.Web_services) (X.Pages_common)

  module Web_auth_dummy =
    Web_auth_dummy.Make (X.Web_services) (X.Pages_common) (Web_auth)

  module Web_auth_password =
    Web_auth_password.Make (X.Web_services) (X.Pages_common) (Web_auth)

  module Web_auth_email =
    Web_auth_email.Make (X.Web_i18n) (X.Web_state) (X.Web_services)
      (X.Pages_common)
      (Web_auth)

  module Web_auth_cas = Web_auth_cas.Make (X.Web_services) (Web_auth)
  module Web_auth_oidc = Web_auth_oidc.Make (X.Web_services) (Web_auth)
  module Site_common = Site_common.Make (X)
  module Site_admin = Site_admin.Make (X) (Site_common) (Web_cont) (Web_auth)
  module Site_voter = Site_voter.Make (X) (Web_auth) (Site_common)

  let () = Api_elections.direct_voter_auth := Web_auth.direct_voter_auth
  let () = Api_elections.state_module := Some (module Web_auth.State)
  let () = Lwt.async Mails_voter_bulk.process_bulk_emails
end

let main () =
  let module M = Make () in
  ()
