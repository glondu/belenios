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

open Belenios_api.Serializable_t
open Belenios_server_core
open Web_common

module type S = sig
  val privacy_notice : privacy_cont -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val admin_login :
    (string -> Web_auth_sig.result Lwt.t) ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val admin :
    elections:summary_list * summary_list * summary_list * summary_list ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val new_election_failure :
    [ `Exists | `Exception of exn ] ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_pre : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft :
    uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_voters :
    uuid ->
    draft_election ->
    int ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_questions :
    uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_credential_authority :
    uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_credentials_done :
    draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_credentials_already_generated :
    unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_credentials_static :
    unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_trustees :
    ?token:string ->
    uuid ->
    draft_election ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_threshold_trustees :
    ?token:string ->
    uuid ->
    draft_election ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_import :
    uuid ->
    draft_election ->
    summary_list * summary_list * summary_list ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_import_trustees :
    uuid ->
    draft_election ->
    summary_list * summary_list * summary_list ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_draft_confirm :
    (draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t)
    Storage.u

  val election_admin :
    ?shuffle_token:string ->
    ?tally_token:string ->
    Storage.t ->
    (module Site_common_sig.ELECTION) ->
    metadata ->
    election_status ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val regenpwd : uuid -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val pretty_records :
    Storage.t ->
    (module Site_common_sig.ELECTION) ->
    records ->
    unit ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val election_shuffler_skip_confirm :
    uuid -> string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val shuffle_static : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
  val tally_trustees_static : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup_captcha :
    service:string ->
    captcha_error option ->
    string ->
    string ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup_changepw :
    service:string ->
    captcha_error option ->
    string ->
    string ->
    string ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup_login : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val signup :
    string ->
    add_account_error option ->
    string ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val changepw :
    username:string ->
    address:string ->
    add_account_error option ->
    [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val compute_fingerprint : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
  val set_email : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val set_email_confirm :
    address:string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

  val sudo : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
  val account : account -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
end
