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

open Serializable_t
open Web_serializable_t
open Signatures
open Web_common

val privacy_notice : privacy_cont -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val admin : elections:((uuid * string) list * (uuid * string) list * (uuid * string) list * (uuid * string) list) option -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val new_election_failure : [ `Exists | `Exception of exn ] -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val generic_page :
  title:string ->
  ?service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
            [< `WithSuffix | `WithoutSuffix ], 'e, unit,
            Eliom_service.non_ocaml)
    Eliom_service.t ->
  string -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val election_draft_pre : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft : uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_voters : uuid -> draft_election -> int -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_questions : uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_credential_authority : uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_credentials_done : draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_credentials : string -> uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_trustees : ?token:string -> uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_threshold_trustees : ?token:string -> uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_trustee : string -> uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_threshold_trustee : string -> uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_import : uuid -> draft_election -> (uuid * string) list * (uuid * string) list * (uuid * string) list -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_import_trustees : uuid -> draft_election -> (uuid * string) list * (uuid * string) list * (uuid * string) list -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_draft_confirm : uuid -> draft_election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val election_home : 'a election -> election_state -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val election_admin : ?shuffle_token:string -> ?tally_token:string -> 'a election -> Web_serializable_j.metadata -> election_state -> (unit -> string list Lwt.t) -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val regenpwd : uuid -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_raw : 'a election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_confirmation : 'a election -> string -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val lost_ballot : 'a election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_confirmed : 'a election -> result:(string, Web_common.error) result -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val pretty_ballots : 'a election -> string list -> Yojson.Safe.t election_result option -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val pretty_records : 'a election -> (string * string) list -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val election_shuffler_skip_confirm : uuid -> string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val shuffle : 'a election -> string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val tally_trustees : 'a election -> int -> string -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val login_choose :
  string list ->
  (string -> (unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
              [< `WithSuffix | `WithoutSuffix ], 'e, unit,
              Eliom_service.non_ocaml)
               Eliom_service.t) ->
  unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val login_dummy : state:string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val login_password : service:string -> allowsignups:bool -> state:string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val login_failed :
  service:(unit, unit, Eliom_service.get, 'a, 'b, 'c, 'd,
                    [< `WithSuffix | `WithoutSuffix ], 'e, unit,
                    Eliom_service.non_ocaml)
                   Eliom_service.t ->
  unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val signup_captcha : service:string -> captcha_error option -> string -> string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val signup_changepw : service:string -> captcha_error option -> string -> string -> string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val signup : string -> add_account_error option -> string -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val changepw : username:string -> address:string -> add_account_error option -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val booth : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val contact_footer : metadata -> string -> string
