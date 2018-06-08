(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

val admin_gdpr : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val admin : elections:('a election list * 'a election list * 'a election list * (uuid * string) list) option -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val new_election_failure : [ `Exists | `Exception of exn ] -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val generic_page : title:string ->
  ?service:(unit, unit, [< Eliom_service.get_service_kind ],
           [< Eliom_service.attached ],
           [< Eliom_service.service_kind ], [< Eliom_service.suff ],
           'a, unit, [< Eliom_service.registrable ],
           [< Eliom_service.non_ocaml_service ])
    Eliom_service.service ->
  string -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val election_setup_pre : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup : uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_voters : uuid -> setup_election -> int -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_questions : uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_credential_authority : uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_credentials : string -> uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_trustees : uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_threshold_trustees : uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_trustee : string -> uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_threshold_trustee : string -> uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_import : uuid -> setup_election -> 'a election list * 'a election list * 'a election list -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_import_trustees : uuid -> setup_election -> 'a election list * 'a election list * 'a election list -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_setup_confirm : uuid -> setup_election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val election_home : 'a election -> Web_persist.election_state -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val election_admin : 'a election -> Web_serializable_j.metadata -> Web_persist.election_state -> (unit -> string list Lwt.t) -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val update_credential : 'a election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val regenpwd : uuid -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val cast_raw : 'a election -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val cast_confirmation : 'a election -> string -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val cast_confirmed : 'a election -> result:[< `Error of Web_common.error | `Valid of string ] -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val pretty_ballots : 'a election -> string list -> Yojson.Safe.json result option -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val pretty_records : 'a election -> (string * string) list -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val tally_trustees : 'a election -> int -> string -> unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val already_logged_in :
  unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val login_choose :
  string list ->
  (string -> (unit, unit, [< Eliom_service.get_service_kind ],
             [< Eliom_service.attached ], [< Eliom_service.service_kind ],
             [< Eliom_service.suff ], 'a, unit,
             [< Eliom_service.registrable ],
             [< Eliom_service.non_ocaml_service ])
            Eliom_service.service) ->
  unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val login_dummy : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val login_password : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val booth : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val contact_footer : metadata -> string -> string
