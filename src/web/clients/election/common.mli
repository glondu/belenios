(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

open Js_of_ocaml_tyxml
open Tyxml_js.Html
open Belenios
open Belenios_web_api

type page = {
  title : string;
  contents : Html_types.div_content_fun elt list;
  footer : Html_types.div_content_fun elt list;
}

val error : string -> page

val make_audit_footer :
  (module Election.ELECTION) -> Html_types.div_content_fun elt

val clear_cache : unit -> unit
val get_election : uuid -> (module Election.ELECTION) option Lwt.t
val get_status : uuid -> election_status option Lwt.t
val get_dates : uuid -> election_auto_dates option Lwt.t
val get_audit_cache : uuid -> audit_cache option Lwt.t
val get_result : uuid -> string option Lwt.t
val get_sized_encrypted_tally : uuid -> hash sized_encrypted_tally option Lwt.t
val get_ballots : uuid -> (hash * weight) list option Lwt.t
