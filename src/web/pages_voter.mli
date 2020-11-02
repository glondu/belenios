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

open Belenios
open Serializable_t
open Web_serializable_t
open Signatures

val election_home : 'a election -> election_state -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_raw : 'a election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_confirmation : 'a election -> string -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val lost_ballot : 'a election -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_confirmed : 'a election -> result:(string, Web_common.error) result -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val pretty_ballots : 'a election -> string list -> Yojson.Safe.t election_result option -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val booth : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val booth_2 : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val schulze : Question_nh_t.question -> schulze_result -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val majority_judgment_select : uuid -> int -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val majority_judgment : Question_nh_t.question -> mj_result -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val generate_password :
  Web_serializable_t.metadata ->
  string list ->
  string -> string -> string -> (string * string) Lwt.t

val mail_credential :
  (module Web_i18n_sig.GETTEXT) ->
  string ->
  bool -> string -> string -> Web_serializable_t.metadata -> string

val mail_confirmation :
  (module Web_i18n_sig.GETTEXT) ->
  string -> string -> string ->
  bool -> string -> string -> Web_serializable_t.metadata -> string
