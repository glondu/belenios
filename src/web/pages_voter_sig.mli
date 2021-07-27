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

open Belenios
open Serializable_builtin_t
open Serializable_t
open Web_serializable_t
open Signatures

module type S = sig

val election_home : (module ELECTION_DATA) -> election_state -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_raw : (module ELECTION_DATA) -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_confirmation : (module ELECTION_DATA) -> string -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val lost_ballot : (module ELECTION_DATA) -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val cast_confirmed : (module ELECTION_DATA) -> result:(string * Weight.t * bool, Web_common.error) result -> unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val pretty_ballots : (module ELECTION_DATA) -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val booth : unit -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val schulze : Question_nh_t.question -> schulze_result -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val majority_judgment_select : uuid -> int -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val majority_judgment : Question_nh_t.question -> mj_result -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val stv_select : uuid -> int -> [> `Html ] Eliom_content.Html.F.elt Lwt.t
val stv : Question_nh_t.question -> stv_result -> [> `Html ] Eliom_content.Html.F.elt Lwt.t

val generate_password :
  Web_serializable_t.metadata ->
  string list ->
  string -> uuid -> string -> string -> bool -> (string * string) Lwt.t

val generate_mail_credential :
  string list -> bool -> string ->
  login:string -> string -> Weight.t option -> string ->
  Web_serializable_t.metadata -> (string * string) Lwt.t

val mail_confirmation :
  (module Web_i18n_sig.GETTEXT) ->
  string -> string -> Weight.t option -> string ->
  bool -> string -> string -> Web_serializable_t.metadata -> string

end
