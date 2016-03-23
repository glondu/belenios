(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

open Web_signatures

type user = {
  uuid: Uuidm.t option;
  service : string;
  name : string;
  logout : unit -> content;
}

val user : user option Eliom_reference.eref
val get_site_user : unit -> Web_serializable_t.user option Lwt.t
val get_election_user : Uuidm.t -> Web_serializable_t.user option Lwt.t

val cont : (unit -> content) list Eliom_reference.eref
val cont_push : (unit -> content) -> unit Lwt.t
val cont_pop : unit -> (unit -> content) option Lwt.t

val ballot : string option Eliom_reference.eref
val cast_confirmed : [ `Error of Web_common.error | `Valid of string ] option Eliom_reference.eref
