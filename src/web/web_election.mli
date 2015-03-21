(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Serializable_builtin_t
open Serializable_t
open Signatures
open Web_serializable_t
open Web_signatures

module type REGISTRATION = sig
  module W : WEB_ELECTION_
  module Register (X : EMPTY) : ELECTION_HANDLERS
end

module type REGISTRABLE = sig
  module W : sig
    include ELECTION_DATA
    include WEB_PARAMS
    module E : ELECTION with type elt = G.t
  end
  module Register (X : EMPTY) : REGISTRATION
end

module Make (D : ELECTION_DATA) (P : WEB_PARAMS) : REGISTRABLE

val login : (module WEB_ELECTION) -> string option -> unit -> content
val logout : (module WEB_ELECTION) -> unit -> unit -> content
val home : (module WEB_ELECTION) -> unit -> unit -> content
val admin : (module WEB_ELECTION) -> user option -> bool -> unit -> unit -> content
val election_dir : (module WEB_ELECTION) -> user option -> Web_common.election_file -> unit -> content
val election_update_credential : (module WEB_ELECTION) -> user option -> unit -> unit -> content
val election_update_credential_post : (module WEB_ELECTION) -> user option -> unit -> string * string -> content
val election_vote : (module WEB_ELECTION) -> unit -> unit -> content
val election_cast : (module WEB_ELECTION) -> unit -> unit -> content
val election_cast_post : (module WEB_ELECTION) -> unit -> string option * Eliom_lib.file_info option -> content
val election_cast_confirm : (module WEB_ELECTION) -> unit -> unit -> content
val election_pretty_ballots : (module WEB_ELECTION) -> int -> unit -> content
val election_pretty_ballot : (module WEB_ELECTION) -> string -> unit -> content
