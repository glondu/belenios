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

open Serializable_t
open Signatures
open Web_serializable_t
open Web_signatures

val string_of_user : user -> string

type instantiator = string -> (module AUTH_SERVICE) -> unit

val register_auth_system :
  spec:(Ocsigen_extensions.Configuration.element list) ->
  exec:(instantiate:instantiator -> unit) ->
  unit

val get_config_spec :
  unit -> Ocsigen_extensions.Configuration.element list

module Make (X : EMPTY) : sig
  module Services : AUTH_SERVICES
  module Register (S : CONT_SERVICE) (T : TEMPLATES) : EMPTY
end
