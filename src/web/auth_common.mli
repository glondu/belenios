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

open Web_signatures

type user = {
  user_type : string;
  user_name : string;
}

type logged_user = {
  user_admin : bool;
  user_user : user;
  user_logout : (module CONT_SERVICE);
}

val string_of_user : user -> string
val user : logged_user option Eliom_reference.eref

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
