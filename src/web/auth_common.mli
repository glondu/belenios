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

type user = {
  user_type : string;
  user_name : string;
}

type logged_user = {
  user_admin : bool;
  user_user : user;
}

val string_of_user : user -> string
val user : logged_user option Eliom_reference.eref

val logout :
  (unit, unit,
   [> `Attached of
        ([> `Internal of [> `Service ] ], [> `Get ])
        Eliom_service.a_s ],
   [ `WithoutSuffix ], unit, unit,
   [< Eliom_service.registrable > `Registrable ], 'a)
  Eliom_service.service

open Web_signatures

module Make (C : AUTH_CONFIG) (S : MAIN_SERVICES) (T : TEMPLATES) : AUTH_SERVICES
