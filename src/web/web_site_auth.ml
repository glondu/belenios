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

module C = struct
  let name = "site"
  let path = []
  let kind = `Site
end

module A = Web_auth.Make (C)
let configure = A.configure
include A.Services

open Eliom_registration
open Web_services

let login service () =
  lwt cont = Eliom_reference.get Web_services.cont in
  A.Handlers.login service cont ()

let logout () () =
  lwt cont = Eliom_reference.get Web_services.cont in
  A.Handlers.logout cont ()

let () = Any.register ~service:site_login login
let () = Any.register ~service:site_logout logout
