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

module type S = sig

val find_election :
  Web_serializable_t.uuid ->
  (module Belenios.Signatures.ELECTION_DATA) option Lwt.t

val election_not_found :
  unit -> Eliom_registration.Html.result Lwt.t

val redir_preapply :
  ('a, unit, Eliom_service.get, Eliom_service.att, 'b, 'c, 'd,
   [< `WithSuffix | `WithoutSuffix ], 'e, unit, 'f)
    Eliom_service.t -> 'a -> unit -> 'g Eliom_registration.kind Lwt.t

val wrap_handler :
  (unit -> Eliom_registration.Html.result Lwt.t) ->
  Eliom_registration.Html.result Lwt.t

end
