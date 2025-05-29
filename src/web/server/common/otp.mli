(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

module type SENDER = sig
  type payload
  type context

  val send :
    context:context ->
    recipient:Belenios_messages.recipient ->
    code:string ->
    (string, unit) result Lwt.t
end

module type S = sig
  type payload
  type context

  val generate :
    context:context ->
    recipient:Belenios_messages.recipient ->
    payload:payload ->
    (string, unit) result Lwt.t

  val check : address:string -> code:string -> payload option
end

module Make (I : SENDER) () :
  S with type payload = I.payload and type context = I.context
