(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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

open Belenios_storage_api
open Belenios_server_core
open Types

module type INPUT = sig
  type session

  val get : session -> 'a file -> 'a Lopt.t Lwt.t
  val list_elections : session -> uuid list Lwt.t
  val with_transaction : (session -> 'a Lwt.t) -> 'a Lwt.t
end

module Make (_ : INPUT) () : sig
  module Clear : CLEAR

  val get_elections_by_owner : int -> Belenios_web_api.summary_list Lwt.t

  val get_next_actions :
    unit -> ([> `Archive | `Delete | `Destroy ] * uuid * float) list Lwt.t
end
