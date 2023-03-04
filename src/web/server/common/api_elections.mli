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

open Web_common
open Web_serializable_t
open Belenios_api.Serializable_t
open Api_generic

val get_election_status : uuid -> election_status Lwt.t
val get_election_automatic_dates : uuid -> election_auto_dates Lwt.t

val set_election_auto_dates : uuid -> election_auto_dates -> unit Lwt.t

val get_records : uuid -> records Lwt.t

val set_postpone_date : uuid -> float option -> bool Lwt.t

val get_partial_decryptions : uuid -> metadata -> partial_decryptions Lwt.t
val get_shuffles : uuid -> metadata -> shuffles Lwt.t

val skip_shuffler : uuid -> string -> unit Lwt.t
val select_shuffler : uuid -> metadata -> string -> unit Lwt.t

val dispatch :
  token:string option -> ifmatch:string option -> string list -> [`GET | `POST | `PUT | `DELETE] ->
  body -> result Lwt.t

val direct_voter_auth : (uuid -> Yojson.Safe.t -> user Lwt.t) ref

val cast_ballot :
  (uuid -> bool -> string -> string -> weight option -> string -> bool Lwt.t) ->
  (module Site_common_sig.ELECTION) -> rawballot:string -> user:user ->
  precast_data:(string * credential_record) ->
  (user * string * bool * weight * bool) Lwt.t
