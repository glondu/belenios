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

open Belenios_core.Signatures
open Web_serializable_t
open Belenios_api.Serializable_t
open Api_generic

val get_election_status : uuid -> election_status Lwt.t

val open_election : uuid -> bool Lwt.t
val close_election : uuid -> bool Lwt.t

val set_election_auto_dates : uuid -> election_auto_dates -> unit Lwt.t

val compute_encrypted_tally :
  (module Site_common_sig.ELECTION_LWT) -> metadata -> bool Lwt.t
val finish_shuffling :
  (module Site_common_sig.ELECTION_LWT) -> metadata -> bool Lwt.t
val release_tally :
  (module Site_common_sig.ELECTION_LWT) ->
  (unit, [`Forbidden | `CombinationError of combination_error]) Stdlib.result Lwt.t

val archive_election : uuid -> unit Lwt.t
val delete_election :
  (module Site_common_sig.ELECTION_LWT) -> metadata -> unit Lwt.t

val regenpwd :
  (module Site_common_sig.ELECTION_LWT) -> metadata -> string -> bool Lwt.t

val set_postpone_date : uuid -> float option -> bool Lwt.t

val get_partial_decryptions : uuid -> metadata -> partial_decryptions Lwt.t
val get_shuffles : uuid -> metadata -> shuffles Lwt.t

val dispatch :
  string option -> string list -> [`GET | `POST | `PUT | `DELETE] ->
  body -> result Lwt.t
