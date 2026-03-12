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

open Types
open Serializable_t
module E : ELECTION_TRANSACTION
module A : ACCOUNT_TRANSACTION

type 'a u = E.t -> uuid -> 'a

val get_user_id : user -> int option Lwt.t
val get_elections_by_owner : int -> Belenios_web_api.summary_list Lwt.t
val new_election : unit -> uuid option Lwt.t

(** Scoped transaction wrappers. Use these in preference to [with_transaction]
    so that the compiler can verify that the operations performed inside a
    transaction are consistent with the declared scope.

    [with_transaction] is kept for the rare cases where the scope cannot be
    determined statically (e.g. when the same closure may operate on either an
    election or an admin-password file depending on a runtime value). *)

val with_election_transaction : uuid -> (E.t -> 'a Lwt.t) -> 'a Lwt.t
(** Transaction scoped to a specific election. Only [Election (uuid, _)] files,
    [append], [append_sealing], [archive_election], [delete_election] and
    [validate_election] should be used inside. *)

val with_account_transaction : (A.t -> 'a Lwt.t) -> 'a Lwt.t
(** Transaction scoped to account / auth-db data. Only [Account _], [Auth_db _]
    and [Admin_password _] files and [new_account_id] should be used inside. *)

val register_backend :
  string -> (Xml.xml list -> (module STORAGE) Lwt.t) -> unit

val init_backend : string -> Xml.xml list -> unit Lwt.t
