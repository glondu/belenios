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

open Serializable_j

let empty_roots = {
    roots_setup_data = None;
    roots_encrypted_tally = None;
    roots_result = None;
    roots_last_ballot_event = None;
    roots_last_shuffle_event = None;
    roots_last_pd_event = None;
}

let update_roots h event accu =
  match event.event_typ with
  | `Ballot -> {accu with roots_last_ballot_event = Some h}
  | `PartialDecryption -> {accu with roots_last_pd_event = Some h}
  | `Shuffle -> {accu with roots_last_shuffle_event = Some h}
  | `Setup -> {accu with roots_setup_data = event.event_payload}
  | `EncryptedTally -> {accu with roots_encrypted_tally = event.event_payload}
  | `Result -> {accu with roots_result = event.event_payload}
  | _ -> accu
