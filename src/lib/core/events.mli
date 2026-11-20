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

open Common_types

type event_type =
  [ `Setup
  | `Ballot
  | `EndBallots
  | `EncryptedTally
  | `Shuffle
  | `EndShuffles
  | `PartialDecryption
  | `Result ]
[@@deriving desc]

type setup_data = {
  election : hash;
  trustees : hash;
  credentials : hash;
  credentials_certificate : hash option;
}
[@@deriving desc]

type event = {
  parent : hash option;
  height : int;
  typ : event_type; [@key "type"]
  payload : hash option;
}
[@@deriving desc]

type last_event = { height : int; hash : hash; pos : int64 } [@@deriving desc]

type roots = {
  setup_data : hash option;
  last_ballot_event : hash option;
  encrypted_tally : hash option;
  last_shuffle_event : hash option;
  last_pd_event : hash option;
  result : hash option;
}
[@@deriving desc]

val empty_roots : roots
val update_roots : hash -> event -> roots -> roots

val is_valid_transition : event_type option -> event_type -> bool
(** [is_valid_transition prev next] tells whether an event of type [next] may
    follow an event of type [prev] ([None] for the first event). *)
