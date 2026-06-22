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
[@@deriving yojson]

type setup_data = {
  election : hash;
  trustees : hash;
  credentials : hash;
  credentials_certificate : hash option; [@yojson.option]
}
[@@deriving yojson]

type event = {
  parent : hash option; [@yojson.option]
  height : int;
  typ : event_type; [@key "type"]
  payload : hash option; [@yojson.option]
}
[@@deriving yojson]

type last_event = { height : int; hash : hash; pos : int64 } [@@deriving yojson]

type roots = {
  setup_data : hash option; [@yojson.option]
  last_ballot_event : hash option; [@yojson.option]
  encrypted_tally : hash option; [@yojson.option]
  last_shuffle_event : hash option; [@yojson.option]
  last_pd_event : hash option; [@yojson.option]
  result : hash option; [@yojson.option]
}
[@@deriving yojson]

val empty_roots : roots
val update_roots : hash -> event -> roots -> roots
