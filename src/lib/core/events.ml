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

open Ppx_yojson_conv_lib.Yojson_conv
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

let empty_roots =
  {
    setup_data = None;
    encrypted_tally = None;
    result = None;
    last_ballot_event = None;
    last_shuffle_event = None;
    last_pd_event = None;
  }

let update_roots h event accu =
  match event.typ with
  | `Ballot -> { accu with last_ballot_event = Some h }
  | `PartialDecryption -> { accu with last_pd_event = Some h }
  | `Shuffle -> { accu with last_shuffle_event = Some h }
  | `Setup -> { accu with setup_data = event.payload }
  | `EncryptedTally -> { accu with encrypted_tally = event.payload }
  | `Result -> { accu with result = event.payload }
  | _ -> accu
