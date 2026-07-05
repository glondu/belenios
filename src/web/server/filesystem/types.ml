(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
(*  Copyright © 2026 VCAST                                                *)
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
open Belenios
open Belenios_storage_api

type datetime = Datetime.t

let yojson_of_datetime x = `String (Datetime.unwrap x)

let datetime_of_yojson = function
  | `String x -> Datetime.wrap x
  | x -> of_yojson_error "string expected" x

type election_dates = {
  creation : int64;
  finalization : int64 option; [@yojson.option]
  tally : int64 option; [@yojson.option]
  archive : int64 option; [@yojson.option]
  last_mail : int64 option; [@yojson.option]
  auto_open : int64 option; [@yojson.option]
  auto_close : int64 option; [@yojson.option]
  grace_period : int64 option; [@yojson.option]
}
[@@deriving yojson]

type extended_record = { username : string; date : int64; credential : string }
[@@deriving yojson]

type ('a, 'b) raw_draft_election = {
  version : int;
  owners : int list;
  mutable group : string;
  mutable voters : draft_voter list;
  mutable questions : template;
  mutable trustees : ('a, 'b) draft_trustees;
  mutable metadata : metadata;
  public_creds : string;
  mutable public_creds_received : bool;
  mutable public_creds_certificate : ('a, 'b) credentials_certificate option;
      [@yojson.option]
  creation_date : int64;
  mutable administrator : string option; [@yojson.option]
  mutable credential_authority_visited : bool;
      [@default false] [@yojson_drop_default ( = )]
  mutable voter_authentication_visited : bool;
      [@default false] [@yojson_drop_default ( = )]
  mutable trustees_setup_step : int; [@default 1] [@yojson_drop_default ( = )]
  mutable pending_credentials : bool;
      [@default false] [@yojson_drop_default ( = )]
}
[@@deriving yojson]

type authentication_method = [ `CAS of string | `Email | `Unknown ]
[@@deriving yojson]

type credential_method = [ `Automatic | `Manual ] [@@deriving yojson]
type deleted_trustee = [ `Single | `Pedersen of int * int ] [@@deriving yojson]

type deleted_election = {
  uuid : uuid;
  template : template;
  owners : int list;
  nb_voters : int;
  nb_ballots : int;
  date : int64;
  tallied : bool;
  authentication_method : authentication_method;
  credential_method : credential_method;
  trustees : deleted_trustee list;
  has_weights : bool;
}
[@@deriving yojson]

type account = {
  id : int;
  name : string;
  email : string option; [@yojson.option]
  last_connected : int64;
  authentications : user list;
  consent : int64 option; [@yojson.option]
  capabilities : int option; [@yojson.option]
  language : string option; [@yojson.option]
  voters_limit : int option; [@yojson.option]
  preferences : Belenios_web_api.preferences;
}
[@@deriving yojson]
