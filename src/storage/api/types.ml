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

(** {1 Predefined types} *)

open Ppx_yojson_conv_lib.Yojson_conv
open Belenios
open Belenios_web_api

(** {1 Web-specific types} *)

type user = { domain : string; name : string } [@@deriving yojson]

type auth_config = {
  auth_system : string;
  auth_instance : string;
  auth_config : (string * string) list;
  auth_portal : string option; [@yojson.option]
}
[@@deriving yojson]

type sealed = {
  date_open : int64 option; [@yojson.option]
  date_close : int64 option; [@yojson.option]
  date_publish : int64 option; [@yojson.option]
}
[@@deriving yojson]

type sealing_op = [ `Seal of sealed | `Unseal ] [@@deriving yojson]
type sealing_event = { date : int64; op : sealing_op } [@@deriving yojson]

type metadata = {
  owners : int list;
  auth_config : auth_config list option; [@yojson.option]
  cred_authority_info : cred_authority_info option; [@yojson.option]
  trustees : external_trustee option list option; [@yojson.option]
  languages : string list option; [@yojson.option]
  contact : string option; [@yojson.option]
  booth_version : int option; [@yojson.option]
  billing_request : string option; [@yojson.option]
  sealed : bool option; [@yojson.option]
  logo : string option; [@yojson.option]
}
[@@deriving yojson]

type extended_record = { username : string; date : int64; credential : string }
[@@deriving yojson]

type credential_mapping = {
  credential : string;
  ballot : string option; [@yojson.option]
}
[@@deriving yojson]

type election_state =
  [ `Draft
  | `Open
  | `Closed
  | `Shuffling
  | `EncryptedTally
  | `Tallied
  | `Archived ]
[@@deriving yojson]

(** {1 Types related to elections being prepared} *)

type draft_voter = { mutable id : voter } [@@deriving yojson]

type ('a, 'b) draft_basic_trustee_kind =
  | Server of { seed : string }
  | External of { id : string; token : string; name : string }
[@@deriving yojson]

type ('a, 'b) draft_basic_trustee = {
  kind : ('a, 'b) draft_basic_trustee_kind;
  mutable parameters : ('a, 'b) basic_parameters option; [@yojson.option]
}
[@@deriving yojson]

type ('a, 'b) draft_threshold_trustee = {
  id : string;
  token : string;
  mutable step : int option; [@yojson.option]
  mutable cert : ('a, 'b) pedersen_cert option; [@yojson.option]
  mutable polynomial : ('a, 'b) polynomial option; [@yojson.option]
  mutable vinput : ('a, 'b) vinput option; [@yojson.option]
  mutable voutput : ('a, 'b) voutput option; [@yojson.option]
  name : string;
}
[@@deriving yojson]

type ('a, 'b) draft_basic_params = {
  mutable trustees : ('a, 'b) draft_basic_trustee list;
}
[@@deriving yojson]

type ('a, 'b) draft_threshold_params = {
  algorithm : string;
  mutable threshold : int option; [@yojson.option]
  mutable trustees : ('a, 'b) draft_threshold_trustee list;
  mutable parameters : ('a, 'b) threshold_parameters option; [@yojson.option]
  mutable error : string option; [@yojson.option]
}
[@@deriving yojson]

type ('a, 'b) draft_trustees =
  [ `Basic of ('a, 'b) draft_basic_params
  | `Threshold of ('a, 'b) draft_threshold_params ]
[@@deriving yojson]

type ('a, 'b) raw_draft_election = {
  version : int;
  owners : int list;
  mutable group : string;
  mutable voters : draft_voter list;
  mutable questions : template;
  mutable trustees : ('a, 'b) draft_trustees;
  public_creds : string;
  mutable public_creds_received : bool;
  mutable public_creds_certificate : ('a, 'b) credentials_certificate option;
      [@yojson.option]
  mutable credential_authority_visited : bool;
      [@default false] [@yojson_drop_default ( = )]
  mutable voter_authentication_visited : bool;
      [@default false] [@yojson_drop_default ( = )]
  mutable trustees_setup_step : int; [@default 1] [@yojson_drop_default ( = )]
  mutable pending_credentials : bool;
      [@default false] [@yojson_drop_default ( = )]
  mutable private_creds_downloaded : bool;
      [@default false] [@yojson_drop_default ( = )]
}
[@@deriving yojson]

(** {1 Administrator accounts} *)

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
  preferences : preferences;
}
[@@deriving yojson]

(** {1 Views} *)

type username_or_address = [ `Username | `Address ] [@@deriving yojson]

type voters_config = {
  has_explicit_weights : bool;
  username_or_address : username_or_address;
  nb_voters : int;
}
[@@deriving yojson]

type password_record = {
  username : string;
  salt : string;
  hashed : string;
  address : string option; [@yoson.option]
}
[@@deriving yojson]

(** {1 Running elections} *)

type election_dates = {
  creation : int64;
  finalization : int64 option; [@yoson.option]
  tally : int64 option; [@yoson.option]
  archive : int64 option; [@yoson.option]
  last_mail : int64 option; [@yoson.option]
  auto_open : int64 option; [@yoson.option]
  auto_close : int64 option; [@yoson.option]
  publish : int64 option; [@yoson.option]
  grace_period : int64 option; [@yoson.option]
}
[@@deriving yojson]

type election_records = (string * int64) list

let yojson_of_election_records x : json =
  `Assoc (List.map (fun (k, v) -> (k, yojson_of_int64 v)) x)

let election_records_of_yojson : json -> election_records = function
  | `Assoc o -> List.map (fun (k, v) -> (k, int64_of_yojson v)) o
  | x -> of_yojson_error "object expected" x

type skipped_shufflers = string list [@@deriving yojson]

type shuffle_token = {
  trustee : external_trustee;
  trustee_id : int;
  name : string;
}
[@@deriving yojson]

type shuffle_state = {
  skipped : skipped_shufflers;
  token : shuffle_token option; [@yoson.option]
}
[@@deriving yojson]

type some_state_state = [ `Decryption | `Shuffle of shuffle_state ]
[@@deriving yojson]

type state_state = some_state_state option [@@deriving yojson]
type credentials_seed = { seed : string; token : string } [@@deriving yojson]

type ('a, 'b) credentials_params = {
  belenios_url : string;
  version : int;
  group : string;
  certificate : ('a, 'b) credentials_certificate;
}
[@@deriving yojson]

type wrapped_credentials_params =
  | W :
      ('a, 'b) group * ('a, 'b) credentials_params
      -> wrapped_credentials_params

let wrapped_credentials_params_of_yojson : json -> wrapped_credentials_params =
 fun x ->
  let x' = credentials_params_of_yojson Fun.id Fun.id x in
  let module G = (val Group.of_string ~version:x'.version x'.group) in
  let x = [%group_of_yojson: _ credentials_params] x in
  W ((module G), x)

let yojson_of_wrapped_credentials_params : wrapped_credentials_params -> json =
 fun (W (w, x)) ->
  let module G = (val w) in
  [%yojson_of_group: _ credentials_params] x

type 'a credentials_record = {
  credential : 'a;
  address : string option; [@yojson.option]
  weight : weight option; [@yojson.option]
}
[@@deriving yojson]

type ('a, 'b) credentials_records_item =
  ('a, 'b, string) encrypted_msg credentials_record
[@@deriving yojson]

type ('a, 'b) credentials_records_object =
  (string * ('a, 'b) credentials_records_item) list

let yojson_of_credentials_records_object a b x : json =
  `Assoc
    (List.map (fun (k, v) -> (k, yojson_of_credentials_records_item a b v)) x)

let credentials_records_object_of_yojson a b :
    json -> _ credentials_records_object = function
  | `Assoc o ->
      List.map (fun (k, v) -> (k, credentials_records_item_of_yojson a b v)) o
  | x -> of_yojson_error "object expected" x

type ('a, 'b) credentials_records = {
  algorithm : string;
  records : ('a, 'b) credentials_records_object;
}
[@@deriving yojson]
