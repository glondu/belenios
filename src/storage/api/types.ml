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

open Belenios
open Belenios_web_api

(** {1 Web-specific types} *)

type user = { domain : string; name : string } [@@deriving desc]

type auth_config = {
  auth_system : string;
  auth_instance : string;
  auth_config : (string * string) list;
  auth_portal : string option;
}
[@@deriving desc]

type sealed = {
  date_open : int64 option;
  date_close : int64 option;
  date_publish : int64 option;
}
[@@deriving desc]

type sealing_op = [ `Seal of sealed | `Unseal ] [@@deriving desc]
type sealing_event = { date : int64; op : sealing_op } [@@deriving desc]

type metadata = {
  owners : int list;
  auth_config : auth_config list option;
  cred_authority_info : cred_authority_info option;
  trustees : uuid option;
  languages : string list option;
  contact : string option;
  booth_version : int option;
  billing_request : string option;
  sealed : bool option;
  logo : string option;
}
[@@deriving desc]

type extended_record = { username : string; date : int64; credential : string }
[@@deriving desc]

type credential_mapping = { credential : string; ballot : hash }
[@@deriving desc]

type credential_dynamic_record = { ballot : hash } [@@deriving desc]

type credential_dynamic_records = credential_dynamic_record option hmap
[@@deriving desc]

type election_dynamic_records = election_record option hmap [@@deriving desc]

type election_state =
  [ `Draft
  | `Open
  | `Closed
  | `Shuffling
  | `EncryptedTally
  | `Tallied
  | `Archived ]
[@@deriving desc]

type skipped_shufflers = string list [@@deriving desc]

type shuffle_token = {
  trustee : external_trustee;
  trustee_id : int;
  name : string;
}
[@@deriving desc]

type shuffle_state = {
  skipped : skipped_shufflers;
  token : shuffle_token option;
}
[@@deriving desc]

type stored_election_state_without_shuffling =
  [ `Open | `Closed | `EncryptedTally | `Tallied ]

type stored_election_state =
  [ `Open | `Closed | `Shuffling of shuffle_state | `EncryptedTally | `Tallied ]
[@@deriving desc]

let to_election_state = function
  | #stored_election_state_without_shuffling as x -> x
  | `Shuffling _ -> `Shuffling

(** {1 Types related to elections being prepared} *)

type ('a, 'b) draft_basic_trustee = {
  address : string;
  name : string;
  token : string;
  mutable parameters : ('a, 'b) basic_parameters option;
}
[@@deriving desc]

type ('a, 'b) draft_threshold_trustee = {
  address : string;
  name : string;
  token : string;
  mutable step : int option;
  mutable cert : ('a, 'b) pedersen_cert option;
  mutable polynomial : ('a, 'b) polynomial option;
  mutable vinput : ('a, 'b) vinput option;
  mutable voutput : ('a, 'b) voutput option;
}
[@@deriving desc]

type ('a, 'b) draft_basic_params = {
  mutable trustees : ('a, 'b) draft_basic_trustee list;
}
[@@deriving desc]

type ('a, 'b) draft_threshold_params = {
  algorithm : string;
  mutable threshold : int option;
  mutable trustees : ('a, 'b) draft_threshold_trustee list;
  mutable parameters : ('a, 'b) threshold_parameters option;
  mutable error : string option;
}
[@@deriving desc]

type ('a, 'b) draft_trustees_mode =
  [ `Basic of ('a, 'b) draft_basic_params
  | `Threshold of ('a, 'b) draft_threshold_params ]
[@@deriving desc]

type ('a, 'b) draft_trustees = {
  step : int;
  mode : ('a, 'b) draft_trustees_mode;
}
[@@deriving desc]

type ('a, 'b) raw_draft_election = {
  version : int;
  mutable group : string;
  mutable questions : template;
  public_creds : string;
  mutable public_creds_received : bool;
  mutable public_creds_certificate : ('a, 'b) credentials_certificate option;
  mutable credential_authority_visited : bool; [@default false]
  mutable voter_authentication_visited : bool; [@default false]
  mutable pending_credentials : bool; [@default false]
  mutable private_creds_downloaded : bool; [@default false]
}
[@@deriving desc]

(** {1 Administrator accounts} *)

type account = {
  id : int;
  name : string;
  email : string option;
  last_connected : int64;
  authentications : user list;
  consent : int64 option;
  capabilities : int option;
  language : string option;
  voters_limit : int option;
  preferences : preferences;
}
[@@deriving desc]

(** {1 Views} *)

type username_or_address = [ `Username | `Address ] [@@deriving desc]

type voters_config = {
  has_explicit_weights : bool;
  username_or_address : username_or_address;
  nb_voters : int;
  bits : int;
}
[@@deriving desc]

type password_record = Belenios_spool.Passwords.record = {
  username : string;
  salt : string;
  hashed : string;
  address : string option;
}

let password_record_d = Belenios_spool.Passwords.record_d

(** {1 Running elections} *)

type election_dates = {
  creation : int64;
  finalization : int64 option;
  tally : int64 option;
  archive : int64 option;
  last_mail : int64 option;
  auto_open : int64 option;
  auto_close : int64 option;
  publish : int64 option;
  grace_period : int64 option;
}
[@@deriving desc]

type credentials_seed = { seed : string; token : string } [@@deriving desc]

type ('a, 'b) credentials_params = {
  belenios_url : string;
  version : int;
  group : string;
  certificate : ('a, 'b) credentials_certificate;
}
[@@deriving desc]

type wrapped_credentials_params =
  | W :
      ('a, 'b) group * ('a, 'b) credentials_params
      -> wrapped_credentials_params

(* Decoded in two passes: the group, then the parameters in this group. *)
let wrapped_credentials_params_d : wrapped_credentials_params Desc.Core.t =
  let of_json x =
    let { version; group; _ } =
      Desc.Json.of_json (credentials_params_d Desc.Core.json Desc.Core.json) x
    in
    let module G = (val Group.make { version; group }) in
    let x = Desc.Json.of_json [%desc_of_group: _ credentials_params] x in
    W ((module G), x)
  and to_json (W (w, x)) =
    let module G = (val w) in
    Desc.Json.to_json [%desc_of_group: _ credentials_params] x
  in
  Desc.Core.custom "wrapped_credentials_params" to_json of_json

type 'a credentials_record = {
  login : string;
  credential : 'a;
  address : string option;
  weight : weight option;
}
[@@deriving desc]

type ('a, 'b) credentials_records_item =
  ('a, 'b, string) encrypted_msg credentials_record
[@@deriving desc]

type ('a, 'b) credentials_records_object =
  ('a, 'b) credentials_records_item smap
[@@deriving desc]

type ('a, 'b) credentials_records = {
  algorithm : string;
  records : ('a, 'b) credentials_records_object;
}
[@@deriving desc]

(** {1 Contents served as a stream} *)

type file_stream = { length : int64 option; stream : string Lwt_stream.t }
