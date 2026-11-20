(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

(** {1 API-specific serializable datatypes} *)

open Belenios

type lang = Belenios.Language.t

type message_metadata = {
  uuid : uuid;
  admin_id : int;
  title : string;
  contact : string option;
  has_weights : bool;
  has_passwords : bool;
  langs : string list;
}
[@@deriving desc]

type restricted_mode_error =
  [ `AutoCredentials
  | `VoterAuthentication
  | `ForbiddenQuestions
  | `NotEnoughTrustees
  | `NoThreshold
  | `HasWeights
  | `BadGroup ]
[@@deriving desc]

type missing_billing = { url : string; id : string; callback : string }
[@@deriving desc]

type validation_error =
  [ `NoTitle
  | `NoQuestions
  | `NoAdministrator
  | `NoCredentialAuthority
  | `NoVoters
  | `TooManyVoters
  | `WrongLength
  | `MissingPublicCredentials
  | `TrusteesNotReady
  | `WeightsAreIncompatibleWithNH
  | `NotSinglePrivateKey
  | `KeyEstablishmentNotFinished
  | `MissingBilling of missing_billing
  | `RestrictedMode of restricted_mode_error
  | `CredentialServerError of int
  | `BadAuthentication ]
[@@deriving desc]

type voter_list_error =
  [ `BadVoter of voter
  | `Duplicate of string
  | `TotalWeightTooBig of weight * weight ]
[@@deriving desc]

type error =
  [ `GenericError of string
  | `NotInExpectedState
  | `CannotChange of string
  | `Invalid of string
  | `Unsupported of string
  | `Missing of string
  | `MissingVoter of string
  | `ValidationError of validation_error
  | `InvalidPublicKey
  | `PublicKeyExists
  | `CastError of cast_error
  | `VoterListError of voter_list_error
  | `ReadonlyStorage ]
[@@deriving desc]

type request_status = { code : int; status : string; error : error }
[@@deriving desc]

type configured_authentication = {
  instance : string;
  system : string;
  descr : string option;
}
[@@deriving desc]

type authentication_system = [ `CAS | `Configured of configured_authentication ]
[@@deriving desc]

type configuration_uris = { home : string; belenios : string; tos : string }
[@@deriving desc]

type languages = (lang * string) list

let languages_d : languages Desc.Core.t =
  Desc.Core.map
    (Desc.Core.assoc Desc.Core.string)
    (List.map (fun (k, v) -> (Belenios.Language.wrap k, v)))
    (List.map (fun (k, v) -> (Belenios.Language.unwrap k, v)))

let yojson_of_languages = Desc.Json.to_json languages_d
let languages_of_yojson = Desc.Json.of_json languages_d

type configuration = {
  restricted_mode : bool;
  vendor : string;
  tos_last_update : int64;
  uris : configuration_uris;
  belenios_version : string;
  belenios_build : string;
  spec_version : string;
  api_version : int;
  supported_crypto_versions : int list;
  supported_booth_versions : int list;
  authentications : authentication_system list;
  default_group : string;
  default_nh_group : string;
  max_voters : int;
  languages : languages;
  election_sealing : bool;
  grace_period : bool;
}
[@@deriving desc]

type authentication_method = {
  service : string;
  username : string;
  portal : string option;
}
[@@deriving desc]

type preferences = { languages : string list; contact : string }
[@@deriving desc]

type api_account = {
  id : int;
  authentication_method : authentication_method;
  name : string;
  address : string option;
  language : string option;
  voters_limit : int option;
  preferences : preferences;
}
[@@deriving desc]

type authentication = [ `CAS of string | `Configured of string ]
[@@deriving desc]

type cred_authority_info = { server : string; operator : string }
[@@deriving desc]

type raw_draft = {
  version : int;
  owners : int list;
  questions : template;
  languages : string list;
  contact : string option;
  booth : int;
  authentication : authentication option;
  group : string;
  cred_authority_info : cred_authority_info option;
}
[@@deriving desc]

type ('a, 'b) pedersen = {
  context : full_context;
  step : int;
  certs : ('a, 'b) pedersen_cert array;
  vinput : ('a, 'b) vinput option;
  voutput : ('a, 'b) voutput option;
}
[@@deriving desc]

type ('a, 'b) trustee_status_basic = [ `Init of string | `Done of 'a ]
[@@deriving desc]

type ('a, 'b) trustee_status_threshold =
  [ `Init
  | `WaitingForCertificate of full_context
  | `WaitingForOtherCertificates of 'a
  | `Pedersen of ('a, 'b) pedersen ]
[@@deriving desc]

type ('a, 'b) trustee_status_draft =
  [ `Basic of ('a, 'b) trustee_status_basic
  | `Threshold of ('a, 'b) trustee_status_threshold ]
[@@deriving desc]

type state =
  [ `Draft
  | `Open
  | `Closed
  | `Shuffling
  | `EncryptedTally
  | `Tallied
  | `Archived ]
[@@deriving desc]

type summary = { uuid : uuid; name : string; date : int64; state : state }
[@@deriving desc]

type summary_list = summary list [@@deriving desc]
type string_list = string list [@@deriving desc]
type addable_trustee = { name : string; address : string } [@@deriving desc]

type draft_status = {
  num_voters : int;
  credentials_ready : bool;
  credentials_left : int option;
  private_credentials_downloaded : bool option;
  trustees_ready : bool;
  nh_and_weights_compatible : bool;
  credential_authority_visited : bool;
  voter_authentication_visited : bool;
  restricted_mode_error : restricted_mode_error option;
}
[@@deriving desc]

type draft_request =
  [ `SetDownloaded
  | `SetTrustees of uuid option
  | `ImportTrustees of uuid
  | `ValidateElection
  | `SetCredentialAuthorityVisited
  | `SetVoterAuthenticationVisited
  | `InitiateCredentialAuthorityProtocol ]
[@@deriving desc]

type credentials_new_request = {
  belenios_url : string;
  uuid : uuid;
  info : cred_authority_info;
  token : string;
  admin_id : int;
}
[@@deriving desc]

type credentials_validate = {
  uuid : uuid;
  token : string;
  metadata : message_metadata;
}
[@@deriving desc]

type resend_spec =
  [ `All_voters | `Missing_voters | `Some_voters of string list ]
[@@deriving desc]

type credentials_resend = { uuid : uuid; seed : string; spec : resend_spec }
[@@deriving desc]

type credentials_request =
  [ `NewRequest of credentials_new_request
  | `Validate of credentials_validate
  | `Resend of credentials_resend ]
[@@deriving desc]

type ('a, 'b) credentials_response = {
  certificate : ('a, 'b) credentials_certificate;
  token : string;
  public_credentials : 'a public_credentials_with_id;
}
[@@deriving desc]

type abstract_resend_spec = [ `All_voters | `Missing_voters | `Some_voters ]
[@@deriving desc]

type credentials_credit_kind =
  [ `Initial | `Resend of abstract_resend_spec | `Other of string ]
[@@deriving desc]

type credentials_credit = {
  credits : int;
  success : bool;
  kind : credentials_credit_kind;
  timestamp : int64;
}
[@@deriving desc]

type credentials_credits = credentials_credit list [@@deriving desc]

type ('a, 'b) trustees_status_trustee = {
  name : string;
  step : int;
  address : string option;
  cert_verification_key : 'a option;
}
[@@deriving desc]

type trustees_status_mode = [ `Basic | `Threshold of int ] [@@deriving desc]

type ('a, 'b) trustees_status = {
  version : int;
  group : string;
  step : int;
  validated : bool;
  mode : trustees_status_mode;
  trustees : ('a, 'b) trustees_status_trustee list;
}
[@@deriving desc]

type wrapped_trustees_status =
  | Wrapped_trustees_status :
      ('a, 'b) group * ('a, 'b) trustees_status
      -> wrapped_trustees_status

let wrapped_trustees_status_of_yojson x =
  let { version; group; _ } = trustees_status_of_yojson Fun.id Fun.id x in
  let module G = (val Group.make { version; group }) in
  let x = [%group_of_yojson: _ trustees_status] x in
  Wrapped_trustees_status ((module G), x)

let yojson_of_wrapped_trustees_status (Wrapped_trustees_status (w, x)) =
  let module G = (val w) in
  [%yojson_of_group: _ trustees_status] x

type send_link_payload = { address : string; langs : string list }
[@@deriving desc]

type trustees_root_request =
  | Create of { group : group_specification; owners : int list }
[@@deriving desc]

type trustees_uuid_request =
  [ `SetStep of int
  | `AddTrustee of addable_trustee
  | `SendLink of send_link_payload
  | `RemoveTrustee of string
  | `SetBasic
  | `SetThreshold of int
  | `Validate
  | `Reset ]
[@@deriving desc]

type voters_request =
  [ `Add of voter_list | `Remove of string | `Import of uuid ]
[@@deriving desc]

type election_status = {
  state : state;
  trustees : uuid option;
  authentication : authentication option;
  auto_delete_date : int64;
  auto_archive_date : int64 option;
  sealed : bool;
}
[@@deriving desc]

type election_auto_dates = {
  open_ : int64 option; [@key "open"]
  close : int64 option;
  publish : int64 option;
  grace_period : int64 option;
}
[@@deriving desc]

type election_record = { timestamp : int64 } [@@deriving desc]
type election_records = election_record option smap [@@deriving desc]

type admin_request =
  [ `Open
  | `Close
  | `ComputeEncryptedTally
  | `FinishShuffling
  | `ReleaseTally
  | `Archive
  | `Seal of bool ]
[@@deriving desc]

type trustee_pd = {
  address : string;
  token : string;
  done_ : bool; [@key "done"]
}
[@@deriving desc]

type partial_decryptions = {
  trustees : trustee_pd list;
  threshold : int option;
}
[@@deriving desc]

type ('a, 'b) tally_trustee_content = {
  algorithm : string;
  private_key : ('a, 'b) sent_partial_decryption_key;
}
[@@deriving desc]

type ('a, 'b) tally_trustee = ('a, 'b) tally_trustee_content option
[@@deriving desc]

type ('a, 'b) trustee_status_ready = {
  cert_verification_key : 'a;
  elections : uuid list;
}
[@@deriving desc]

type ('a, 'b) trustees_trustee_status_status =
  [ `Draft of ('a, 'b) trustee_status_draft
  | `Ready of ('a, 'b) trustee_status_ready ]
[@@deriving desc]

type ('a, 'b) trustees_trustee_status = {
  index : int;
  status : ('a, 'b) trustees_trustee_status_status;
}
[@@deriving desc]

type ('a, 'b) election_trustee_status =
  [ `Shuffle | `Tally of ('a, 'b) tally_trustee ]
[@@deriving desc]

type external_trustee = { address : string; token : string } [@@deriving desc]

type trustees_metadata = {
  version : int;
  group : string;
  owners : int list;
  trustees : external_trustee list option;
}
[@@deriving desc]

type shuffler = {
  trustee : (external_trustee * bool) option;
  fingerprint : hash option option;
}
[@@deriving desc]

type shuffles = { shufflers : shuffler list } [@@deriving desc]
type shuffler_request = [ `Skip | `Select ] [@@deriving desc]

type ballots_info = { bits : int; accepted : int; accepted_weight : weight }
[@@deriving desc]

type billing_request = {
  admin_id : int;
  date : int64;
  uuid : uuid;
  nb_voters : int;
}
[@@deriving desc]

type client_configuration = { consent : int64 option; lang : string option }
[@@deriving desc]

type confirmation = {
  voter : voter;
  name : string option;
  hash : hash;
  revote : bool;
  weight : bool;
  email : bool;
}
[@@deriving desc]

type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b
[@@deriving desc]

type cast_result = (confirmation, cast_error) result [@@deriving desc]

(** {1 Authentication API} *)

type auth_token = { token : string; expiration : int64 } [@@deriving desc]

(** {1 Dummy authentication related types} *)

type auth_dummy_info = { username : string } [@@deriving desc]

(** {1 Password authentication related types} *)

type auth_password_info = { username : string; password : string }
[@@deriving desc]

(** {1 Belenios Connect-related types} *)

type user_info = {
  login : string;
  name : string option;
  address : string option;
  timestamp : int64 option;
}
[@@deriving desc]

type connect_kind = [ `Site | `Election of uuid ] [@@deriving desc]

type connect_context = { kind : connect_kind; username : string option }
[@@deriving desc]
