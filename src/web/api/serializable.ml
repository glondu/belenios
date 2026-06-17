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

open Ppx_yojson_conv_lib.Yojson_conv
open Belenios_core.Serializable_core
open Belenios_core.Serializable

type lang = Belenios.Language.t

type message_metadata = {
  uuid : uuid;
  admin_id : int;
  title : string;
  contact : string option; [@yojson.option]
  has_weights : bool;
  has_passwords : bool;
  langs : string list;
}
[@@deriving yojson]

type restricted_mode_error =
  [ `AutoCredentials
  | `VoterAuthentication
  | `ForbiddenQuestions
  | `NotEnoughTrustees
  | `NoThreshold
  | `HasWeights
  | `BadGroup ]
[@@deriving yojson]

type missing_billing = { url : string; id : string; callback : string }
[@@deriving yojson]

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
[@@deriving yojson]

type voter_list_error =
  [ `FormatMix
  | `Identity of string
  | `Duplicate of string
  | `TotalWeightTooBig of number * number ]
[@@deriving yojson]

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
[@@deriving yojson]

type request_status = { code : int; status : string; error : error }
[@@deriving yojson]

type configured_authentication = {
  instance : string;
  system : string;
  descr : string option; [@yojson.option]
}
[@@deriving yojson]

type authentication_system = [ `CAS | `Configured of configured_authentication ]
[@@deriving yojson]

type configuration_uris = { home : string; belenios : string; tos : string }
[@@deriving yojson]

type languages = (lang * string) list

let yojson_of_languages x : json =
  `Assoc
    (List.map
       (fun (k, v) -> (Belenios.Language.unwrap k, yojson_of_string v))
       x)

let languages_of_yojson : json -> languages = function
  | `Assoc o ->
      List.map (fun (k, v) -> (Belenios.Language.wrap k, string_of_yojson v)) o
  | x -> of_yojson_error "object expected" x

type configuration = {
  restricted_mode : bool;
  vendor : string;
  tos_last_update : float;
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
[@@deriving yojson]

type authentication_method = {
  service : string;
  username : string;
  portal : string option; [@yojson.option]
}
[@@deriving yojson]

type api_account = {
  id : int;
  authentication_method : authentication_method;
  name : string;
  address : string option; [@yojson.option]
  language : string option; [@yojson.option]
  default_voter_languages : string list;
  default_contact : string;
  voters_limit : int option; [@yojson.option]
}
[@@deriving yojson]

type authentication = [ `CAS of string | `Configured of string ]
[@@deriving yojson]

type cred_authority_info = { server : string; operator : string }
[@@deriving yojson]

type 'a raw_draft = {
  version : int;
  owners : int list;
  questions : 'a template;
  languages : string list;
  contact : string option; [@yojson.option]
  booth : int;
  authentication : authentication option; [@yojson.option]
  group : string;
  cred_authority_info : cred_authority_info option; [@yojson.option]
}
[@@deriving yojson]

type ('a, 'b) pedersen = {
  context : full_context;
  step : int;
  certs : ('a, 'b) cert array;
  vinput : ('a, 'b) vinput option; [@yojson.option]
  voutput : ('a, 'b) voutput option; [@yojson.option]
}
[@@deriving yojson]

type trustee_status_basic = [ `Init of string | `Done ] [@@deriving yojson]

type ('a, 'b) trustee_status_threshold =
  [ `Init
  | `WaitingForCertificate of full_context
  | `WaitingForOtherCertificates
  | `Pedersen of ('a, 'b) pedersen ]
[@@deriving yojson]

type ('a, 'b) trustee_status =
  [ `Basic of trustee_status_basic
  | `Threshold of ('a, 'b) trustee_status_threshold ]
[@@deriving yojson]

type state =
  [ `Draft
  | `Open
  | `Closed
  | `Shuffling
  | `EncryptedTally
  | `Tallied
  | `Archived ]
[@@deriving yojson]

type summary = { uuid : uuid; name : string; date : float; state : state }
[@@deriving yojson]

type summary_list = summary list [@@deriving yojson]
type string_list = string list [@@deriving yojson]
type public_credentials = string list [@@deriving yojson]

type 'a trustee = {
  address : string option; [@yojson.option]
  name : string;
  token : string option; [@yojson.option]
  state : int option; [@yojson.option]
  key : 'a option; [@yojson.option]
}
[@@deriving yojson]

type ('a, 'b) basic_trustees = {
  trustees : ('a, 'b) trustee_public_key trustee list;
}
[@@deriving yojson]

type ('a, 'b) threshold_trustees = {
  threshold : int option; [@yojson.option]
  trustees : ('a, 'b) cert trustee list;
}
[@@deriving yojson]

type ('a, 'b) draft_trustees =
  [ `Basic of ('a, 'b) basic_trustees
  | `Threshold of ('a, 'b) threshold_trustees ]
[@@deriving yojson]

type draft_status = {
  num_voters : int;
  credentials_ready : bool;
  credentials_left : int option; [@yojson.option]
  private_credentials_downloaded : bool option; [@yojson.option]
  trustees_ready : bool;
  nh_and_weights_compatible : bool;
  credential_authority_visited : bool;
  voter_authentication_visited : bool;
  trustees_setup_step : int;
  restricted_mode_error : restricted_mode_error option; [@yojson.option]
}
[@@deriving yojson]

type draft_request =
  [ `SetDownloaded
  | `ValidateElection
  | `SetCredentialAuthorityVisited
  | `SetVoterAuthenticationVisited
  | `SetTrusteesSetupStep of int
  | `InitiateCredentialAuthorityProtocol ]
[@@deriving yojson]

type credentials_new_request = {
  belenios_url : string;
  uuid : uuid;
  info : cred_authority_info;
  token : string;
  admin_id : int;
}
[@@deriving yojson]

type credentials_validate = {
  uuid : uuid;
  token : string;
  metadata : message_metadata;
}
[@@deriving yojson]

type resend_spec =
  [ `All_voters | `Missing_voters | `Some_voters of string list ]
[@@deriving yojson]

type credentials_resend = { uuid : uuid; seed : string; spec : resend_spec }
[@@deriving yojson]

type credentials_request =
  [ `NewRequest of credentials_new_request
  | `Validate of credentials_validate
  | `Resend of credentials_resend ]
[@@deriving yojson]

type credentials_response = {
  certificate : (json, json) credentials_certificate;
  token : string;
  public_credentials : public_credentials;
}
[@@deriving yojson]

type abstract_resend_spec = [ `All_voters | `Missing_voters | `Some_voters ]
[@@deriving yojson]

type credentials_credit_kind =
  [ `Initial | `Resend of abstract_resend_spec | `Other of string ]
[@@deriving yojson]

type credentials_credit = {
  credits : int;
  success : bool;
  kind : credentials_credit_kind;
  timestamp : float;
}
[@@deriving yojson]

type credentials_credits = credentials_credit list [@@deriving yojson]

type trustees_request =
  [ `Add of json trustee
  | `Import of uuid
  | `SetBasic
  | `SetThreshold of int
  | `Reset ]
[@@deriving yojson]

type voters_request = [ `Import of uuid ] [@@deriving yojson]

type election_status = {
  state : state;
  authentication : authentication option; [@yojson.option]
  auto_delete_date : float;
  auto_archive_date : float option; [@yojson.option]
  sealed : bool;
}
[@@deriving yojson]

type election_auto_dates = {
  open_ : float option; [@yojson.option] [@key "open"]
  close : float option; [@yojson.option]
  publish : float option; [@yojson.option]
  grace_period : float option; [@yojson.option]
}
[@@deriving yojson]

type voting_record = { date : float; username : string } [@@deriving yojson]
type records = voting_record list [@@deriving yojson]

type admin_request =
  [ `Open
  | `Close
  | `ComputeEncryptedTally
  | `FinishShuffling
  | `ReleaseTally
  | `Archive
  | `Seal of bool ]
[@@deriving yojson]

type trustee_pd = {
  address : string;
  token : string;
  done_ : bool; [@key "done"]
}
[@@deriving yojson]

type partial_decryptions = {
  trustees : trustee_pd list;
  threshold : int option; [@yojson.option]
}
[@@deriving yojson]

type tally_trustee = {
  private_key : (json, json) sent_partial_decryption_key option; [@yojson.option]
}
[@@deriving yojson]

type shuffler = {
  address : string;
  token : string option; [@yojson.option]
  fingerprint : string option; [@yojson.option]
}
[@@deriving yojson]

type shuffles = { shufflers : shuffler list } [@@deriving yojson]
type shuffler_request = [ `Skip | `Select ] [@@deriving yojson]
type ballots_with_weights = (hash * weight) list

let yojson_of_ballots_with_weights x : json =
  let open Belenios_core.Common_types in
  `Assoc (List.map (fun (k, v) -> (Hash.unwrap k, Weight.unwrap v)) x)

let ballots_with_weights_of_yojson : json -> ballots_with_weights =
  let open Belenios_core.Common_types in
  function
  | `Assoc o -> List.map (fun (k, v) -> (Hash.wrap k, Weight.wrap v)) o
  | x -> of_yojson_error "object expected" x

type billing_request = {
  admin_id : int;
  date : float;
  uuid : uuid;
  nb_voters : int;
}
[@@deriving yojson]

type client_configuration = {
  consent : float option; [@yojson.option]
  lang : string option; [@yojson.option]
}
[@@deriving yojson]

type confirmation = {
  recipient : recipient;
  name : string option; [@yojson.option]
  hash : hash;
  revote : bool;
  weight : weight option;
  email : bool;
}
[@@deriving yojson]

type cast_result = [ `Ok of confirmation | `Error of cast_error ]
[@@deriving yojson]

(** {1 Authentication API} *)

type auth_token = { token : string; expiration : float } [@@deriving yojson]

(** {1 Dummy authentication related types} *)

type auth_dummy_info = { username : string } [@@deriving yojson]

(** {1 Password authentication related types} *)

type auth_password_info = { username : string; password : string }
[@@deriving yojson]

(** {1 Belenios Connect-related types} *)

type user_info = {
  login : string;
  name : string option; [@yojson.option]
  address : string option; [@yojson.option]
  timestamp : float option; [@yojson.option]
}
[@@deriving yojson]

type connect_kind = [ `Site | `Election of uuid ] [@@deriving yojson]

type connect_context = {
  kind : connect_kind;
  username : string option; [@yojson.option]
}
[@@deriving yojson]

(** {1 OpenID Connect-related types} *)

type oidc_configuration = {
  authorization_endpoint : string;
  token_endpoint : string;
  userinfo_endpoint : string;
}
[@@deriving yojson]

type oidc_tokens = {
  access_token : string;
  token_type : string;
  id_token : string;
}
[@@deriving yojson]

type oidc_userinfo = {
  sub : string;
  name : string option; [@yojson.option]
  email : string option; [@yojson.option]
}
[@@deriving yojson]
