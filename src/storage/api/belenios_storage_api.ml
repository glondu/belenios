(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

module Lopt = Lopt
module Storage = Storage
include File_types
include Signatures
include Extra
include Types

let get_election_file_serializers = File_serializers.get_election
let get_credentials_file_serializers = File_serializers.get_credentials
let get_account_file_serializers = File_serializers.get_account

let default_election_dates =
  {
    creation = 0.;
    finalization = None;
    tally = None;
    archive = None;
    last_mail = None;
    auto_open = None;
    auto_close = None;
    publish = None;
    grace_period = None;
  }
