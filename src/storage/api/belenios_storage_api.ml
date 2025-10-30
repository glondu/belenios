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

module Atd_t = Serializable_t
module Atd_j = Serializable_j
module Lopt = Lopt
module Storage = Storage
include Serializable_j
include Extra
include Types

let get_file_serializers = File_serializers.get

let default_election_dates =
  {
    e_date_creation = 0.;
    e_date_finalization = None;
    e_date_tally = None;
    e_date_archive = None;
    e_date_last_mail = None;
    e_date_auto_open = None;
    e_date_auto_close = None;
    e_date_publish = None;
    e_date_grace_period = None;
  }
