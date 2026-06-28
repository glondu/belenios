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

(** {1 Web-specific serializable datatypes} *)

open Ppx_yojson_conv_lib.Yojson_conv
open Belenios_messages

(** {1 Bulk emails} *)

type bulk_email = [ `Credential of material_message ] [@@deriving yojson]
type bulk_emails = bulk_email array [@@deriving yojson]
type bulk_mode = [ `Primary | `Secondary ] [@@deriving yojson]
type bulk_processed = { mode : bulk_mode; processed : int } [@@deriving yojson]

(** {1 OTP logging} *)

type otp_record = { recipient : string; code : string; expiration_time : float }
[@@deriving yojson]
