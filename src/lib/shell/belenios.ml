(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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

include Belenios_core
module Election = Election
module Group = Group
module Pki = Belenios_core.Pki
module Trustees = Trustees

let xch_encrypted_credential =
  {
    dst = dst_prefix ^ "-encrypted_credential";
    of_yojson = Ppx_yojson_conv_lib.Yojson_conv.string_of_yojson;
    to_yojson = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_string;
  }

module Credentials_certificate (G : GROUP) = struct
  let xch_credentials_certificate =
    {
      dst = dst_prefix ^ "-credentials_certificate";
      of_yojson =
        [%witness_of_yojson ((module G) : _ raw_credentials_certificate)];
      to_yojson =
        [%yojson_of_witness ((module G) : _ raw_credentials_certificate)];
    }

  let check (certificate : _ credentials_certificate) =
    G.check certificate.message.verification_key
    && G.check certificate.message.encryption_key
    &&
    let module P = Pki.Make (G) in
    P.verify xch_credentials_certificate certificate.message.verification_key
      certificate
end

module Language = Language

type lang = Language.t
