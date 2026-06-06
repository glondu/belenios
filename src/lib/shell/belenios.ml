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

include Belenios_platform
include Belenios_core.Common
include Belenios_core.Serializable_j
include Belenios_core.Signatures
module Version = Belenios_platform.Version
module Password = Belenios_core.Password
module Election = Election
module Group = Group
module Pki = Belenios_core.Pki
module Trustees = Trustees
module Credential = Belenios_core.Credential
module Events = Belenios_core.Events
module Archive = Belenios_core.Archive

module Methods = struct
  module Schulze = Belenios_core.Schulze
  module Stv = Belenios_core.Stv
  module Majority_judgment = Belenios_core.Majority_judgment
end

let xch_credentials_certificate =
  {
    dst = dst_prefix ^ "-credentials_certificate";
    of_string = Hash.of_hex;
    to_string = Hash.to_hex;
  }

let xch_encrypted_credential =
  {
    dst = dst_prefix ^ "-encrypted_credential";
    of_string = Fun.id;
    to_string = Fun.id;
  }

module Credentials_certificate (G : GROUP) = struct
  let check certificate =
    G.check certificate.verification_key
    && G.check certificate.encryption_key
    &&
    let@ signature cont =
      match certificate.signature with None -> false | Some x -> cont x
    in
    let hash =
      { certificate with signature = None }
      |> string_of_credentials_certificate (swrite G.to_string)
           (swrite G.Zq.to_string)
      |> Hash.hash_string
    in
    hash = signature.s_message
    &&
    let module P = Pki.Make (G) in
    P.verify xch_credentials_certificate certificate.verification_key signature
end

module Language = Language

type lang = Language.t
