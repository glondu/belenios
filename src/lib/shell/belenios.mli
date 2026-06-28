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

include module type of Belenios_core
module Election = Election
module Group = Group
module Pki = Belenios_core.Pki
module Trustees = Trustees

val xch_encrypted_credential : string exchangeable

module Credentials_certificate (G : GROUP) : sig
  val xch_credentials_certificate :
    (G.t, G.Zq.t) raw_credentials_certificate exchangeable

  val check : (G.t, G.Zq.t) credentials_certificate -> bool
end

module Language = Language

type lang = Language.t
