(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

let get_by_version = function
  | 1 -> (module Belenios_v1.Trustees : Belenios_core.Trustees_sig.S)
  | _ -> failwith "Trustees.get_by_version: unsupported version"

open Belenios_core.Signatures

let string_of_combination_error = function
  | MissingPartialDecryption -> "a partial decryption is missing"
  | NotEnoughPartialDecryptions -> "not enough partial decryptions"
  | InvalidPartialDecryption -> "invalid partial decryption"
