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

(** {1 Serializable datatypes for lists questions} *)

open Ppx_yojson_conv_lib.Yojson_conv

(** {2 Predefined types} *)

type weight = Belenios_core.Serializable_core.weight [@@deriving yojson]

type 'a ciphertext = 'a Belenios_core.Serializable_core.ciphertext
[@@deriving yojson]

type 'a proof = 'a Belenios_core.Serializable_core.proof [@@deriving yojson]

type 'a disjunctive_proof = 'a Belenios_core.Serializable_core.disjunctive_proof
[@@deriving yojson]

(** {2 Non-zero proof} *)

type ('a, 'b) nonzero_proof = {
  commitment : 'a;
  challenge : 'b;
  response : 'b * 'b;
}
[@@deriving yojson]

(** {2 Questions and answers} *)

type question = { answers : string array array; question : string }
[@@deriving yojson]

type ('a, 'b) answer = {
  choices : 'a ciphertext array array;
  individual_proofs : 'b disjunctive_proof array array;
  overall_proof : 'b proof;
  list_proofs : 'b disjunctive_proof array;
  nonzero_proof : ('a, 'b) nonzero_proof;
}
[@@deriving yojson]

type result = weight array array [@@deriving yojson]
