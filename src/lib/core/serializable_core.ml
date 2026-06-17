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

(** {1 Serializable datatypes (core)} *)

open Ppx_yojson_conv_lib.Yojson_conv

let () =
  Printexc.register_printer (function
    | Of_yojson_error (e, j) ->
        Some
          (Printf.sprintf "Of_yojson_error(%s, %s)" (Printexc.to_string e)
             (Yojson.Safe.to_string j))
    | _ -> None)

(** {2 Predefined types} *)

type number = Common_types.Number.t

let number_of_yojson = function
  | `String x -> Common_types.Number.wrap x
  | x -> of_yojson_error "string expected" x

let yojson_of_number x = `String (Common_types.Number.unwrap x)

type weight = Common_types.Weight.t

let weight_of_yojson = Common_types.Weight.wrap
let yojson_of_weight = Common_types.Weight.unwrap

(** {2 Basic cryptographic datastructures} *)

type 'a ciphertext = { alpha : 'a; beta : 'a } [@@deriving yojson]
(** An ElGamal ciphertext. *)

type 'a proof = { challenge : 'a; response : 'a } [@@deriving yojson]
(** A Fiat-Shamir non-interactive zero-knowledge proof of knowledge (ZKP). *)

type 'a disjunctive_proof = 'a proof array [@@deriving yojson]
(** A disjunctive ZKP. The size of the array is the number of disjuncts. *)

type voter = {
  address : string option; [@yojson.option]
  login : string option; [@yojson.option]
  weight : weight option; [@yojson.option]
}
[@@deriving yojson]

type voter_list = voter list [@@deriving yojson]
type recipient = { name : string; address : string } [@@deriving yojson]

type 'a public_credential = {
  credential : 'a;
  weight : weight option; [@yojson.option]
  username : string option; [@yojson.option]
}
[@@deriving yojson]
