(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

(** {1 Serializable datatypes for non-homomorphic questions} *)

open Ppx_yojson_conv_lib.Yojson_conv
open Belenios_core

(** {2 Questions and answers} *)

type question = { answers : string array; question : string }
[@@deriving yojson]

type ('a, 'b) answer = { choices : 'a ciphertext; proof : 'b proof }
[@@deriving yojson]
(** An answer to a question. *)

type result = int array array [@@deriving yojson]

(** {2 Counting methods} *)

type mj_extra = { blank : bool; grades : string array } [@@deriving yojson]
type schulze_extra = { blank : bool } [@@deriving yojson]
type stv_extra = { blank : bool; seats : int } [@@deriving yojson]
