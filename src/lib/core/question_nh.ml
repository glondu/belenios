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

open Ppx_yojson_conv_lib.Yojson_conv
open Signatures_core
open Crypto_types
open Question_types

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
type _ id += Id : question id

let type_ = "NonHomomorphic"

let erase (q : question) : question =
  { answers = Array.map (fun _ -> "") q.answers; question = "" }

type counting_method =
  [ `None
  | `MajorityJudgment of mj_extra
  | `Schulze of schulze_extra
  | `STV of stv_extra ]

let get_counting_method extra =
  match extra with
  | Some (`Assoc o as extra) -> (
      match List.assoc_opt "method" o with
      | Some (`String "MajorityJudgment") -> (
          match mj_extra_of_yojson extra with
          | x -> `MajorityJudgment x
          | exception _ -> `None)
      | Some (`String "Schulze") -> (
          match schulze_extra_of_yojson extra with
          | x -> `Schulze x
          | exception _ -> `None)
      | Some (`String "STV") -> (
          match stv_extra_of_yojson extra with
          | x -> `STV x
          | exception _ -> `None)
      | _ -> `None)
  | _ -> `None

let check group ~extra (q : question) =
  let module G = (val Lazy.force group : GROUP) in
  let n = Array.length q.answers in
  if n > G.max_ints then Error `Vector_size
  else
    match get_counting_method extra with
    | `None -> Ok ()
    | `Schulze _ | `STV _ ->
        if n < 1 lsl G.bits_per_int then Ok () else Error `Int_size
    | `MajorityJudgment { grades = g; _ } ->
        if Array.length g < 1 lsl G.bits_per_int then Ok () else Error `Int_size
