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

open Lwt.Syntax
open Belenios
open Serializable_j

exception Race_condition
exception Election_not_found of uuid * string

let ( let&* ) x f = match x with None -> Lwt.return_none | Some x -> f x
let sleep = Lwt_unix.sleep

module Datetime = Types.Datetime
module Period = Types.Period

module Random = struct
  open Crypto_primitives

  let init_prng () = lazy (pseudo_rng (random_string secure_rng 16))
  let prng = ref (init_prng ())

  let () =
    let rec loop () =
      let* () = sleep 1800. in
      prng := init_prng ();
      loop ()
    in
    Lwt.async loop

  let get_rng () = Lazy.force !prng
end

include MakeGenerateToken (Random)

type draft_election =
  | Draft :
      'a Belenios.Election.version * 'a raw_draft_election
      -> draft_election

let draft_election_of_string x =
  let abstract = raw_draft_election_of_string Yojson.Safe.read_json x in
  let open Belenios.Election in
  match version_of_int abstract.se_version with
  | Version v ->
      let open (val get_serializers v) in
      let x = raw_draft_election_of_string read_question x in
      Draft (v, x)

let string_of_draft_election (Draft (v, x)) =
  let open (val Belenios.Election.get_serializers v) in
  string_of_raw_draft_election write_question x
