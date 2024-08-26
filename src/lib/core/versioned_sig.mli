(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Signatures

module type GROUP_SIG = sig
  val of_string : string -> (module GROUP)
end

module type QUESTION_SIG = sig
  type t

  val wrap : Yojson.Safe.t -> t
  val unwrap : t -> Yojson.Safe.t
  val is_nh_question : t -> bool

  module Make (_ : RANDOM) (G : GROUP) :
    Question_sigs.QUESTION
      with type element := G.t
       and type question := t
       and type answer := Yojson.Safe.t
       and type result := string
end

module type MIXNET_SIG = sig
  type question

  module Make (W : ELECTION_DATA with type question := question) (_ : RANDOM) :
    MIXNET
      with type element := W.G.t
       and type scalar := W.G.Zq.t
       and type 'a proof := ('a, W.G.Zq.t) Serializable_t.shuffle_proof
end

module type ELECTION_SIG = sig
  type question

  val template_of_string : string -> question Serializable_t.template

  val make_raw_election :
    question Serializable_t.template ->
    uuid:Common.Uuid.t ->
    group:string ->
    public_key:string ->
    string

  module Make (_ : RAW_ELECTION) (_ : RANDOM) () :
    ELECTION with type question := question
end
