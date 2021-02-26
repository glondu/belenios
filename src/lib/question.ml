(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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

open Signatures_core

type question =
  | Homomorphic of Question_h_t.question
  | NonHomomorphic of Question_nh_t.question * Yojson.Safe.t option

let read_question l b =
  let x = Yojson.Safe.read_json l b in
  match x with
  | `Assoc o ->
     (match List.assoc_opt "type" o with
      | None ->
         Homomorphic (Question_h_j.question_of_string (Yojson.Safe.to_string x))
      | Some (`String "NonHomomorphic") ->
         (match List.assoc_opt "value" o with
          | None -> failwith "Question.read_question: value is missing"
          | Some v ->
             NonHomomorphic (
                 Question_nh_j.question_of_string (Yojson.Safe.to_string v),
                 List.assoc_opt "extra" o
               )
         )
      | Some _ ->
         failwith "Question.read_question: unexpected type"
     )
  | _ -> failwith "Question.read_question: unexpected JSON value"

let write_question b = function
  | Homomorphic q -> Question_h_j.write_question b q
  | NonHomomorphic (q, extra) ->
     let o =
       match extra with
       | None -> []
       | Some x -> ["extra", x]
     in
     let o =
       ("type", `String "NonHomomorphic")
       :: ("value", Yojson.Safe.from_string (Question_nh_j.string_of_question q))
       :: o
     in
     Yojson.Safe.write_json b (`Assoc o)

let erase_question = function
  | Homomorphic q ->
     let open Question_h_t in
     Homomorphic {
         q_answers = Array.map (fun _ -> "") q.q_answers;
         q_blank = q.q_blank;
         q_min = q.q_min;
         q_max = q.q_max;
         q_question = "";
       }
  | NonHomomorphic (q, extra) ->
     let open Question_nh_t in
     NonHomomorphic (
         {
           q_answers = Array.map (fun _ -> "") q.q_answers;
           q_question = "";
         },
         extra
       )

module Make (M : RANDOM) (G : GROUP) = struct
  let ( >>= ) = M.bind

  module QHomomorphic = Question_h.Make (M) (G)
  module QNonHomomorphic = Question_nh.Make (M) (G)

  let create_answer q ~public_key ~prefix m =
    match q with
    | Homomorphic q ->
       QHomomorphic.create_answer q ~public_key ~prefix m >>= fun answer ->
       answer
       |> Question_h_j.string_of_answer G.write
       |> Yojson.Safe.from_string
       |> M.return
    | NonHomomorphic (q, _) ->
       QNonHomomorphic.create_answer q ~public_key ~prefix m >>= fun answer ->
       answer
       |> Question_nh_j.string_of_answer G.write
       |> Yojson.Safe.from_string
       |> M.return

  let verify_answer q ~public_key ~prefix a =
    match q with
    | Homomorphic q ->
       a
       |> Yojson.Safe.to_string
       |> Question_h_j.answer_of_string G.read
       |> QHomomorphic.verify_answer q ~public_key ~prefix
    | NonHomomorphic (q, _) ->
       a
       |> Yojson.Safe.to_string
       |> Question_nh_j.answer_of_string G.read
       |> QNonHomomorphic.verify_answer q ~public_key ~prefix

  let extract_ciphertexts q a =
    match q with
    | Homomorphic q ->
       a
       |> Yojson.Safe.to_string
       |> Question_h_j.answer_of_string G.read
       |> QHomomorphic.extract_ciphertexts q
    | NonHomomorphic (q, _) ->
       a
       |> Yojson.Safe.to_string
       |> Question_nh_j.answer_of_string G.read
       |> QNonHomomorphic.extract_ciphertexts q

  let process_ciphertexts q e =
    match q with
    | Homomorphic q -> QHomomorphic.process_ciphertexts q e
    | NonHomomorphic (q, _) -> QNonHomomorphic.process_ciphertexts q e

  let compute_result ~num_tallied =
    let compute_h = lazy (QHomomorphic.compute_result ~num_tallied) in
    fun q x ->
    match q with
    | Homomorphic q -> Lazy.force compute_h q x
    | NonHomomorphic (q, _) -> QNonHomomorphic.compute_result ~num_tallied q x

  let check_result q x r =
    match q with
    | Homomorphic q -> QHomomorphic.check_result q x r
    | NonHomomorphic (q, _) -> QNonHomomorphic.check_result q x r
end
