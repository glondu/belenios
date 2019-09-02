(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2019 Inria                                           *)
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
  | Standard of Question_h_t.question
  | Open of Question_nh_t.question

let read_question l b =
  let x = Yojson.Safe.read_json l b in
  match x with
  | `Assoc o ->
     (match List.assoc_opt "type" o with
      | None ->
         Standard (Question_h_j.question_of_string (Yojson.Safe.to_string x))
      | Some (`String "open") ->
         (match List.assoc_opt "value" o with
          | None -> failwith "Question.read_question: value is missing"
          | Some v -> Open (Question_nh_j.question_of_string (Yojson.Safe.to_string v))
         )
      | Some _ ->
         failwith "Question.read_question: unexpected type"
     )
  | _ -> failwith "Question.read_question: unexpected JSON value"

let write_question b = function
  | Standard q -> Question_h_j.write_question b q
  | Open q ->
     let o = [
         "type", `String "open";
         "value", Yojson.Safe.from_string (Question_nh_j.string_of_question q);
       ]
     in
     Yojson.Safe.write_json b (`Assoc o)

let erase_question = function
  | Standard q ->
     let open Question_h_t in
     Standard {
         q_answers = Array.map (fun _ -> "") q.q_answers;
         q_blank = q.q_blank;
         q_min = q.q_min;
         q_max = q.q_max;
         q_question = "";
       }
  | Open q ->
     let open Question_nh_t in
     Open {
         q_answers = Array.map (fun _ -> "") q.q_answers;
         q_question = "";
       }

module Make (M : RANDOM) (G : GROUP) = struct
  let ( >>= ) = M.bind

  module QStandard = Question_h.Make (M) (G)
  module QOpen = Question_nh.Make (M) (G)

  let create_answer q ~public_key ~prefix m =
    match q with
    | Standard q ->
       QStandard.create_answer q ~public_key ~prefix m >>= fun answer ->
       answer
       |> Question_h_j.string_of_answer G.write
       |> Yojson.Safe.from_string
       |> M.return
    | Open q ->
       QOpen.create_answer q ~public_key ~prefix m >>= fun answer ->
       answer
       |> Question_nh_j.string_of_answer G.write
       |> Yojson.Safe.from_string
       |> M.return

  let verify_answer q ~public_key ~prefix a =
    match q with
    | Standard q ->
       a
       |> Yojson.Safe.to_string
       |> Question_h_j.answer_of_string G.read
       |> QStandard.verify_answer q ~public_key ~prefix
    | Open q ->
       a
       |> Yojson.Safe.to_string
       |> Question_nh_j.answer_of_string G.read
       |> QOpen.verify_answer q ~public_key ~prefix

  let extract_ciphertexts q a =
    match q with
    | Standard q ->
       a
       |> Yojson.Safe.to_string
       |> Question_h_j.answer_of_string G.read
       |> QStandard.extract_ciphertexts q
    | Open q ->
       a
       |> Yojson.Safe.to_string
       |> Question_nh_j.answer_of_string G.read
       |> QOpen.extract_ciphertexts q

  let process_ciphertexts q e =
    match q with
    | Standard q -> QStandard.process_ciphertexts q e
    | Open q -> QOpen.process_ciphertexts q e

  let compute_result ~num_tallied =
    let compute_std = lazy (QStandard.compute_result ~num_tallied) in
    fun q x ->
    match q with
    | Standard q -> Lazy.force compute_std q x
    | Open q -> QOpen.compute_result ~num_tallied q x

  let check_result q x r =
    match q with
    | Standard q -> QStandard.check_result q x r
    | Open q -> QOpen.check_result q x r
end
