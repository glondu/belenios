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

open Signatures_core
open Common

type t =
  | Homomorphic of Question_h_t.question
  | NonHomomorphic of Question_nh_t.question * Yojson.Safe.t option

let rec compute_signature qs =
  match qs with
  | [] ->
     let module X =
       struct
         type t = unit
         let x = Question_signature.Nil
       end
     in
     (module X : QUESTION_SIGNATURE_PACK)
  | Homomorphic _ :: qs ->
     let module X = (val compute_signature qs) in
     let module Y =
       struct
         type t = [`Homomorphic] * X.t
         let x = Question_signature.Homomorphic X.x
       end
     in
     (module Y)
  | NonHomomorphic (q, _) :: qs ->
     let module X = (val compute_signature qs) in
     let module Y =
       struct
         type t = [`NonHomomorphic] * X.t
         let x = Question_signature.NonHomomorphic (Array.length q.Question_nh_t.q_answers, X.x)
       end
     in
     (module Y)

let wrap x =
  match x with
  | `Assoc o ->
     (match List.assoc_opt "type" o with
      | None ->
         Homomorphic (Question_h_j.question_of_string (Yojson.Safe.to_string x))
      | Some (`String "NonHomomorphic") ->
         (match List.assoc_opt "value" o with
          | None -> failwith "Question.wrap: value is missing"
          | Some v ->
             NonHomomorphic (
                 Question_nh_j.question_of_string (Yojson.Safe.to_string v),
                 List.assoc_opt "extra" o
               )
         )
      | Some _ ->
         failwith "Question.wrap: unexpected type"
     )
  | _ -> failwith "Question.wrap: unexpected JSON value"

let unwrap = function
  | Homomorphic q -> Yojson.Safe.from_string (Question_h_j.string_of_question q)
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
     `Assoc o

type counting_method =
  [ `None
  | `MajorityJudgment of Question_nh_t.mj_extra
  | `Schulze of Question_nh_t.schulze_extra
  ]

let get_counting_method extra =
  let open Question_nh_j in
  match extra with
  | Some (`Assoc o as extra) ->
     (match List.assoc_opt "method" o with
      | Some (`String "MajorityJudgment") ->
         (match extra |> Yojson.Safe.to_string |> mj_extra_of_string with
          | x -> `MajorityJudgment x
          | exception _ -> `None
         )
      | Some (`String "Schulze") ->
         (match extra |> Yojson.Safe.to_string |> schulze_extra_of_string with
          | x -> `Schulze x
          | exception _ -> `None
         )
      | _ -> `None
     )
  | _ -> `None

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

module Make (M : RANDOM) (G : GROUP)
         (QHomomorphic : Question_sigs.QUESTION_H
          with type 'a m := 'a M.t
           and type elt := G.t
           and type question := Question_h_t.question
           and type answer := G.t Question_h_t.answer)
         (QNonHomomorphic : Question_sigs.QUESTION_NH
          with type 'a m := 'a M.t
           and type elt := G.t
           and type question := Question_nh_t.question
           and type answer := G.t Question_nh_t.answer) = struct

  let ( let* ) = M.bind

  let create_answer q ~public_key ~prefix m =
    match q with
    | Homomorphic q ->
       let* answer = QHomomorphic.create_answer q ~public_key ~prefix m in
       answer
       |> Question_h_j.string_of_answer (swrite G.to_string)
       |> Yojson.Safe.from_string
       |> M.return
    | NonHomomorphic (q, _) ->
       let* answer = QNonHomomorphic.create_answer q ~public_key ~prefix m in
       answer
       |> Question_nh_j.string_of_answer (swrite G.to_string)
       |> Yojson.Safe.from_string
       |> M.return

  let verify_answer q ~public_key ~prefix a =
    match q with
    | Homomorphic q ->
       a
       |> Yojson.Safe.to_string
       |> Question_h_j.answer_of_string (sread G.of_string)
       |> QHomomorphic.verify_answer q ~public_key ~prefix
    | NonHomomorphic (q, _) ->
       a
       |> Yojson.Safe.to_string
       |> Question_nh_j.answer_of_string (sread G.of_string)
       |> QNonHomomorphic.verify_answer q ~public_key ~prefix

  let extract_ciphertexts q a =
    match q with
    | Homomorphic q ->
       a
       |> Yojson.Safe.to_string
       |> Question_h_j.answer_of_string (sread G.of_string)
       |> QHomomorphic.extract_ciphertexts q
    | NonHomomorphic (q, _) ->
       a
       |> Yojson.Safe.to_string
       |> Question_nh_j.answer_of_string (sread G.of_string)
       |> QNonHomomorphic.extract_ciphertexts q

  let process_ciphertexts q e =
    match q with
    | Homomorphic q -> QHomomorphic.process_ciphertexts q e
    | NonHomomorphic (q, _) -> QNonHomomorphic.process_ciphertexts q e

  let compute_result ~num_tallied qs x =
    match x with
    | `Atomic _ -> invalid_arg "compute_result: invalid result"
    | `Array xs ->
       let compute_h = lazy (QHomomorphic.compute_result ~num_tallied) in
       let rec loop : 'a . 'a Question_signature.t -> G.t Shape.t list -> 'a Election_result.t =
         fun (type a) (qs : a Question_signature.t) xs ->
         let r : a Election_result.t =
           match qs, xs with
           | Nil, [] -> Nil
           | Nil, _ -> invalid_arg "compute_result: list too long"
           | _, [] -> invalid_arg "compute_result: list too short"
           | Homomorphic qs, x :: xs ->
              Homomorphic (Lazy.force compute_h x, loop qs xs)
           | NonHomomorphic (num_answers, qs), x :: xs ->
              NonHomomorphic (QNonHomomorphic.compute_result ~num_answers x, loop qs xs)
         in
         r
       in
       loop qs (Array.to_list xs)

  let check_result ~num_tallied qs x rs =
    match x with
    | `Atomic _ -> invalid_arg "check_result: invalid result"
    | `Array xs ->
       let rec loop : 'a . 'a Question_signature.t -> G.t Shape.t list -> 'a Election_result.t -> bool =
         fun (type a) (qs : a Question_signature.t) xs (rs : a Election_result.t) ->
         match qs, xs, rs with
         | Nil, [], Nil -> true
         | Nil, _, Nil -> invalid_arg "check_result: list too long"
         | _, [], _ -> invalid_arg "check_result: list too short"
         | Homomorphic qs, x :: xs, Homomorphic (r, rs) ->
            QHomomorphic.check_result ~num_tallied x r
            && loop qs xs rs
         | NonHomomorphic (num_answers, qs), x :: xs, NonHomomorphic (r, rs) ->
            QNonHomomorphic.check_result ~num_answers x r
            && loop qs xs rs
       in
       loop qs (Array.to_list xs) rs

end
