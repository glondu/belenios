(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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

open Serializable_j
open Tool_js_common

let (>>=) = Js.Opt.bind
let return = Js.Opt.return
let handler f = Dom_html.handler (fun e -> ignore (f e); Js._false)

(* Getting the OCaml structure out of the DOM *)

let extractAnswer a =
  Dom_html.CoerceTo.input a >>= fun x ->
  return (Js.to_string (x##value))

let extractQuestion q =
  Dom_html.CoerceTo.input q >>= fun x ->
  let q_question = Js.to_string (x##value) in
  q##parentNode >>= fun p1 ->
  p1##parentNode >>= fun p2 ->
  Dom.CoerceTo.element p2 >>= fun p2 ->
  let p2 = Dom_html.element p2 in
  let numeric selector error_msg =
    p2##querySelector (Js.string selector) >>= fun x ->
    Dom_html.CoerceTo.input x >>= fun x ->
    let x = Js.to_string x##value in
    try return (int_of_string x)
    with _ -> failwith (error_msg ^ ": " ^ x ^ ".")
  in
  numeric ".question_min" "Invalid minimum number of choices" >>= fun q_min ->
  numeric ".question_max" "Invalid maximum number of choices" >>= fun q_max ->
  if not (q_min <= q_max) then
    failwith "Minimum number of choices must be less than or equal to maximum number of choices!";
  let answers = p2##querySelectorAll (Js.string ".question_answer") in
  let q_answers =
    Array.init
      (answers##length)
      (fun i ->
       let a = answers##item (i) >>= extractAnswer in
       Js.Opt.get a (fun () -> failwith "extractQuestion"))
  in
  return {q_question; q_min; q_max; q_answers}

let extractTemplate () =
  let t_name = get_input "election_name" in
  let t_short_name = get_input "election_short_name" in
  let t_description = get_textarea "election_description" in
  let questions = document##querySelectorAll (Js.string ".question_question") in
  let t_questions =
    Array.init
      (questions##length)
      (fun i ->
       let q = questions##item (i) >>= extractQuestion in
       Js.Opt.get q (fun () -> failwith "extractTemplate"))
  in
  {t_name; t_short_name; t_description; t_questions}

(* Injecting the OCaml structure into the DOM *)

let createAnswer a =
  let container = Dom_html.createDiv document in
  let t = document##createTextNode (Js.string "Answer: ") in
  let u = Dom_html.createInput document in
  u##className <- Js.string "question_answer";
  u##value <- Js.string a;
  u##size <- 60;
  Dom.appendChild container t;
  Dom.appendChild container u;
  container

let createQuestion q =
  let container = Dom_html.createDiv document in
  (* properties *)
  let x = Dom_html.createDiv document in
  let t = document##createTextNode (Js.string "Question: ") in
  Dom.appendChild x t;
  let h_question = Dom_html.createInput document in
  Dom.appendChild x h_question;
  h_question##className <- Js.string "question_question";
  h_question##size <- 60;
  h_question##value <- Js.string q.q_question;
  Dom.appendChild container x;
  let x = Dom_html.createDiv document in
  let t = document##createTextNode (Js.string "The voter has to choose between ") in
  Dom.appendChild x t;
  let h_min = Dom_html.createInput document in
  Dom.appendChild x h_min;
  h_min##className <- Js.string "question_min";
  h_min##size <- 5;
  h_min##value <- Js.string (string_of_int q.q_min);
  let t = document##createTextNode (Js.string " and ") in
  Dom.appendChild x t;
  let h_max = Dom_html.createInput document in
  Dom.appendChild x h_max;
  h_max##className <- Js.string "question_max";
  h_max##size <- 5;
  h_max##value <- Js.string (string_of_int q.q_max);
  let t = document##createTextNode (Js.string " answers.") in
  Dom.appendChild x t;
  Dom.appendChild container x;
  (* answers *)
  let h_answers = Dom_html.createDiv document in
  h_answers##className <- Js.string "question_answers";
  Dom.appendChild container h_answers;
  Array.iter
    (fun a ->
     let x = createAnswer a in
     Dom.appendChild h_answers x)
    q.q_answers;
  (* buttons for adding/removing answers *)
  let x = Dom_html.createDiv document in
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string "Add an answer") in
  let f _ =
    let x = createAnswer "" in
    Dom.appendChild h_answers x
  in
  b##onclick <- handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string "Remove last answer") in
  let f _ =
    let answers = h_answers##querySelectorAll (Js.string ".question_answer") in
    let last_answer = answers##item (answers##length - 1) in
    last_answer >>= fun x ->
    x##parentNode >>= fun p ->
    p##parentNode >>= fun q ->
    ignore (q##removeChild (p));
    return ()
  in
  b##onclick <- handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  Dom.appendChild container x;
  (* horizontal rule *)
  let x = Dom_html.createHr document in
  Dom.appendChild container x;
  (* return *)
  container

let createTemplate template =
  let container = Dom_html.createDiv document in
  (* name *)
  let x = Dom_html.createDiv document in
  x##style##display <- Js.string "none";
  let t = document##createTextNode (Js.string "Name of the election: ") in
  Dom.appendChild x t;
  let h_name = Dom_html.createInput document in
  h_name##id <- Js.string "election_name";
  h_name##value <- Js.string template.t_name;
  Dom.appendChild x h_name;
  Dom.appendChild container x;
  (* short name *)
  let x = Dom_html.createDiv document in
  x##style##display <- Js.string "none";
  let t = document##createTextNode (Js.string "Short name: ") in
  Dom.appendChild x t;
  let h_short_name = Dom_html.createInput document in
  h_short_name##id <- Js.string "election_short_name";
  h_short_name##value <- Js.string template.t_short_name;
  Dom.appendChild x h_short_name;
  Dom.appendChild container x;
  (* description *)
  let x = Dom_html.createDiv document in
  x##style##display <- Js.string "none";
  let y = Dom_html.createDiv document in
  let t = document##createTextNode (Js.string "Description:") in
  Dom.appendChild y t;
  Dom.appendChild x y;
  let y = Dom_html.createDiv document in
  let h_description = Dom_html.createTextarea document in
  h_description##id <- Js.string "election_description";
  h_description##value <- Js.string template.t_description;
  h_description##cols <- 80;
  Dom.appendChild y h_description;
  Dom.appendChild x y;
  Dom.appendChild container x;
  (* questions *)
  let x = Dom_html.createDiv document in
  let h_questions_div = Dom_html.createDiv document in
  h_questions_div##id <- Js.string "election_questions";
  Dom.appendChild x h_questions_div;
  Dom.appendChild container x;
  Array.iter
    (fun q ->
     let x = createQuestion q in
     Dom.appendChild h_questions_div x)
    template.t_questions;
  (* buttons for adding/removing questions *)
  let x = Dom_html.createDiv document in
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string "Add a question") in
  let f _ =
    let x = createQuestion {q_question=""; q_min=0; q_max=1; q_answers=[||]} in
    Dom.appendChild h_questions_div x
  in
  b##onclick <- handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string "Remove last question") in
  let f _ =
    let questions = h_questions_div##querySelectorAll (Js.string ".question_question") in
    let last_question = questions##item (questions##length - 1) in
    last_question >>= fun x ->
    x##parentNode >>= fun p ->
    p##parentNode >>= fun q ->
    q##parentNode >>= fun r ->
    ignore (r##removeChild (q));
    return ()
  in
  b##onclick <- handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  Dom.appendChild container x;
  (* button for submitting *)
  let x = Dom_html.createHr document in
  Dom.appendChild container x;
  let x = Dom_html.createDiv document in
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string "Save changes") in
  let f _ =
    try
      let template = extractTemplate () in
      set_textarea "questions" (string_of_template template);
      document##querySelector (Js.string "form") >>= fun x ->
      Dom_html.CoerceTo.form x >>= fun x ->
      x##submit ();
      return ()
    with Failure e ->
      alert e;
      return ()
  in
  b##onclick <- handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  Dom.appendChild container x;
  (* return *)
  container

(* Entry point *)

let fill_interactivity _ =
  document##getElementById (Js.string "interactivity") >>= fun e ->
  let t = template_of_string (get_textarea "questions") in
  let div = createTemplate t in
  Dom.appendChild e div;
  document##querySelector (Js.string "form") >>= fun x ->
  x##style##display <- Js.string "none";
  return ()

let () =
  Dom_html.window##onload <- handler fill_interactivity;
