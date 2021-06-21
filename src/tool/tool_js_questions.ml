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

open Lwt.Syntax
open Js_of_ocaml
open Belenios
open Belenios_tool_js_common
open Serializable_j
open Tool_js_common
open Tool_js_i18n.Gettext

let return = Js.Opt.return
let handler f = Dom_html.handler (fun e -> ignore (f e); Js._false)

let hybrid_mode = ref false

let q_answers = [| "Answer 1"; "Answer 2"; "Answer 3" |]

let default_question_h =
  let open Question_h_t in
  Question.Homomorphic
    {
      q_question = "Question?";
      q_min = 1;
      q_max = 2;
      q_blank = None;
      q_answers;
    }

let default_question_nh =
  let open Question_nh_t in
  Question.NonHomomorphic (
      {
        q_question = "Give a rank to each candidate (a number between 1 and 3)";
        q_answers;
      },
      None
    )

let default_question () =
  if !hybrid_mode then default_question_nh else default_question_h

let default_mj_specification =
  "{\"type\":\"ScoreVoting\",\"blank\":true,\"grades\":[\"Excellent\",\"Good\",\"Bad\",\"Reject\"],\"method\":\"MajorityJudgment\"}"

(* Getting the OCaml structure out of the DOM *)

let extractAnswer a =
  Dom_html.CoerceTo.input a >>= fun x ->
  return (Js.to_string (x##.value))

let extractQuestion q =
  Dom_html.CoerceTo.input q >>= fun x ->
  let q_question = Js.to_string (x##.value) in
  q##.parentNode >>= fun p1 ->
  p1##.parentNode >>= fun p2 ->
  Dom.CoerceTo.element p2 >>= fun p2 ->
  let p2 = Dom_html.element p2 in
  let numeric selector error_msg =
    p2##querySelector (Js.string selector) >>= fun x ->
    Dom_html.CoerceTo.input x >>= fun x ->
    let x = Js.to_string x##.value in
    try return (int_of_string x) with
    | _ -> failwith (error_msg ^ ": " ^ x ^ ".")
  in
  let answers = p2##querySelectorAll (Js.string ".question_answer") in
  let q_answers =
    Array.init
      (answers##.length)
      (fun i ->
       let a = answers##item (i) >>= extractAnswer in
       Js.Opt.get a (fun () -> failwith "extractQuestion"))
  in
  match Js.Opt.to_option (p2##querySelector (Js.string ".question_blank")) with
  | Some q_blank ->
     Dom_html.CoerceTo.input q_blank >>= fun q_blank ->
     let q_blank = if Js.to_bool q_blank##.checked then Some true else None in
     numeric ".question_min" (s_ "Invalid minimum number of choices") >>= fun q_min ->
     numeric ".question_max" (s_ "Invalid maximum number of choices") >>= fun q_max ->
     if not (q_min <= q_max) then
       failwith (s_ "Minimum number of choices must be less than or equal to maximum number of choices!");
     if (q_max = 0) then
       failwith (s_ "Maximum number of choices must be greater than 0!");
     if (q_max > Array.length q_answers) then
       failwith (s_ "The given maximum is greater than the number of choices!");
     let open Question_h_t in
     return (Question.Homomorphic {q_question; q_blank; q_min; q_max; q_answers})
  | None ->
     p2##querySelector (Js.string ".question_extra") >>= fun x ->
     Dom_html.CoerceTo.input x >>= fun x ->
     let x = Js.to_string x##.value in
     let extra =
       if x = "" then
         None
       else (
         try Some (Yojson.Safe.from_string x)
         with _ -> failwith (s_ "Invalid counting method specification!")
       )
     in
     let open Question_nh_t in
     return (Question.NonHomomorphic ({q_question; q_answers}, extra))

let extractTemplate () =
  let t_name = get_input "election_name" in
  let t_description = get_textarea "election_description" in
  let questions = document##querySelectorAll (Js.string ".question_question") in
  let t_questions =
    Array.init
      (questions##.length)
      (fun i ->
       let q = questions##item (i) >>= extractQuestion in
       Js.Opt.get q (fun () -> failwith "extractTemplate"))
  in
  let t_administrator = None in
  let t_credential_authority = None in
  {t_name; t_description; t_questions; t_administrator; t_credential_authority}

(* Injecting the OCaml structure into the DOM *)

let rec createAnswer a =
  let container = Dom_html.createDiv document in
  container##.className := Js.string "question_answer_item";
  let t = document##createTextNode (Js.string (s_ "Answer: ")) in
  let u = Dom_html.createInput document in
  u##.className := Js.string "question_answer";
  u##.value := Js.string a;
  u##.size := 60;
  Dom.appendChild container t;
  Dom.appendChild container u;
  let btn_text = document##createTextNode (Js.string (s_ "Remove")) in
  let btn = Dom_html.createButton document in
  let f _ =
    container##.parentNode >>= fun x ->
    Dom.removeChild x container;
    return ()
  in
  btn##.onclick := handler f;
  btn##.className := Js.string "btn_remove";
  Dom.appendChild btn btn_text;
  Dom.appendChild container btn;
  let insert_text = document##createTextNode (Js.string (s_ "Insert")) in
  let insert_btn = Dom_html.createButton document in
  let f _ =
    let x = createAnswer "" in
    container##.parentNode >>= fun p ->
    Dom.insertBefore p x (Js.some container);
    return ()
  in
  insert_btn##.onclick := handler f;
  insert_btn##.className := Js.string "btn_insert";
  Dom.appendChild insert_btn insert_text;
  Dom.appendChild container insert_btn;
  container

let createHomomorphicQuestionPropDiv min max blank =
  let container = Dom_html.createDiv document in
  let x = Dom_html.createDiv document in
  let t = document##createTextNode (Js.string (s_ "The voter has to choose between ")) in
  Dom.appendChild x t;
  let h_min = Dom_html.createInput document in
  Dom.appendChild x h_min;
  h_min##.className := Js.string "question_min";
  h_min##.size := 5;
  h_min##.value := Js.string (string_of_int min);
  let t = document##createTextNode (Js.string (s_ " and ")) in
  Dom.appendChild x t;
  let h_max = Dom_html.createInput document in
  Dom.appendChild x h_max;
  h_max##.className := Js.string "question_max";
  h_max##.size := 5;
  h_max##.value := Js.string (string_of_int max);
  let t = document##createTextNode (Js.string (s_ " answers.")) in
  Dom.appendChild x t;
  Dom.appendChild container x;
  (* is blank allowed? *)
  let checkboxContainer = Dom_html.createDiv document in
  let x = Dom_html.createLabel document in
  let h_blank = Dom_html.createInput ~_type:(Js.string "checkbox") document in
  h_blank##.className := Js.string "question_blank";
  h_blank##.checked := Js.(match blank with Some true -> _true | _ -> _false);
  Dom.appendChild x h_blank;
  let t = document##createTextNode (Js.string (s_ "Blank vote is allowed")) in
  Dom.appendChild x t;
  Dom.appendChild checkboxContainer x;
  Dom.appendChild container checkboxContainer;
  container

let default_props = None, 0, 1

let gensym =
  let counter = ref 0 in
  fun () -> incr counter; !counter

let deleteQuestion q =
  q##.parentNode >>= fun x ->
  Dom.removeChild x q;
  return ()

let rec createQuestion booth_set q =
  let question, answers, props, extra =
    match q with
    | Question.Homomorphic q ->
       let open Question_h_t in
       q.q_question, q.q_answers, Some (q.q_blank, q.q_min, q.q_max), None
    | Question.NonHomomorphic (q, extra) ->
       let open Question_nh_t in
       q.q_question, q.q_answers, None, extra
  in
  let container = Dom_html.createDiv document in
  container##.className := Js.string "question";
  (* question text and remove/insert buttons *)
  let x = Dom_html.createDiv document in
  let t = document##createTextNode (Js.string (s_ "Question: ")) in
  Dom.appendChild x t;
  let h_question = Dom_html.createInput document in
  Dom.appendChild x h_question;
  h_question##.className := Js.string "question_question";
  h_question##.size := 60;
  h_question##.value := Js.string question;
  let remove_text = document##createTextNode (Js.string (s_ "Remove")) in
  let remove_btn = Dom_html.createButton document in
  let f _ =
    container##.parentNode >>= fun x ->
    Dom.removeChild x container;
    return ()
  in
  remove_btn##.onclick := handler f;
  Dom.appendChild remove_btn remove_text;
  Dom.appendChild x remove_btn;
  let insert_text = document##createTextNode (Js.string (s_ "Insert")) in
  let insert_btn = Dom_html.createButton document in
  let f _ =
    let x = createQuestion booth_set (default_question ()) in
    container##.parentNode >>= fun p ->
    Dom.insertBefore p x (Js.some container);
    return ()
  in
  insert_btn##.onclick := handler f;
  Dom.appendChild insert_btn insert_text;
  Dom.appendChild x insert_btn;
  Dom.appendChild container x;
  (* properties *)
  let prop_div_h =
    let blank, min, max =
      match props with
      | Some x -> x
      | None -> default_props
    in
    createHomomorphicQuestionPropDiv min max blank
  in
  let type_div = Dom_html.createDiv document in
  type_div##.style##.display := if !hybrid_mode then Js.string "block" else Js.string "none";
  Dom.appendChild container type_div;
  let prop_div_nh = Dom_html.createDiv document in
  (* extra *)
  let h_extra = Dom_html.createDiv document in
  Dom.appendChild prop_div_nh h_extra;
  let title = Js.string (s_ "Counting method specification") in
  let fieldset = Dom_html.createFieldset document in
  fieldset##.className := Js.string "counting_method_specification";
  Dom.appendChild h_extra fieldset;
  let legend = Dom_html.createLegend document in
  Dom.appendChild fieldset legend;
  Dom.appendChild legend (document##createTextNode title);
  let div_extra1 = Dom_html.createDiv document in
  Dom.appendChild fieldset div_extra1;
  let prefill_mj = Dom_html.createButton document in
  Dom.appendChild prefill_mj
    (document##createTextNode (Js.string (s_ "Prefill with Majority Judgment")));
  Dom.appendChild div_extra1 prefill_mj;
  let clear_spec = Dom_html.createButton document in
  Dom.appendChild clear_spec
    (document##createTextNode (Js.string (s_ "Clear")));
  Dom.appendChild div_extra1 clear_spec;
  let div_extra2 = Dom_html.createDiv document in
  Dom.appendChild fieldset div_extra2;
  let i_extra = Dom_html.createInput document in
  Dom.appendChild div_extra2 i_extra;
  i_extra##.placeholder := title;
  i_extra##.className := Js.string "question_extra";
  i_extra##.size := 80;
  (match extra with
   | None -> ()
   | Some x ->
      i_extra##.value := Js.string (Yojson.Safe.to_string x)
  );
  prefill_mj##.onclick :=
    Dom_html.handler
      (fun _ ->
        i_extra##.value := Js.string default_mj_specification;
        booth_set `New;
        Js._false
      );
  clear_spec##.onclick :=
    Dom_html.handler
      (fun _ ->
        i_extra##.value := Js.string "";
        booth_set `Classic;
        Js._false
      );
  let div_extra3 = Dom_html.createDiv document in
  div_extra3##.className := Js.string "nh_explain";
  Dom.appendChild fieldset div_extra3;
  Dom.appendChild div_extra3 (document##createTextNode (Js.string (s_ "Leave blank for other counting methods. Note that for other counting methods, the voting interface is quite rough: the voter has to enter an integer in front of each answer.")));
  Dom.appendChild div_extra3 (document##createTextNode (Js.string " "));
  let more_info = Dom_html.createA document in
  Dom.appendChild div_extra3 more_info;
  more_info##.href := Js.string "https://www.belenios.org/mixnet.html";
  Dom.appendChild more_info (document##createTextNode (Js.string (s_ "More information.")));
  (* selector *)
  let _type = Js.string "radio" and name = Printf.ksprintf Js.string "type%d" (gensym ()) in
  let type_classical = Dom_html.createDiv document in
  Dom.appendChild type_div type_classical;
  let x = Dom_html.createLabel document in
  Dom.appendChild type_classical x;
  let cb_type_classical = Dom_html.createInput ~_type ~name document in
  Dom.appendChild x cb_type_classical;
  Dom.appendChild x (document##createTextNode (Js.string (s_ "Classical (selection of answers)")));
  let type_alternative = Dom_html.createDiv document in
  Dom.appendChild type_div type_alternative;
  let x = Dom_html.createLabel document in
  Dom.appendChild type_alternative x;
  let cb_type = Dom_html.createInput ~_type ~name document in
  cb_type##.className := Js.string "nonhomomorphic_tally";
  (match props with
   | Some _ ->
      Dom.appendChild container prop_div_h;
      cb_type_classical##.checked := Js._true
   | None ->
      Dom.appendChild container prop_div_nh;
      cb_type##.checked := Js._true
  );
  let f =
    handler
      (fun _ ->
        container##.parentNode >>= fun parent ->
        if Js.to_bool cb_type##.checked then
          Dom.replaceChild parent (createQuestion booth_set default_question_nh) container
        else
          Dom.replaceChild parent (createQuestion booth_set default_question_h) container;
        return ()
      )
  in
  cb_type##.onchange := f;
  cb_type_classical##.onchange := f;
  if not (Js.to_bool (Js.Unsafe.pure_js_expr "allow_nh")) then
    cb_type##.disabled := Js._true;
  Dom.appendChild x cb_type;
  Dom.appendChild x (document##createTextNode (Js.string (s_ "Alternative (voters assign a number to each candidate)")));
  (* answers *)
  let h_answers = Dom_html.createDiv document in
  h_answers##.className := Js.string "question_answers";
  Dom.appendChild container h_answers;
  Array.iter
    (fun a ->
     let x = createAnswer a in
     Dom.appendChild h_answers x)
    answers;
  (* button for adding answer *)
  let x = Dom_html.createDiv document in
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string (s_ "Add an answer")) in
  let f _ =
    let x = createAnswer "" in
    Dom.appendChild h_answers x
  in
  b##.onclick := handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  Dom.appendChild container x;
  (* horizontal rule *)
  let x = Dom_html.createHr document in
  Dom.appendChild container x;
  (* return *)
  container

let createRadioItem name checked label =
  let container = Dom_html.createLabel document in
  let radio = Dom_html.createInput ~_type:(Js.string "radio") ~name document in
  radio##.checked := Js.bool checked;
  Dom.appendChild container radio;
  Dom.appendChild container (document##createTextNode (Js.string " "));
  Dom.appendChild container (document##createTextNode (Js.string label));
  radio, container

let createBoothSelector booth_version =
  let container = Dom_html.createDiv document in
  let line1 = Dom_html.createDiv document in
  Dom.appendChild container line1;
  Dom.appendChild line1 (document##createTextNode (Js.string (s_ "Booth:")));
  let line2 = Dom_html.createDiv document in
  Dom.appendChild container line2;
  let name = Js.string "booth_version_radio" in
  let r1, i1 = createRadioItem name (booth_version = 1) (s_ "classic") in
  Dom.appendChild line2 i1;
  let line3 = Dom_html.createDiv document in
  Dom.appendChild container line3;
  let r2, i2 = createRadioItem name (booth_version = 2) (s_ "new interface (more elegant, still under test)") in
  Dom.appendChild line3 i2;
  let get () = if Js.to_bool r2##.checked then 2 else 1 in
  let set = function
    | `Classic -> r1##.checked := Js._true
    | `New -> r2##.checked := Js._true
  in
  get, set, container

let createTemplate (booth_get, booth_set, booth_select) template =
  let container = Dom_html.createDiv document in
  (* name *)
  let x = Dom_html.createDiv document in
  x##.style##.display := Js.string "none";
  let t = document##createTextNode (Js.string (s_ "Name of the election: ")) in
  Dom.appendChild x t;
  let h_name = Dom_html.createInput document in
  h_name##.id := Js.string "election_name";
  h_name##.value := Js.string template.t_name;
  Dom.appendChild x h_name;
  Dom.appendChild container x;
  (* description *)
  let x = Dom_html.createDiv document in
  x##.style##.display := Js.string "none";
  let y = Dom_html.createDiv document in
  let t = document##createTextNode (Js.string (s_ "Description:")) in
  Dom.appendChild y t;
  Dom.appendChild x y;
  let y = Dom_html.createDiv document in
  let h_description = Dom_html.createTextarea document in
  h_description##.id := Js.string "election_description";
  h_description##.value := Js.string template.t_description;
  h_description##.cols := 80;
  Dom.appendChild y h_description;
  Dom.appendChild x y;
  Dom.appendChild container x;
  (* questions *)
  let x = Dom_html.createDiv document in
  let h_questions_div = Dom_html.createDiv document in
  h_questions_div##.id := Js.string "election_questions";
  Dom.appendChild x h_questions_div;
  Dom.appendChild container x;
  Array.iter
    (fun q ->
     let x = createQuestion booth_set q in
     Dom.appendChild h_questions_div x)
    template.t_questions;
  (* button for adding question *)
  let x = Dom_html.createDiv document in
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string (s_ "Add a question")) in
  let f _ =
    let x = createQuestion booth_set (default_question ()) in
    Dom.appendChild h_questions_div x
  in
  b##.onclick := handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  Dom.appendChild container x;
  (* booth selection *)
  Dom.appendChild container (Dom_html.createHr document);
  Dom.appendChild container booth_select;
  (* button for submitting *)
  let x = Dom_html.createHr document in
  Dom.appendChild container x;
  let x = Dom_html.createDiv document in
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string (s_ "Save changes")) in
  let f _ =
    try
      let template = extractTemplate () in
      set_textarea "questions" (string_of_template template);
      let booth_version = booth_get () in
      set_input "booth_version" (string_of_int booth_version);
      document##querySelector (Js.string "form") >>= fun x ->
      Dom_html.CoerceTo.form x >>= fun x ->
      let () = x##submit in
      return ()
    with Failure e ->
      alert e;
      return ()
  in
  b##.onclick := handler f;
  Dom.appendChild b t;
  Dom.appendChild x b;
  Dom.appendChild container x;
  (* return *)
  container

(* Handling of hybrid checkbox *)

let handle_hybrid booth_set e _ =
  hybrid_mode := Js.to_bool e##.checked;
  let qs = document##querySelectorAll (Js.string ".question") in
  for i = 0 to qs##.length do
    ignore (qs##item i >>= deleteQuestion)
  done;
  document##getElementById (Js.string "election_questions") >>= fun qsdiv ->
  Dom.appendChild qsdiv (createQuestion booth_set (default_question ()));
  return ()

(* Entry point *)

let fill_interactivity () =
  document##getElementById (Js.string "interactivity") >>= fun e ->
  let t = template_of_string (get_textarea "questions") in
  let booth_version = int_of_string (get_input "booth_version") in
  let has_nh =
    Array.exists
      (function
       | Question.NonHomomorphic _ -> true
       | _ -> false
      ) t.t_questions
  in
  hybrid_mode := has_nh;
  let (_, booth_set, _) as booth_selector = createBoothSelector booth_version in
  let div = createTemplate booth_selector t in
  Dom.appendChild e div;
  document##querySelector (Js.string "form") >>= fun x ->
  x##.style##.display := Js.string "none";
  document##getElementById (Js.string "hybrid_mode") >>= fun e ->
  Dom_html.CoerceTo.input e >>= fun e ->
  e##.checked := Js.bool !hybrid_mode;
  e##.onchange := handler (handle_hybrid booth_set e);
  return ()

let () =
  Lwt.async (fun () ->
      let* _ = Js_of_ocaml_lwt.Lwt_js_events.onload () in
      let* () = Tool_js_i18n.auto_init "admin" in
      ignore (fill_interactivity ());
      Lwt.return_unit
    )
