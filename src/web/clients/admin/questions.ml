(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria, CNRS                                     *)
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

open Belenios
open Belenios_question
open Belenios_web_api
open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios_js.Common
open Common

(* A generic type for question, covering H, NH and L, with default values for
 * irrelevant parts of the record.
 * This is closer to what is shown to the user.
 *)
type gen_quest = {
  question : string;
  answers : string array;
  answers_lists : string array array;
  blank : bool;
  kind : [ `Select | `Sort | `Grade | `Lists ];
  sel_min : int;
  sel_max : int;
  seats : int;
  count_meth : [ `None | `MJ | `Schulze | `STV ];
  grade_names : string array;
}

(* TODO : internationalize this *)
let default_grades = [| "Good"; "Average"; "Bad" |]

let new_gen_quest () : gen_quest =
  {
    question = "Question?";
    answers = [| "Answer 1"; "Answer 2"; "Answer 3" |];
    answers_lists = [| [| "List 1"; "Candidate 1"; "Candidate 2" |] |];
    blank = true;
    kind = `Select;
    sel_min = 1;
    sel_max = 1;
    seats = 1;
    count_meth = `None;
    grade_names = default_grades;
  }

(*
 * We keep the data corresponding to the questions, as seen by the user, as
 * global variables, so that they can be easily be updated when the user
 * interacts with the interface.
 *)

let all_gen_quest = ref [||]
let curr_doing = ref (-1)

(* Forward decl of update functions *)

let update_question = ref (fun ?save:_ _ -> Lwt.return_unit)
let update_main_zone = ref (fun _ -> Lwt.return_unit)

let q_to_gen (question : Belenios_question.t) =
  let ( question,
        answers,
        answers_lists,
        blank,
        kind,
        sel_min,
        sel_max,
        seats,
        meth,
        names ) =
    match question.value with
    | Homomorphic.Q q ->
        ( q.q_question,
          q.q_answers,
          [| q.q_answers |],
          Option.value ~default:false q.q_blank,
          `Select,
          q.q_min,
          q.q_max,
          1,
          `None,
          default_grades )
    | Non_homomorphic.Q q ->
        let me = Non_homomorphic.get_counting_method question.extra in
        let bk, ki, gr, me, seats =
          match me with
          | `Schulze o ->
              (o.schulze_extra_blank, `Sort, default_grades, `Schulze, 1)
          | `STV o ->
              (o.stv_extra_blank, `Sort, default_grades, `STV, o.stv_extra_seats)
          | `MajorityJudgment o ->
              (o.mj_extra_blank, `Grade, o.mj_extra_grades, `MJ, 1)
          | `None -> (false, `Grade, default_grades, `None, 1)
        in
        ( q.q_question,
          q.q_answers,
          [| q.q_answers |],
          bk,
          ki,
          1,
          1,
          seats,
          me,
          gr )
    | Lists.Q q ->
        ( q.q_question,
          q.q_answers.(0),
          q.q_answers,
          false,
          `Lists,
          1,
          1,
          1,
          `None,
          default_grades )
    | _ -> failwith "q_to_gen"
  in
  {
    question;
    answers;
    answers_lists;
    blank;
    kind;
    sel_min;
    sel_max;
    seats;
    count_meth = meth;
    grade_names = names;
  }

let gen_to_q q =
  match q.kind with
  | `Select ->
      Homomorphic.make
        ~value:
          {
            q_question = q.question;
            q_answers = q.answers;
            q_blank = Some q.blank;
            q_min = q.sel_min;
            q_max = q.sel_max;
          }
        ~extra:None
  | `Sort ->
      let extra =
        match q.count_meth with
        | `Schulze ->
            Some
              (`Assoc
                 [
                   ("type", `String "PreferentialVoting");
                   ("blank", `Bool q.blank);
                   ("method", `String "Schulze");
                 ])
        | `STV ->
            Some
              (`Assoc
                 [
                   ("type", `String "PreferentialVoting");
                   ("blank", `Bool q.blank);
                   ("seats", `Int q.seats);
                   ("method", `String "STV");
                 ])
        | _ -> None
      in
      Non_homomorphic.make
        ~value:{ q_question = q.question; q_answers = q.answers }
        ~extra
  | `Grade ->
      let extra =
        Some
          (`Assoc
             [
               ("type", `String "ScoreVoting");
               ("blank", `Bool q.blank);
               ("method", `String "MajorityJudgment");
               ( "grades",
                 `List
                   (q.grade_names |> Array.to_list
                   |> List.map (fun x -> `String x)) );
             ])
      in
      Non_homomorphic.make
        ~value:{ q_question = q.question; q_answers = q.answers }
        ~extra
  | `Lists ->
      Lists.make
        ~value:{ q_question = q.question; q_answers = q.answers_lists }
        ~extra:None

let delete_or_insert item attr handler_d handler_i =
  let del =
    div ~a:[ a_class [ "remove_" ^ item; "del_sym"; "clickable" ] ] []
  in
  let r = Tyxml_js.To_dom.of_div del in
  r##.onclick := lwt_handler handler_d;
  let ins = div ~a:[ a_class [ "add_" ^ item; "ins_sym"; "clickable" ] ] [] in
  let r = Tyxml_js.To_dom.of_div ins in
  r##.onclick := lwt_handler handler_i;
  Lwt.return @@ div ~a:attr [ del; ins ]

let set_complexity = ref (fun _ -> ())

(* save current state of questions in the cache, to be synchronized
 * with the server at some point.
 *)
let local_save () =
  let qq = Array.map gen_to_q !all_gen_quest in
  let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
  let draft_questions =
    let open (val Election.get_serializers v) in
    let t_questions = Array.map of_concrete qq in
    { draft.draft_questions with t_questions }
  in
  let draft_group =
    if Election.has_nh_questions (Template (v, draft_questions)) then
      match !server_configuration with
      | None -> draft.draft_group
      | Some c -> c.default_nh_group
    else draft.draft_group
  in
  let exception Question_error of int * string in
  try
    let () =
      let open (val !Belenios_js.I18n.gettext) in
      let version = draft.draft_version in
      let group = lazy (Group.of_string ~version draft_group) in
      Array.iteri
        (fun i q ->
          match Belenios_question.check_question group q with
          | Ok () -> ()
          | Error `Min_max ->
              raise
                (Question_error
                   ( i,
                     Printf.sprintf
                       (f_
                          "The min of question #%d is greater than its max, \
                           voting will be impossible!")
                       (i + 1) ))
          | Error (`Int_size | `Vector_size | `No_encoding) ->
              raise
                (Question_error
                   ( i,
                     Printf.sprintf
                       (f_
                          "Question #%d has too many choices, voting will fail!")
                       (i + 1) )))
        qq
    in
    let () =
      !set_complexity @@ Election.get_complexity @@ Template (v, draft_questions)
    in
    Cache.set Cache.draft
      (Draft (v, { draft with draft_questions; draft_group }));
    Lwt.return_unit
  with Question_error (i, msg) ->
    alert msg;
    (* restore cache state *)
    let qs = draft.draft_questions.t_questions in
    let open (val Election.get_serializers v) in
    !all_gen_quest.(i) <- (to_concrete >> q_to_gen) qs.(i);
    !update_question ~save:false (i + 1)

let insert_new_q ind =
  let ind = ind + 1 in
  (* new question is inserted after the current one *)
  let len = Array.length !all_gen_quest in
  let q_beg = Array.sub !all_gen_quest 0 ind in
  let q_end = Array.sub !all_gen_quest ind (len - ind) in
  let new_q = Array.concat [ q_beg; [| new_gen_quest () |]; q_end ] in
  curr_doing := ind;
  all_gen_quest := new_q;
  local_save ()

(* Create the HTML box that allows to edit a question *)
let q_to_html_inner ind q =
  let open (val !Belenios_js.I18n.gettext) in
  let ro = not (!curr_doing = ind) in
  let ind = ind + 1 in
  (* text of the question *)
  let inp_tit, _ =
    let attr = [ a_class [ "qtit" ]; a_id ("q" ^ string_of_int ind) ] in
    let attr = if ro then a_readonly () :: attr else attr in
    input ~a:attr q.question
  in
  let r = Tyxml_js.To_dom.of_input inp_tit in
  r##.onchange :=
    lwt_handler (fun _ ->
        !all_gen_quest.(!curr_doing) <-
          {
            (!all_gen_quest.(!curr_doing)) with
            question = Js.to_string r##.value;
          };
        !update_question !curr_doing);
  (* type of question, select, sort, grade or lists *)
  let rad_name = "type" ^ string_of_int ind in
  let mk_qtype i kind l =
    let id = Printf.sprintf "%s_%d" rad_name i in
    let inp_rad, _ =
      let attr = [ a_name rad_name; a_id id; a_input_type `Radio ] in
      let attr = if q.kind = kind then a_checked () :: attr else attr in
      let attr =
        if ro then a_disabled () :: attr else a_class [ "clickable" ] :: attr
      in
      input ~a:attr ""
    in
    let r = Tyxml_js.To_dom.of_input inp_rad in
    r##.onchange :=
      lwt_handler (fun () ->
          let current = !all_gen_quest.(!curr_doing) in
          let count_meth =
            match kind with
            | `Sort when current.count_meth = `None -> `Schulze
            | `Grade -> `MJ
            | _ -> current.count_meth
          in
          !all_gen_quest.(!curr_doing) <- { current with kind; count_meth };
          !update_question !curr_doing);
    div [ inp_rad; label ~a:[ a_label_for id ] [ txt l ] ]
  in
  let qtype1 = mk_qtype 1 `Select @@ s_ "Select propositions" in
  let qtype2 = mk_qtype 2 `Sort @@ s_ "Sort propositions" in
  let qtype3 = mk_qtype 3 `Grade @@ s_ "Grade propositions" in
  let qtype4 = mk_qtype 4 `Lists @@ s_ "Lists" in
  (* options depending of type of question *)
  let* expand =
    match q.kind with
    | `Select ->
        let attr =
          [
            a_input_type `Number;
            a_input_max (`Number (Array.length q.answers));
            a_input_min (`Number 0);
          ]
        in
        let attr = if ro then a_disabled () :: attr else attr in
        let inp_selm, _ =
          let attr1 = a_id ("selm" ^ string_of_int ind) :: attr in
          input ~a:attr1 (string_of_int !all_gen_quest.(ind - 1).sel_min)
        in
        let r = Tyxml_js.To_dom.of_input inp_selm in
        r##.onchange :=
          lwt_handler (fun () ->
              let value = int_of_string (Js.to_string r##.value) in
              !all_gen_quest.(!curr_doing) <-
                { (!all_gen_quest.(!curr_doing)) with sel_min = value };
              !update_question !curr_doing);
        let inp_selM, _ =
          let attr2 = a_id ("selM" ^ string_of_int ind) :: attr in
          input ~a:attr2 (string_of_int !all_gen_quest.(ind - 1).sel_max)
        in
        let r = Tyxml_js.To_dom.of_input inp_selM in
        r##.onchange :=
          lwt_handler (fun () ->
              let value = int_of_string (Js.to_string r##.value) in
              !all_gen_quest.(!curr_doing) <-
                { (!all_gen_quest.(!curr_doing)) with sel_max = value };
              !update_question !curr_doing);
        Lwt.return
        @@ div
             ~a:[ a_class [ "expand_select" ] ]
             [
               div
                 [
                   div
                     [
                       inp_selm;
                       label
                         ~a:[ a_label_for ("selm" ^ string_of_int ind) ]
                         [ txt @@ s_ "Minimal number of choices to select" ];
                     ];
                   div
                     [
                       inp_selM;
                       label
                         ~a:[ a_label_for ("selM" ^ string_of_int ind) ]
                         [ txt @@ s_ "Maximal number of choices to select" ];
                     ];
                 ];
             ]
    | `Sort ->
        let update_count_meth count_meth =
          let current = !all_gen_quest.(!curr_doing) in
          if current.count_meth <> count_meth then (
            !all_gen_quest.(!curr_doing) <- { current with count_meth };
            !update_question !curr_doing)
          else Lwt.return_unit
        in
        let inp_seats, _ =
          let attr =
            [
              a_input_type `Number;
              a_id ("seats" ^ string_of_int ind);
              a_input_min (`Number 1);
              a_input_max (`Number (Array.length q.answers));
            ]
          in
          let attr = if ro then a_disabled () :: attr else attr in
          input ~a:attr (string_of_int !all_gen_quest.(ind - 1).seats)
        in
        let r = Tyxml_js.To_dom.of_input inp_seats in
        r##.onchange :=
          lwt_handler (fun () ->
              let value = int_of_string (Js.to_string r##.value) in
              !all_gen_quest.(!curr_doing) <-
                { (!all_gen_quest.(!curr_doing)) with seats = value };
              !update_question !curr_doing);
        let div_seats =
          div
            [
              label
                ~a:[ a_label_for ("seats" ^ string_of_int ind) ]
                [ txt @@ s_ "Number of seats:"; txt " " ];
              inp_seats;
            ]
        in
        let dom_seats = Tyxml_js.To_dom.of_div div_seats in
        let () =
          let display = if q.count_meth = `STV then "block" else "none" in
          dom_seats##.style##.display := Js.string display
        in
        let rad_name = "sort" ^ string_of_int ind in
        let inp_sort_rad1, _ =
          let attr =
            [ a_input_type `Radio; a_name rad_name; a_id (rad_name ^ "_1") ]
          in
          let attr =
            if q.count_meth = `Schulze then a_checked () :: attr else attr
          in
          let attr =
            if ro then a_disabled () :: attr
            else a_class [ "clickable" ] :: attr
          in
          input ~a:attr ""
        in
        let r = Tyxml_js.To_dom.of_input inp_sort_rad1 in
        r##.onchange :=
          lwt_handler (fun () ->
              let checked = Js.to_bool r##.checked in
              let display = if checked then "none" else "block" in
              dom_seats##.style##.display := Js.string display;
              let count_meth = if checked then `Schulze else `STV in
              update_count_meth count_meth);
        let inp_sort_rad2, _ =
          let attr =
            [ a_input_type `Radio; a_name rad_name; a_id (rad_name ^ "_2") ]
          in
          let attr =
            if q.count_meth = `STV then a_checked () :: attr else attr
          in
          let attr =
            if ro then a_disabled () :: attr
            else a_class [ "clickable" ] :: attr
          in
          input ~a:attr ""
        in
        let r = Tyxml_js.To_dom.of_input inp_sort_rad2 in
        r##.onchange :=
          lwt_handler (fun () ->
              let checked = Js.to_bool r##.checked in
              let display = if checked then "block" else "none" in
              dom_seats##.style##.display := Js.string display;
              let count_meth = if checked then `STV else `Schulze in
              update_count_meth count_meth);
        Lwt.return
        @@ div
             ~a:[ a_class [ "expand_sort" ] ]
             [
               div
                 [
                   div
                     [
                       inp_sort_rad1;
                       label
                         ~a:[ a_label_for (rad_name ^ "_1") ]
                         [ txt @@ s_ "Condorcet-Schulze method" ];
                     ];
                   div
                     [
                       inp_sort_rad2;
                       label
                         ~a:[ a_label_for (rad_name ^ "_2") ]
                         [ txt @@ s_ "STV method" ];
                       div_seats;
                     ];
                 ];
             ]
    | `Grade ->
        let* list_grades =
          q.grade_names |> Array.to_list
          |> Lwt_list.mapi_s (fun i z ->
                 let inp, _ =
                   input
                     ~a:
                       [
                         a_id
                           ("ment" ^ string_of_int ind ^ "_" ^ string_of_int i);
                       ]
                     z
                 in
                 let r = Tyxml_js.To_dom.of_input inp in
                 r##.onchange :=
                   lwt_handler (fun _ ->
                       let new_ment = q.grade_names in
                       new_ment.(i) <- Js.to_string r##.value;
                       !all_gen_quest.(!curr_doing) <-
                         {
                           (!all_gen_quest.(!curr_doing)) with
                           grade_names = new_ment;
                         };
                       !update_question !curr_doing);
                 let* dd =
                   delete_or_insert "grade"
                     [ a_class [ "d_i_side" ] ]
                     (fun () ->
                       let new_m =
                         q.grade_names |> Array.to_list
                         |> List.filteri (fun j _ -> j <> i)
                         |> Array.of_list
                       in
                       !all_gen_quest.(!curr_doing) <-
                         {
                           (!all_gen_quest.(!curr_doing)) with
                           grade_names = new_m;
                         };
                       !update_question !curr_doing)
                     (fun () ->
                       let len = Array.length q.grade_names in
                       let a_beg = Array.sub q.grade_names 0 i in
                       let a_end = Array.sub q.grade_names i (len - i) in
                       let new_m =
                         Array.concat [ a_beg; [| s_ "New grade" |]; a_end ]
                       in
                       !all_gen_quest.(!curr_doing) <-
                         {
                           (!all_gen_quest.(!curr_doing)) with
                           grade_names = new_m;
                         };
                       !update_question !curr_doing)
                 in
                 Lwt.return @@ div ~a:[ a_class [ "mention" ] ] [ inp; dd ])
        in
        let* list_grades =
          Lwt.return @@ list_grades
          @ [
              (let dd =
                 div ~a:[ a_class [ "add_grade"; "ins_sym"; "clickable" ] ] []
               in
               let r = Tyxml_js.To_dom.of_div dd in
               r##.onclick :=
                 lwt_handler (fun () ->
                     let new_a =
                       Array.concat [ q.grade_names; [| s_ "New grade" |] ]
                     in
                     !all_gen_quest.(!curr_doing) <-
                       {
                         (!all_gen_quest.(!curr_doing)) with
                         grade_names = new_a;
                       };
                     !update_question !curr_doing);
               div
                 ~a:[ a_class [ "fake_mention" ] ]
                 [
                   div [ txt @@ s_ "Add a grade" ];
                   div ~a:[ a_class [ "d_i_side" ] ] [ dd ];
                 ]);
            ]
        in
        Lwt.return
        @@ div
             ~a:[ a_class [ "expand_grade" ] ]
             [ div (div [ txt @@ s_ "Proposed grades:" ] :: list_grades) ]
    | `Lists ->
        Lwt.return @@ div [] (* lists elections do not have specific settings *)
  in
  (* blank choice *)
  let inp, _ =
    let attr = [ a_id ("blank" ^ string_of_int ind); a_input_type `Checkbox ] in
    let attr = if q.blank then a_checked () :: attr else attr in
    let attr =
      if ro then a_disabled () :: attr else a_class [ "clickable" ] :: attr
    in
    input ~a:attr ""
  in
  let r = Tyxml_js.To_dom.of_input inp in
  r##.onchange :=
    lwt_handler (fun _ ->
        !all_gen_quest.(!curr_doing) <-
          { (!all_gen_quest.(!curr_doing)) with blank = not q.blank };
        !update_question !curr_doing);
  let bk =
    div
      ~a:[ a_class [ "blank_choice" ] ]
      [
        inp;
        label
          ~a:[ a_label_for ("blank" ^ string_of_int ind) ]
          [ txt @@ s_ "Allow blank vote" ];
      ]
  in

  (* text of the answers *)
  let* answers_box =
    match q.kind with
    | `Lists ->
        let make_list_box list_i answer_list =
          let* list_items =
            answer_list |> Array.to_list
            |> Lwt_list.mapi_s (fun candidate_i z ->
                   if candidate_i == 0 then (
                     (* list name *)
                     let inp, _ = input z in
                     let r = Tyxml_js.To_dom.of_input inp in
                     r##.onchange :=
                       lwt_handler (fun _ ->
                           let new_ans =
                             !all_gen_quest.(!curr_doing).answers_lists
                           in
                           new_ans.(list_i).(candidate_i) <-
                             Js.to_string r##.value;
                           !all_gen_quest.(!curr_doing) <-
                             {
                               (!all_gen_quest.(!curr_doing)) with
                               answers_lists = new_ans;
                             };
                           !update_question !curr_doing);
                     let* dd =
                       delete_or_insert "list"
                         [ a_class [ "d_i_side" ] ]
                         (fun () ->
                           (* delete a list*)
                           let new_a =
                             q.answers_lists |> Array.to_list
                             |> List.filteri (fun j _ -> j <> list_i)
                             |> Array.of_list
                           in
                           !all_gen_quest.(!curr_doing) <-
                             {
                               (!all_gen_quest.(!curr_doing)) with
                               answers_lists = new_a;
                             };
                           !update_question !curr_doing)
                         (fun () ->
                           (* insert a list *)
                           let list_i = list_i + 1 in
                           (* insert new list after the current one *)
                           let len = Array.length q.answers_lists in
                           let a_beg = Array.sub q.answers_lists 0 list_i in
                           let a_end =
                             Array.sub q.answers_lists list_i (len - list_i)
                           in
                           let new_a =
                             Array.concat
                               [
                                 a_beg;
                                 [| [| s_ "New list"; s_ "New candidate" |] |];
                                 a_end;
                               ]
                           in
                           !all_gen_quest.(!curr_doing) <-
                             {
                               (!all_gen_quest.(!curr_doing)) with
                               answers_lists = new_a;
                             };
                           !update_question !curr_doing)
                     in
                     let label =
                       div
                         ~a:[ a_class [ "answer_label" ] ]
                         [ txt (s_ "List name:") ]
                     in
                     Lwt.return
                     @@ div
                          ~a:[ a_class [ "answer"; "answer_list_name" ] ]
                          [ label; inp; dd ])
                   else
                     (* list candidate *)
                     let inp, _ = input z in
                     let r = Tyxml_js.To_dom.of_input inp in
                     r##.onchange :=
                       lwt_handler (fun _ ->
                           let new_ans =
                             !all_gen_quest.(!curr_doing).answers_lists
                           in
                           new_ans.(list_i).(candidate_i) <-
                             Js.to_string r##.value;
                           !all_gen_quest.(!curr_doing) <-
                             {
                               (!all_gen_quest.(!curr_doing)) with
                               answers_lists = new_ans;
                             };
                           !update_question !curr_doing);
                     let* dd =
                       delete_or_insert "candidate"
                         [ a_class [ "d_i_side" ] ]
                         (fun () ->
                           (* delete candidate *)
                           let new_a =
                             q.answers_lists |> Array.to_list
                             |> List.mapi (fun j l ->
                                    if j <> list_i then l
                                    else
                                      l |> Array.to_list
                                      |> List.filteri (fun j _ ->
                                             j <> candidate_i)
                                      |> Array.of_list)
                             |> Array.of_list
                           in
                           !all_gen_quest.(!curr_doing) <-
                             {
                               (!all_gen_quest.(!curr_doing)) with
                               answers_lists = new_a;
                             };
                           !update_question !curr_doing)
                         (fun () ->
                           (* insert new candidate after the current one *)
                           let candidate_i = candidate_i + 1 in
                           let len = Array.length q.answers_lists.(list_i) in
                           let a_beg =
                             Array.sub q.answers_lists.(list_i) 0 candidate_i
                           in
                           let a_end =
                             Array.sub q.answers_lists.(list_i) candidate_i
                               (len - candidate_i)
                           in
                           let new_a =
                             q.answers_lists |> Array.to_list
                             |> List.mapi (fun j l ->
                                    if j <> list_i then l
                                    else
                                      Array.concat
                                        [
                                          a_beg; [| s_ "New candidate" |]; a_end;
                                        ])
                             |> Array.of_list
                           in
                           !all_gen_quest.(!curr_doing) <-
                             {
                               (!all_gen_quest.(!curr_doing)) with
                               answers_lists = new_a;
                             };
                           !update_question !curr_doing)
                     in
                     Lwt.return @@ div ~a:[ a_class [ "answer" ] ] [ inp; dd ])
          in
          Lwt.return @@ div ~a:[ a_class [ "answers_list" ] ] list_items
        in
        let* answers_lists =
          q.answers_lists |> Array.to_list |> Lwt_list.mapi_s make_list_box
        in
        let lists_with_insert =
          (let dd =
             div ~a:[ a_class [ "add_list"; "ins_sym"; "clickable" ] ] []
           in
           let r = Tyxml_js.To_dom.of_div dd in
           r##.onclick :=
             lwt_handler (fun () ->
                 let new_a =
                   Array.concat
                     [
                       [| [| s_ "New list"; s_ "New candidate" |] |];
                       q.answers_lists;
                     ]
                 in
                 !all_gen_quest.(!curr_doing) <-
                   { (!all_gen_quest.(!curr_doing)) with answers_lists = new_a };
                 !update_question !curr_doing);
           div
             ~a:[ a_class [ "fake_answer" ] ]
             [
               div [ txt @@ s_ "Insert a list" ];
               div ~a:[ a_class [ "d_i_side" ] ] [ dd ];
             ])
          :: answers_lists
        in
        Lwt.return @@ div ~a:[ a_class [ "answers_lists" ] ] lists_with_insert
    | _ ->
        (* all types of question except Lists *)
        let* answers =
          q.answers |> Array.to_list
          |> Lwt_list.mapi_s (fun i z ->
                 let inp, _ =
                   input
                     ~a:
                       [
                         a_id ("ans" ^ string_of_int ind ^ "_" ^ string_of_int i);
                       ]
                     z
                 in
                 let r = Tyxml_js.To_dom.of_input inp in
                 r##.onchange :=
                   lwt_handler (fun _ ->
                       let new_ans = !all_gen_quest.(!curr_doing).answers in
                       new_ans.(i) <- Js.to_string r##.value;
                       !all_gen_quest.(!curr_doing) <-
                         {
                           (!all_gen_quest.(!curr_doing)) with
                           answers = new_ans;
                         };
                       !update_question !curr_doing);
                 let* dd =
                   delete_or_insert "answer"
                     [ a_class [ "d_i_side" ] ]
                     (fun () ->
                       let new_a =
                         q.answers |> Array.to_list
                         |> List.filteri (fun j _ -> j <> i)
                         |> Array.of_list
                       in
                       !all_gen_quest.(!curr_doing) <-
                         { (!all_gen_quest.(!curr_doing)) with answers = new_a };
                       !update_question !curr_doing)
                     (fun () ->
                       let i = i + 1 in
                       (* insert new answer after the current *)
                       let len = Array.length q.answers in
                       let a_beg = Array.sub q.answers 0 i in
                       let a_end = Array.sub q.answers i (len - i) in
                       let new_a =
                         Array.concat [ a_beg; [| s_ "New answer" |]; a_end ]
                       in
                       !all_gen_quest.(!curr_doing) <-
                         { (!all_gen_quest.(!curr_doing)) with answers = new_a };
                       !update_question !curr_doing)
                 in
                 Lwt.return @@ div ~a:[ a_class [ "answer" ] ] [ inp; dd ])
        in
        let answers_with_insert =
          (let dd =
             div ~a:[ a_class [ "add_answer"; "ins_sym"; "clickable" ] ] []
           in
           let r = Tyxml_js.To_dom.of_div dd in
           r##.onclick :=
             lwt_handler (fun () ->
                 let new_a =
                   Array.concat [ [| s_ "New answer" |]; q.answers ]
                 in
                 !all_gen_quest.(!curr_doing) <-
                   { (!all_gen_quest.(!curr_doing)) with answers = new_a };
                 !update_question !curr_doing);
           div
             ~a:[ a_class [ "fake_answer" ] ]
             [
               div [ txt @@ s_ "Insert an answer" ];
               div ~a:[ a_class [ "d_i_side" ] ] [ dd ];
             ])
          :: answers
        in
        Lwt.return
        @@ div [ bk; div ~a:[ a_class [ "answers" ] ] answers_with_insert ]
  in
  (* Put things together *)
  Lwt.return
    [
      label
        ~a:[ a_class [ "qtitle" ]; a_label_for ("q" ^ string_of_int ind) ]
        [ txt (s_ "Question " ^ string_of_int ind) ];
      inp_tit;
      div
        [
          div ~a:[ a_class [ "qtype" ] ] [ txt @@ s_ "Type of answer:" ];
          qtype1;
          qtype2;
          qtype3;
          qtype4;
        ];
      expand;
      div ~a:[ a_class [ "qans" ] ] [ txt @@ s_ "Proposed answers:" ];
      answers_box;
    ]

let scroll_to_active_question () =
  let id = "qq" ^ string_of_int (!curr_doing + 1) in
  scrollIntoViewById id

(* Around the box that allows a question to be edited *)
let q_to_html ind q all_ro =
  let open (val !Belenios_js.I18n.gettext) in
  let* inner = q_to_html_inner ind q in
  let* inner =
    if all_ro then Lwt.return inner
    else
      (* The Delete / Insert icons + handlers *)
      let* d_i =
        delete_or_insert "question"
          [ a_class [ "d_i_side_top" ] ]
          (fun () ->
            (* Delete handler *)
            let confirm =
              confirm @@ s_ "Are you sure you want to delete this question?"
            in
            if not confirm then Lwt.return_unit
            else
              let new_q =
                !all_gen_quest |> Array.to_list
                |> List.filteri (fun i _ -> i <> ind)
                |> Array.of_list
              in
              curr_doing := 0;
              all_gen_quest := new_q;
              let* () = local_save () in
              !update_main_zone ())
          (fun () ->
            (* Insert handler *)
            let* () = insert_new_q ind in
            !update_main_zone ())
      in
      Lwt.return (d_i :: inner)
  in
  (* Show the non-active questions in background, and make then clickable *)
  let ro = all_ro || not (!curr_doing = ind) in
  let attr =
    if ro then
      if all_ro then [ "question"; "qro" ] else [ "question"; "qro"; "blur" ]
    else [ "question"; "qdoing" ]
  in
  let dd = div ~a:[ a_class attr ] inner in
  (if ro then
     let r = Tyxml_js.To_dom.of_div dd in
     r##.onclick :=
       lwt_handler (fun _ ->
           if all_ro then Lwt.return_unit
           else
             (* Changing the focus of the question implies a sync to server *)
             let* () = local_save () in
             let* () = Cache.sync_until_success () in
             let old_curr = !curr_doing in
             curr_doing := ind;
             let* () = !update_question old_curr in
             let* () = !update_question !curr_doing in
             scroll_to_active_question ()));
  Lwt.return dd

let () =
  update_question :=
    fun ?(save = true) i ->
      let* () = if save then local_save () else Lwt.return_unit in
      let&&* container =
        document##getElementById (Js.string ("qq" ^ string_of_int (i + 1)))
      in
      let* content = q_to_html i !all_gen_quest.(i) false in
      show_in container (fun () -> Lwt.return [ content ])

let draft_recompute_main_zone () =
  let open (val !Belenios_js.I18n.gettext) in
  let* q_show =
    Lwt_list.mapi_s
      (fun i q ->
        let* dd = q_to_html i q false in
        Lwt.return @@ div ~a:[ a_id ("qq" ^ string_of_int (i + 1)) ] [ dd ])
      (Array.to_list !all_gen_quest)
  in
  let* complexity =
    let complexity_elt = span [] in
    let () =
      let container = Tyxml_js.To_dom.of_span complexity_elt in
      set_complexity :=
        fun c ->
          container##.textContent :=
            Js.some @@ Js.string
            @@ Printf.sprintf "%d+%d" c.nb_ciphertexts c.nb_zkps
    in
    let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
    let () =
      !set_complexity @@ Election.get_complexity
      @@ Template (v, draft.draft_questions)
    in
    Lwt.return
    @@ div
         ~a:[ a_style "display: none;" ]
         [ txt @@ s_ "Total complexity:"; txt " "; complexity_elt ]
  in
  let dd = div ~a:[ a_class [ "add_question"; "ins_sym"; "clickable" ] ] [] in
  let r = Tyxml_js.To_dom.of_div dd in
  r##.onclick :=
    lwt_handler (fun () ->
        all_gen_quest := Array.concat [ [| new_gen_quest () |]; !all_gen_quest ];
        curr_doing := 0;
        let* () = local_save () in
        !update_main_zone ());
  let prev_but =
    div ~a:[ a_class [ "clickable" ] ] [ txt @@ s_ "Preview voter's interface" ]
  in
  let prev = div ~a:[ a_id "previewbooth" ] [ prev_but ] in
  (if Array.length !all_gen_quest > 0 then
     let r = Tyxml_js.To_dom.of_div prev_but in
     r##.onclick :=
       lwt_handler (fun () ->
           let* () = local_save () in
           let* () = Cache.sync_until_success () in
           Preview.preview_booth ()));
  let q_show =
    div
      ~a:[ a_class [ "fake_question" ] ]
      [
        div [ txt @@ s_ "Insert a question" ];
        div ~a:[ a_class [ "d_i_side" ] ] [ dd ];
      ]
    :: q_show
  in
  let q_show = q_show @ [ prev ] in
  Lwt.return (h2 [ txt @@ s_ "Questions:" ] :: complexity :: q_show)

let () =
  update_main_zone :=
    fun () ->
      let* content = draft_recompute_main_zone () in
      let* () =
        let&&* container = document##getElementById (Js.string "main_zone") in
        show_in container (fun () -> Lwt.return content)
      in
      scroll_to_active_question ()

let running_recompute_main_zone () =
  let open (val !Belenios_js.I18n.gettext) in
  let* q_show =
    Lwt_list.mapi_s
      (fun i q ->
        let* dd = q_to_html i q true in
        Lwt.return @@ div ~a:[ a_id ("qq" ^ string_of_int (i + 1)) ] [ dd ])
      (Array.to_list !all_gen_quest)
  in
  Lwt.return (h2 [ txt @@ s_ "Questions (non editable):" ] :: q_show)

type questions =
  | Questions : 'a Belenios.Election.version * 'a array -> questions

(* Called from the outside.
 * Returns stuff to be put in the main zone.
 * In draft mode, if empty questions, then create a new draft question when
 * visiting the tab.
 *)
let questions_content () =
  let is_draft =
    match !where_am_i with Election { status = Draft; _ } -> true | _ -> false
  in
  let* (Questions (v, qs)) =
    if is_draft then
      let* (Draft (v, draft)) = Cache.get_until_success Cache.draft in
      Lwt.return (Questions (v, draft.draft_questions.t_questions))
    else
      let* x = Cache.get_until_success Cache.e_elec in
      let (Template (v, elec)) = Belenios.Election.template_of_string x in
      Lwt.return (Questions (v, elec.t_questions))
  in
  let open (val Election.get_serializers v) in
  all_gen_quest := Array.map (to_concrete >> q_to_gen) qs;
  if !curr_doing < 0 || !curr_doing >= Array.length !all_gen_quest then
    curr_doing := 0;
  let* () =
    if is_draft && Array.length !all_gen_quest = 0 then insert_new_q (-1)
    else Lwt.return_unit
  in
  let* content =
    if is_draft then draft_recompute_main_zone ()
    else (
      curr_doing := -1;
      running_recompute_main_zone ())
  in
  Lwt.return content
