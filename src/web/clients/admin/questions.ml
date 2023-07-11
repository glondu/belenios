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

open Belenios_core.Common
open Belenios_core.Question
open Belenios_core.Question_h_j
open Belenios_core.Question_nh_j
open Belenios_api.Serializable_j
open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Tyxml_js.Html5
open Belenios_js.Common
open Common

(* A generic type for question, covering H and NH, with default values for
 * irrelevant parts of the record.
 * This is closer to what is shown to the user.
 *)
(* TODO(seats) : the number of seats is not properly taken into account in the API *)
type gen_quest = {
  question : string;
  answers : string array;
  blank : bool;
  kind : [ `Select | `Sort | `Grade ];
  sel_min : int;
  sel_max : int;
  seats : int;
  count_meth : [ `None | `MJ | `Schulze ];
  grade_names : string array;
}

(* TODO : internationalize this *)
let default_grades = [| "Good"; "Average"; "Bad" |]

let new_gen_quest () : gen_quest =
  {
    question = "Question?";
    answers = [| "Answer 1"; "Answer 2"; "Answer 3" |];
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

let update_question = ref (fun _ -> Lwt.return_unit)
let update_main_zone = ref (fun _ -> Lwt.return_unit)

let q_to_gen q =
  let question, answers, blank, kind, sel_min, sel_max, seats, meth, names =
    match q with
    | Homomorphic q ->
        ( q.q_question,
          q.q_answers,
          Option.value ~default:false q.q_blank,
          `Select,
          q.q_min,
          q.q_max,
          1,
          `None,
          default_grades )
    | NonHomomorphic (q, extra) ->
        let me = get_counting_method extra in
        let bk, ki, gr, me =
          match me with
          | `Schulze o ->
              (o.schulze_extra_blank, `Sort, default_grades, `Schulze)
          | `MajorityJudgment o ->
              (o.mj_extra_blank, `Grade, o.mj_extra_grades, `MJ)
          | `None -> (false, `Grade, default_grades, `None)
        in
        (q.q_question, q.q_answers, bk, ki, 1, 1, 1, me, gr)
  in
  {
    question;
    answers;
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
      Homomorphic
        {
          q_question = q.question;
          q_answers = q.answers;
          q_blank = Some q.blank;
          q_min = q.sel_min;
          q_max = q.sel_max;
        }
  | `Sort ->
      let extra =
        `Assoc
          [
            ("type", `String "PreferentialVoting");
            ("blank", `Bool q.blank);
            ("method", `String "Schulze");
          ]
      in
      NonHomomorphic
        ({ q_question = q.question; q_answers = q.answers }, Some extra)
  | `Grade ->
      let extra =
        `Assoc
          [
            ("type", `String "ScoreVoting");
            ("blank", `Bool q.blank);
            ("method", `String "MajorityJudgment");
            ( "grades",
              `List
                (q.grade_names |> Array.to_list |> List.map (fun x -> `String x))
            );
          ]
      in
      NonHomomorphic
        ({ q_question = q.question; q_answers = q.answers }, Some extra)

let delete_or_insert attr handler_d handler_i =
  let del = div ~a:[ a_class [ "del_sym clickable" ] ] [] in
  let r = Tyxml_js.To_dom.of_div del in
  r##.onclick := lwt_handler handler_d;
  let ins = div ~a:[ a_class [ "ins_sym clickable" ] ] [] in
  let r = Tyxml_js.To_dom.of_div ins in
  r##.onclick := lwt_handler handler_i;
  Lwt.return @@ div ~a:attr [ del; ins ]

(* save current state of questions in the cache, to be synchronized
 * with the server at some point.
 *)
let local_save () =
  (*
    let neednh = Array.exists (fun x -> x.kind <> `Select) !all_gen_quest in
   *)
  (* FIXME: should use default groups sent by server, here *)
  let group = "Ed25519" in
  let qq = Array.map gen_to_q !all_gen_quest in
  let* draft = Cache.get_until_success Cache.draft in
  Cache.set Cache.draft
    {
      draft with
      draft_questions = { draft.draft_questions with t_questions = qq };
      draft_group = group;
    };
  Lwt.return_unit

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
  let attr = [ a_class [ "qtit" ]; a_id ("q" ^ string_of_int ind) ] in
  let attr = if ro then a_readonly () :: attr else attr in
  let inp_tit, _ = input ~a:attr q.question in
  let r = Tyxml_js.To_dom.of_input inp_tit in
  r##.onchange :=
    lwt_handler (fun _ ->
        !all_gen_quest.(!curr_doing) <-
          {
            (!all_gen_quest.(!curr_doing)) with
            question = Js.to_string r##.value;
          };
        !update_question !curr_doing);
  (* type of question, select, sort or grade *)
  let rad_name = "type" ^ string_of_int ind in
  let attr = [ a_name rad_name; a_id (rad_name ^ "_1"); a_input_type `Radio ] in
  let attr = if q.kind = `Select then a_checked () :: attr else attr in
  let attr =
    if ro then a_disabled () :: attr else a_class [ "clickable" ] :: attr
  in
  let inp_rad1, _ = input ~a:attr "" in
  let r = Tyxml_js.To_dom.of_input inp_rad1 in
  r##.onchange :=
    lwt_handler (fun () ->
        !all_gen_quest.(!curr_doing) <-
          { (!all_gen_quest.(!curr_doing)) with kind = `Select };
        !update_question !curr_doing);
  let attr = [ a_name rad_name; a_id (rad_name ^ "_2"); a_input_type `Radio ] in
  let attr = if q.kind = `Sort then a_checked () :: attr else attr in
  let attr =
    if ro then a_disabled () :: attr else a_class [ "clickable" ] :: attr
  in
  let inp_rad2, _ = input ~a:attr "" in
  let r = Tyxml_js.To_dom.of_input inp_rad2 in
  r##.onchange :=
    lwt_handler (fun () ->
        !all_gen_quest.(!curr_doing) <-
          { (!all_gen_quest.(!curr_doing)) with kind = `Sort };
        if !all_gen_quest.(!curr_doing).count_meth = `None then
          !all_gen_quest.(!curr_doing) <-
            { (!all_gen_quest.(!curr_doing)) with count_meth = `Schulze };
        !update_question !curr_doing);
  let attr = [ a_name rad_name; a_id (rad_name ^ "_3"); a_input_type `Radio ] in
  let attr = if q.kind = `Grade then a_checked () :: attr else attr in
  let attr =
    if ro then a_disabled () :: attr else a_class [ "clickable" ] :: attr
  in
  let inp_rad3, _ = input ~a:attr "" in
  let r = Tyxml_js.To_dom.of_input inp_rad3 in
  r##.onchange :=
    lwt_handler (fun () ->
        !all_gen_quest.(!curr_doing) <-
          {
            (!all_gen_quest.(!curr_doing)) with
            kind = `Grade;
            count_meth = `MJ;
          };
        !update_question !curr_doing);
  (* options depending of type of question *)
  let* expand =
    match q.kind with
    | `Select ->
        let attr =
          [
            a_input_type `Number;
            a_input_max (`Number (Array.length q.answers));
            a_input_min (`Number 1);
          ]
        in
        let attr = if ro then a_disabled () :: attr else attr in
        let attr1 = a_id ("selm" ^ string_of_int ind) :: attr in
        let inp_selm, _ =
          input ~a:attr1 (string_of_int !all_gen_quest.(ind - 1).sel_min)
        in
        let r = Tyxml_js.To_dom.of_input inp_selm in
        r##.onchange :=
          lwt_handler (fun () ->
              let value = int_of_string (Js.to_string r##.value) in
              !all_gen_quest.(!curr_doing) <-
                { (!all_gen_quest.(!curr_doing)) with sel_min = value };
              !update_question !curr_doing);
        let attr2 = a_id ("selM" ^ string_of_int ind) :: attr in
        let inp_selM, _ =
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
        (* TODO(seats) *)
        (*
      let attr = [a_input_type `Number; a_id ("seats"^string_of_int(ind)); a_input_min (`Number 1); a_input_max (`Number (Array.length q.answers))] in
      let attr = if ro then (a_disabled () :: attr) else attr in
      let inp_seats, _ = input ~a:attr (string_of_int !all_gen_quest.(ind-1).seats) in
      let r = Tyxml_js.To_dom.of_input inp_seats in
      r##.onchange := lwt_handler (fun () ->
          let value = int_of_string (Js.to_string r##.value) in
          !all_gen_quest.(!curr_doing) <- { !all_gen_quest.(!curr_doing) with
            seats = value };
          !update_question !curr_doing
      );
        *)
        let rad_name = "sort" ^ string_of_int ind in
        let attr =
          [ a_input_type `Radio; a_name rad_name; a_id (rad_name ^ "_1") ]
        in
        let attr =
          if q.count_meth = `Schulze then a_checked () :: attr else attr
        in
        let attr =
          if ro then a_disabled () :: attr else a_class [ "clickable" ] :: attr
        in
        let inp_sort_rad1, _ = input ~a:attr "" in
        let attr =
          [ a_input_type `Radio; a_name rad_name; a_id (rad_name ^ "_2") ]
        in
        let attr = if false then a_checked () :: attr else attr in
        (* STV is currently not available: disable this button *)
        let attr =
          if true then a_disabled () :: attr
          else a_class [ "clickable" ] :: attr
        in
        let inp_sort_rad2, _ = input ~a:attr "" in
        Lwt.return
        @@ div
             ~a:[ a_class [ "expand_sort" ] ]
             [
               div
                 [
                   (* TODO(seats) *)
                   (*
          div [
            inp_seats;
            label ~a:[a_label_for ("seats"^string_of_int(ind))] [txt "Nombre de sièges à pourvoir"];
          ];
                              *)
                   div
                     [
                       inp_sort_rad1;
                       label
                         ~a:[ a_label_for (rad_name ^ "_1") ]
                         [ txt @@ s_ "Method of Condorcet-Schulze" ];
                     ];
                   div
                     [
                       inp_sort_rad2;
                       label
                         ~a:[ a_label_for (rad_name ^ "_2") ]
                         [ txt @@ s_ "STV method" ];
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
                   delete_or_insert
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
              (let dd = div ~a:[ a_class [ "ins_sym clickable" ] ] [] in
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
  in
  (* blank choice *)
  let attr = [ a_id ("blank" ^ string_of_int ind); a_input_type `Checkbox ] in
  let attr = if q.blank then a_checked () :: attr else attr in
  let attr =
    if ro then a_disabled () :: attr else a_class [ "clickable" ] :: attr
  in
  let inp, _ = input ~a:attr "" in
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
  let* answers =
    q.answers |> Array.to_list
    |> Lwt_list.mapi_s (fun i z ->
           let inp, _ =
             input
               ~a:[ a_id ("ans" ^ string_of_int ind ^ "_" ^ string_of_int i) ]
               z
           in
           let r = Tyxml_js.To_dom.of_input inp in
           r##.onchange :=
             lwt_handler (fun _ ->
                 let new_ans = !all_gen_quest.(!curr_doing).answers in
                 new_ans.(i) <- Js.to_string r##.value;
                 !all_gen_quest.(!curr_doing) <-
                   { (!all_gen_quest.(!curr_doing)) with answers = new_ans };
                 !update_question !curr_doing);
           let* dd =
             delete_or_insert
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
                   Array.concat [ a_beg; [| "New answer" |]; a_end ]
                 in
                 !all_gen_quest.(!curr_doing) <-
                   { (!all_gen_quest.(!curr_doing)) with answers = new_a };
                 !update_question !curr_doing)
           in
           Lwt.return @@ div ~a:[ a_class [ "answer" ] ] [ inp; dd ])
  in
  let* answers =
    Lwt.return
    @@ (let dd = div ~a:[ a_class [ "ins_sym clickable" ] ] [] in
        let r = Tyxml_js.To_dom.of_div dd in
        r##.onclick :=
          lwt_handler (fun () ->
              let new_a = Array.concat [ [| s_ "New answer" |]; q.answers ] in
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
  (* Put things together *)
  Lwt.return
    [
      label
        ~a:[ a_class [ "qtitle" ]; a_label_for ("q" ^ string_of_int ind) ]
        [ txt (s_ "Question " ^ string_of_int ind) ];
      inp_tit;
      div ~a:[ a_class [ "qtype" ] ] [ txt @@ s_ "Type of answer:" ];
      inp_rad1;
      label
        ~a:[ a_label_for (rad_name ^ "_1") ]
        [ txt @@ s_ "Select propositions" ];
      inp_rad2;
      label
        ~a:[ a_label_for (rad_name ^ "_2") ]
        [ txt @@ s_ "Sort propositions" ];
      inp_rad3;
      label
        ~a:[ a_label_for (rad_name ^ "_3") ]
        [ txt @@ s_ "Grade propositions" ];
      expand;
      div ~a:[ a_class [ "qans" ] ] [ txt @@ s_ "Proposed answers:" ];
      bk;
      div ~a:[ a_class [ "answers" ] ] answers;
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
        delete_or_insert
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
    fun i ->
      let* () = local_save () in
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
  let dd = div ~a:[ a_class [ "ins_sym clickable" ] ] [] in
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
   r##.onclick := lwt_handler (fun () -> Preview.preview_booth ()));
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
  Lwt.return (h2 [ txt @@ s_ "Questions:" ] :: q_show)

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

(* Called from the outside.
 * Returns stuff to be put in the main zone.
 * In draft mode, if empty questions, then create a new draft question when
 * visiting the tab.
 *)
let questions_content () =
  let is_draft =
    match !where_am_i with Election { status = Draft; _ } -> true | _ -> false
  in
  let* qs =
    if is_draft then
      let* draft = Cache.get_until_success Cache.draft in
      Lwt.return draft.draft_questions.t_questions
    else
      let* elec = Cache.get_until_success Cache.e_elec in
      Lwt.return elec.e_questions
  in
  all_gen_quest := Array.map q_to_gen qs;
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