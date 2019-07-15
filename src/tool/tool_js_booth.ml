(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Js_of_ocaml
open Platform
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Tool_js_common

let encryptBallot params cred plaintext () =
  let module P = (val params : ELECTION_DATA) in
  let module G = P.G in
  let module E = Election.Make (P) (LwtJsRandom) in
  let module CD = Credential.MakeDerive (G) in
  let sk = CD.derive P.election.e_params.e_uuid cred in
  let%lwt b = E.create_ballot ~sk plaintext () in
  let s = string_of_ballot G.write b in
  set_textarea "ballot" s;
  set_content "ballot_tracker" (sha256_b64 s);
  set_element_display "encrypting_div" "none";
  set_element_display "ballot_div" "block";
  Dom_html.window##.onbeforeunload := Dom_html.no_handler;
  Lwt.return ()

let progress_step n =
  let () =
    let old_ = Printf.sprintf "progress%d" (n-1) in
    document##getElementById (Js.string old_) >>== fun old_ ->
    old_##.style##.fontWeight := Js.string "normal"
  in
  let () =
    let new_ = Printf.sprintf "progress%d" n in
    document##getElementById (Js.string new_) >>== fun new_ ->
    new_##.style##.fontWeight := Js.string "bold"
  in ()

let appendStdQuestion div num_questions i q answers =
  let open Question_std_t in
  let () =
    let c = Dom_html.createH2 document in
    let t = document##createTextNode (Js.string q.q_question) in
    Dom.appendChild c t;
    Dom.appendChild div c
  in
  let () =
    let c = Dom_html.createDiv document in
    let fmt = Scanf.format_from_string
      (get_content "question_header") "%d%d%d%d"
    in
    let s = Printf.sprintf fmt
      (i + 1) num_questions q.q_min q.q_max
    in
    let t = document##createTextNode (Js.string s) in
    Dom.appendChild c t;
    Dom.appendChild div c
  in
  let q_answers = match q.q_blank with
    | Some true -> Array.append [|get_content "str_blank_vote"|] q.q_answers
    | _ -> q.q_answers
  in
  let () =
    let choices = Dom_html.createDiv document in
    choices##.className := Js.string "answer_div";
    let choices_divs = Array.mapi (fun i a ->
      let div = Dom_html.createDiv document in
      let checkbox = Dom_html.createInput ~_type:(Js.string "checkbox") document in
      if answers.(i) > 0 then checkbox##.checked := Js.bool true;
      checkbox##.style##.cursor := Js.string "pointer";
      Dom.appendChild div checkbox;
      let t = document##createTextNode (Js.string a) in
      checkbox##.onclick := Dom_html.handler (fun _ ->
        answers.(i) <- if Js.to_bool checkbox##.checked then 1 else 0;
        Js._true
      );
      Dom.appendChild div t;
      div
    ) q_answers
    in
    begin match q.q_blank with
    | Some true ->
       for i = 1 to Array.length choices_divs - 1 do
         Dom.appendChild choices choices_divs.(i)
       done;
       (* Put the blank choice at the end of the list *)
       Dom.appendChild choices (Dom_html.createBr document);
       Dom.appendChild choices choices_divs.(0)
    | _ ->
       for i = 0 to Array.length choices_divs - 1 do
         Dom.appendChild choices choices_divs.(i)
       done
    end;
    Dom.appendChild div choices
  in
  let check_constraints () =
    let check_min_max total =
      if total < q.q_min then (
        let fmt = Scanf.format_from_string (get_content "at_least") "%d" in
        Printf.ksprintf alert fmt q.q_min;
        false
      ) else if total > q.q_max then (
        let fmt = Scanf.format_from_string (get_content "at_most") "%d" in
        Printf.ksprintf alert fmt q.q_max;
        false
      ) else true
    in
    match q.q_blank with
    | Some true ->
       let answers' = Array.sub answers 1 (Array.length answers - 1) in
       let total = Array.fold_left (+) 0 answers' in
       if answers.(0) > 0 then (
         if total <> 0 then
           (alert (get_content "no_other_blank"); false)
         else true
       ) else check_min_max total
    | _ ->
       let total = Array.fold_left (+) 0 answers in
       check_min_max total
  in
  check_constraints

let appendOpenQuestion div q answers =
  let open Question_open_t in
  let () =
    let c = Dom_html.createH2 document in
    let t = document##createTextNode (Js.string q.q_question) in
    Dom.appendChild c t;
    Dom.appendChild div c
  in
  let inputs =
    let choices = Dom_html.createDiv document in
    choices##.className := Js.string "answer_div";
    let choices_divs = Array.mapi (fun i a ->
      let div = Dom_html.createDiv document in
      let input = Dom_html.createInput document in
      input##.size := 5;
      input##.value := Js.string (string_of_int answers.(i));
      Dom.appendChild div input;
      let t = document##createTextNode (Js.string (" " ^ a)) in
      input##.onchange :=
        Dom_html.handler (fun _ ->
            let x =
              try
                let x = int_of_string (Js.to_string input##.value) in
                if x < 0 then raise Exit;
                x
              with _ ->
                alert "Value must be a non-negative integer";
                0
            in
            answers.(i) <- x;
            Js._true
          );
      Dom.appendChild div t;
      input, div
    ) q.q_answers
    in
    for i = 0 to Array.length choices_divs - 1 do
      Dom.appendChild choices (snd choices_divs.(i))
    done;
    Dom.appendChild div choices;
    Array.map fst choices_divs
  in
  let check_constraints () =
    let n = Array.length inputs - 1 in
    let rec loop i =
      if i < n then
        if Js.string (string_of_int answers.(i)) <> inputs.(i)##.value then (
          alert "At least one of the answers is invalid!";
          false
        ) else loop (i + 1)
      else true
    in
    loop 0
  in
  check_constraints

let appendStdSummary e a q =
  let open Question_std_t in
  let h = Dom_html.createH3 document in
  let t = document##createTextNode (Js.string q.q_question) in
  Dom.appendChild h t;
  Dom.appendChild e h;
  let ul = Dom_html.createUl document in
  let checked = ref 0 in
  Array.iteri (fun i a ->
      if a > 0 then (
        incr checked;
        let li = Dom_html.createLi document in
        let text = match q.q_blank with
          | Some true -> if i = 0 then get_content "str_blank_vote" else q.q_answers.(i-1)
          | _ -> q.q_answers.(i)
        in
        let t = document##createTextNode (Js.string text) in
        Dom.appendChild li t;
        Dom.appendChild ul li;
      )
    ) a;
  if !checked = 0 then (
    let t = document##createTextNode (Js.string @@ get_content "str_nothing") in
    Dom.appendChild ul t
  );
  Dom.appendChild e ul

let appendOpenSummary e a q =
  let open Question_open_t in
  let h = Dom_html.createH3 document in
  let t = document##createTextNode (Js.string q.q_question) in
  Dom.appendChild h t;
  Dom.appendChild e h;
  let ul = Dom_html.createUl document in
  Array.iteri (fun i a ->
      let li = Dom_html.createLi document in
      let text = Printf.sprintf "%d (%s)" a q.q_answers.(i) in
      let t = document##createTextNode (Js.string text) in
      Dom.appendChild li t;
      Dom.appendChild ul li;
    ) a;
  Dom.appendChild e ul

let rec createQuestionNode sk params question_div num_questions i prev (q, answers) next =
  (* Create div element for the current question. [i] and [(q,
     answers)] point to the current question. [List.rev prev @ [q,
     answers] @ next] is the list of all questions. *)
  let div = Dom_html.createDiv document in
  let check_constraints =
    match q with
    | Question.Standard q -> appendStdQuestion div num_questions i q answers
    | Question.Open q -> appendOpenQuestion div q answers
  in
  let () =
    (* previous button *)
    let btns = Dom_html.createDiv document in
    btns##.style##.textAlign := Js.string "center";
    let () =
      match prev with
      | [] ->
        (* first question, no "Previous" button *)
        ()
      | r :: prev ->
        let b = Dom_html.createButton document in
        let t = document##createTextNode (Js.string @@ get_content "str_previous") in
        b##.onclick := Dom_html.handler (fun _ ->
          if check_constraints () then (
            let ndiv = createQuestionNode sk params
              question_div num_questions (i - 1) prev r ((q, answers) :: next)
            in
            Dom.replaceChild question_div ndiv div;
            Js._false
          ) else Js._false
        );
        Dom.appendChild b t;
        Dom.appendChild btns b;
    in
    let () =
      (* next button *)
      match next with
      | [] ->
        (* last question, the button leads to encryption page *)
        let b = Dom_html.createButton document in
        let t = document##createTextNode (Js.string @@ get_content "str_next") in
        b##.onclick := Dom_html.handler (fun _ ->
         if check_constraints () then (
          let all = (q, answers) :: prev in
          let all_answers = List.rev_map snd all |> Array.of_list in
          let all_questions = List.rev_map fst all |> Array.of_list in
          set_textarea "choices" (string_of_plaintext all_answers);
          question_div##.style##.display := Js.string "none";
          let () =
            document##getElementById (Js.string "pretty_choices") >>== fun e ->
            Array.iteri (fun i a ->
                match all_questions.(i) with
                | Question.Standard q -> appendStdSummary e a q
                | Question.Open q -> appendOpenSummary e a q
              ) all_answers
          in
          Lwt_js_events.async (encryptBallot params sk all_answers);
          set_element_display "plaintext_div" "block";
          progress_step 3;
          Js._false
         ) else Js._false
        );
        Dom.appendChild b t;
        Dom.appendChild btns b
      | r :: next ->
        let b = Dom_html.createButton document in
        let t = document##createTextNode (Js.string @@ get_content "str_next") in
        b##.onclick := Dom_html.handler (fun _ ->
          if check_constraints () then (
            let ndiv = createQuestionNode sk params
              question_div num_questions (i + 1) ((q, answers) :: prev) r next
            in
            Dom.replaceChild question_div ndiv div;
            Js._false
          ) else Js._false
        );
        Dom.appendChild b t;
        Dom.appendChild btns b;
    in
    Dom.appendChild div btns
  in
  div

let addQuestions sk params qs =
  document##getElementById (Js.string "question_div") >>== fun e ->
  let n = Array.length qs in
  let qs =
    Array.to_list qs |>
      List.map (fun q ->
          match q with
          | Question.Standard x -> q, Array.make (Question_std.question_length x) 0
          | Question.Open x -> q, Array.make (Array.length x.Question_open_t.q_answers) 0
        )
  in
  match qs with
  | [] -> failwith "no questions"
  | q :: next ->
     let div = createQuestionNode sk params e n 0 [] q next in
     Dom.appendChild e div

let createStartButton params intro_div qs =
  let b = Dom_html.createButton document in
  b##.style##.fontSize := Js.string "20px";
  let t = document##createTextNode (Js.string (get_content "str_here")) in
  b##.onclick := Dom_html.handler (fun _ ->
    (match prompt (get_content "enter_cred") with
    | Some cred when Credential.check cred ->
      intro_div##.style##.display := Js.string "none";
      set_element_display "question_div" "block";
      Dom_html.window##.onbeforeunload := Dom_html.handler (fun _ ->
        Js._false
      );
      progress_step 2;
      addQuestions cred params qs
    | Some _ ->
       alert (get_content "invalid_cred")
    | None -> ()
    );
    Js._false
  );
  Dom.appendChild b t;
  b

let drop_trailing_newline s =
  let n = String.length s in
  if n > 0 && s.[n-1] = '\n' then String.sub s 0 (n-1) else s

let loadElection () =
  set_element_display "election_loader" "none";
  set_element_display "wait_div" "none";
  set_element_display "booth_div" "block";
  let election_raw =
    match get_textarea_opt "election_params" with
    | Some x -> drop_trailing_newline x
    | None -> failwith "election_params is missing"
  in
  let election_params = Election.(get_group (of_string election_raw)) in
  let module P = (val election_params : ELECTION_DATA) in
  let params = P.election.e_params in
  set_content "election_name" params.e_name;
  set_content "election_description" params.e_description;
  set_content "election_uuid" (raw_string_of_uuid params.e_uuid);
  set_content "election_fingerprint" P.election.e_fingerprint;
  document##getElementById (Js.string "intro") >>== fun e ->
  let b = createStartButton election_params e params.e_questions in
  document##getElementById (Js.string "input_code") >>== fun e ->
  Dom.appendChild e b

let get_prefix str =
  let n = String.length str in
  if n >= 4 then String.sub str 0 (n-4) else str

let get_url x =
  let n = String.length x in
  if n <= 1 || String.sub x 0 1 <> "#" then
    None
  else
    let args = Url.decode_arguments (String.sub x 1 (n-1)) in
    List.assoc_opt "url" args

let load_url url =
  let open Lwt_xmlHttpRequest in
  Lwt.async (fun () ->
      let%lwt raw = get (url ^ "election.json") in
      let () = set_textarea "election_params" raw.content in
      Lwt.return (run_handler loadElection ())
    )

let load_url_handler _ =
  (match get_textarea_opt "url" with
   | Some url ->
      let encoded = Url.encode_arguments ["url", url] in
      Dom_html.window##.location##.hash := Js.string encoded;
      load_url url
   | None -> ()
  ); Js._false

let load_params_handler _ =
  set_element_display "div_ballot" "block";
  set_element_display "div_submit" "none";
  set_element_display "div_submit_manually" "block";
  Lwt.async (fun () ->
      Lwt.return (run_handler loadElection ())
    );
  Js._false

let onload_handler _ =
  let () =
    document##getElementById (Js.string "load_url") >>== fun e ->
    e##.onclick := Dom_html.handler load_url_handler
  in
  let () =
    document##getElementById (Js.string "load_params") >>== fun e ->
    e##.onclick := Dom_html.handler load_params_handler;
  in
  let () =
    match get_url (Js.to_string Dom_html.window##.location##.hash) with
    | None ->
       set_element_display "wait_div" "none";
       set_element_display "election_loader" "block";
    | Some url -> load_url url
  in Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload_handler
