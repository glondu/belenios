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
open Js_of_ocaml_lwt
open Belenios_core
open Belenios
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Belenios_js.Common
open Belenios_js.I18n.Gettext

module type ELECTION_LWT = ELECTION with type 'a m = 'a Lwt.t

let encryptBallot election cred plaintext () =
  let module E = (val election : ELECTION_LWT) in
  let module CD = Credential.MakeDerive (E.G) in
  let sk = CD.derive E.election.e_uuid cred in
  let* b = E.E.create_ballot ~sk plaintext in
  let s = E.string_of_ballot b in
  set_textarea "ballot" s;
  set_content "ballot_tracker" (sha256_b64 s);
  set_element_display "encrypting_div" "none";
  set_element_display "ballot_div" "block";
  Dom_html.window##.onbeforeunload := Dom_html.no_handler;
  Lwt.return ()

let progress_step n =
  let () =
    let old_ = Printf.sprintf "progress%d" (n-1) in
    let$ old_ = document##getElementById (Js.string old_) in
    old_##.style##.fontWeight := Js.string "normal"
  in
  let () =
    let new_ = Printf.sprintf "progress%d" n in
    let$ new_ = document##getElementById (Js.string new_) in
    new_##.style##.fontWeight := Js.string "bold"
  in ()

type questionWidget =
  {
    div : Dom_html.divElement Js.t;
    extract_answers : unit -> (int array * Dom_html.divElement Js.t) option;
  }

let createHomomorphicQuestionWidget q =
  let div = Dom_html.createDiv document in
  let open Question_h_t in
  let () =
    let c = Dom_html.createH2 document in
    append_with_br c q.q_question;
    Dom.appendChild div c
  in
  let () =
    let c = Dom_html.createDiv document in
    let s = Printf.sprintf (f_ "Select between %d and %d answer(s)") q.q_min q.q_max in
    let t = document##createTextNode (Js.string s) in
    Dom.appendChild c t;
    Dom.appendChild div c
  in
  let q_answers = match q.q_blank with
    | Some true -> Array.append [|s_ "Blank vote"|] q.q_answers
    | _ -> q.q_answers
  in
  let answers = Array.make (Array.length q_answers) 0 in
  let () =
    let choices = Dom_html.createDiv document in
    choices##.className := Js.string "answer_div";
    let choices_divs = Array.mapi (fun i a ->
      let container = Dom_html.createDiv document in
      let div = Dom_html.createLabel document in
      let checkbox = Dom_html.createInput ~_type:(Js.string "checkbox") document in
      if answers.(i) > 0 then checkbox##.checked := Js.bool true;
      checkbox##.style##.cursor := Js.string "pointer";
      Dom.appendChild div checkbox;
      checkbox##.onclick := Dom_html.handler (fun _ ->
        answers.(i) <- if Js.to_bool checkbox##.checked then 1 else 0;
        Js._true
      );
      append_with_br div a;
      Dom.appendChild container div;
      container
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
        Printf.ksprintf alert (f_ "You must select at least %d answer(s)") q.q_min;
        false
      ) else if total > q.q_max then (
        Printf.ksprintf alert (f_ "You must select at most %d answer(s)") q.q_max;
        false
      ) else true
    in
    match q.q_blank with
    | Some true ->
       let answers' = Array.sub answers 1 (Array.length answers - 1) in
       let total = Array.fold_left (+) 0 answers' in
       if answers.(0) > 0 then (
         if total <> 0 then
           (alert (s_ "No other choices are allowed when voting blank"); false)
         else true
       ) else check_min_max total
    | _ ->
       let total = Array.fold_left (+) 0 answers in
       check_min_max total
  in
  let create_summary () =
    let e = Dom_html.createDiv document in
    let h = Dom_html.createH3 document in
    append_with_br h q.q_question;
    Dom.appendChild e h;
    let ul = Dom_html.createUl document in
    let checked = ref 0 in
    Array.iteri
      (fun i a ->
        if a > 0 then (
          incr checked;
          let li = Dom_html.createLi document in
          let text = match q.q_blank with
            | Some true -> if i = 0 then s_ "Blank vote" else q.q_answers.(i-1)
            | _ -> q.q_answers.(i)
          in
          append_with_br li text;
          Dom.appendChild ul li;
        )
      ) answers;
    if !checked = 0 then (
      let t = document##createTextNode (Js.string (s_ "(nothing)")) in
      Dom.appendChild ul t
    );
    Dom.appendChild e ul;
    e
  in
  let extract_answers () =
    if check_constraints () then
      Some (Array.copy answers, create_summary ())
    else
      None
  in
  {div; extract_answers}

let createNonHomomorphicQuestionWidget q =
  let div = Dom_html.createDiv document in
  let open Question_nh_t in
  let () =
    let c = Dom_html.createH2 document in
    append_with_br c q.q_question;
    Dom.appendChild div c
  in
  let answers = Array.make (Array.length q.q_answers) 0 in
  let inputs =
    let choices = Dom_html.createDiv document in
    choices##.className := Js.string "answer_div";
    let choices_divs = Array.mapi (fun i a ->
      let div = Dom_html.createDiv document in
      let input = Dom_html.createInput document in
      input##.size := 5;
      input##.value := Js.string (string_of_int answers.(i));
      Dom.appendChild div input;
      input##.onchange :=
        Dom_html.handler (fun _ ->
            try
              let x = int_of_string (Js.to_string input##.value) in
              if x < 0 then raise Exit;
              if x > 255 then raise Exit;
              answers.(i) <- x;
              Js._true
            with _ ->
              alert (s_ "Value must be an integer between 0 and 255.");
              Js._false
          );
      append_with_br div (" " ^ a);
      input, div
    ) q.q_answers
    in
    for i = 0 to Array.length choices_divs - 1 do
      Dom.appendChild choices (snd choices_divs.(i))
    done;
    Dom.appendChild div choices;
    Array.map fst choices_divs
  in
  let () =
    let d = Dom_html.createDiv document in
    d##.style##.margin := Js.string "1em";
    Dom.appendChild div d;
    let div_note = Dom_html.createDiv document in
    Dom.appendChild d div_note;
    Dom.appendChild div_note (document##createTextNode (Js.string (s_ "Notes:")));
    let list = Dom_html.createUl document in
    Dom.appendChild div_note list;
    let item1 = Dom_html.createLi document in
    Dom.appendChild list item1;
    Dom.appendChild item1 (document##createTextNode (Js.string (s_ "If you are asked to grade candidates (majority judgement) then 1 is the best grade, higher numbers are worse.")));
    let item2 = Dom_html.createLi document in
    Dom.appendChild list item2;
    Dom.appendChild item2 (document##createTextNode (Js.string (s_ "If you are asked to rank candidates (Condorcet, STV, ...) then use 1 for your first choice, 2 for the second, etc.")));
    let div_warning = Dom_html.createDiv document in
    Dom.appendChild d div_warning;
    Dom.appendChild div_warning (document##createTextNode (Js.string (s_ "Warning: the system will accept any integer between 0 and 255 but, according to the election rules, invalid ballots (score too high or candidates not properly ranked) will be rejected at the end of the election.")))
  in
  let check_constraints () =
    let n = Array.length inputs - 1 in
    let rec loop i =
      if i < n then
        let valid =
          let x =
            try Some (int_of_string (Js.to_string inputs.(i)##.value))
            with _ -> None
          in
          match x with
          | Some y -> y = answers.(i)
          | None -> false
        in
        if not valid then (
          alert (s_ "At least one of the answers is invalid!");
          false
        ) else loop (i + 1)
      else true
    in
    loop 0
  in
  let create_summary () =
    let e = Dom_html.createDiv document in
    let h = Dom_html.createH3 document in
    append_with_br h q.q_question;
    Dom.appendChild e h;
    let ul = Dom_html.createUl document in
    Array.iteri
      (fun i a ->
        let li = Dom_html.createLi document in
        let text = Printf.sprintf "%d (%s)" a q.q_answers.(i) in
        append_with_br li text;
        Dom.appendChild ul li;
      ) answers;
    Dom.appendChild e ul;
    e
  in
  let extract_answers () =
    if check_constraints () then
      Some (Array.copy answers, create_summary ())
    else
      None
  in
  {div; extract_answers}

let createQuestionWidget = function
  | Question.Homomorphic q -> createHomomorphicQuestionWidget q
  | Question.NonHomomorphic (q, _) -> createNonHomomorphicQuestionWidget q

let createLabeledButton label =
  let b = Dom_html.createButton document in
  let t = document##createTextNode (Js.string label) in
  Dom.appendChild b t;
  b

let arrayMapOptionGet xs =
  try
    Array.map
      (function
       | Some x -> x
       | None -> raise Exit
      ) xs
    |> (fun x -> Some x)
  with
  | Exit -> None

let appendQuestionNavigation question_div sk params qs =
  let qs = Array.map createQuestionWidget qs in
  let n = Array.length qs in
  let answers = Array.make n None in
  if n < 1 then failwith "no questions";
  let cur = ref 0 in
  Dom.appendChild question_div qs.(!cur).div;
  let btns = Dom_html.createDiv document in
  btns##.style##.textAlign := Js.string "center";
  let btn_prev = createLabeledButton (s_ "Previous") in
  let btn_next = createLabeledButton (s_ "Next") in
  btn_prev##.id := Js.string "btn_prev";
  btn_next##.id := Js.string "btn_next";
  Dom.appendChild btns btn_prev;
  Dom.appendChild btns btn_next;
  btn_prev##.style##.display := Js.string "none";
  btn_prev##.onclick :=
    Dom_html.handler
      (fun _ ->
        answers.(!cur) <- qs.(!cur).extract_answers ();
        match answers.(!cur) with
        | None -> Js._false
        | Some _ ->
           decr cur;
           let d = if !cur = 0 then "none" else "inline" in
           btn_prev##.style##.display := Js.string d;
           Dom.replaceChild question_div qs.(!cur).div qs.(!cur + 1).div;
           Js._true
      );
  btn_next##.onclick :=
    Dom_html.handler
      (fun _ ->
        answers.(!cur) <- qs.(!cur).extract_answers ();
        match answers.(!cur) with
        | None -> Js._false
        | Some _ ->
           if !cur < n - 1 then (
             incr cur;
             btn_prev##.style##.display := Js.string "inline";
             Dom.replaceChild question_div qs.(!cur).div qs.(!cur - 1).div;
             Js._true
           ) else (
             match arrayMapOptionGet answers with
             | None -> Js._false
             | Some answers ->
                question_div##.style##.display := Js.string "none";
                let plaintext = Array.map fst answers in
                set_textarea "choices" (string_of_plaintext plaintext);
                let () =
                  let$ e = document##getElementById (Js.string "pretty_choices") in
                  Array.iter (fun (_, a) -> Dom.appendChild e a) answers
                in
                Lwt_js_events.async (encryptBallot params sk plaintext);
                set_element_display "plaintext_div" "block";
                progress_step 3;
                Js._true
           )
      );
  Dom.appendChild question_div btns

let proceedWithCredential params intro_div qs cred =
  intro_div##.style##.display := Js.string "none";
  set_element_display "question_div" "block";
  Dom_html.window##.onbeforeunload :=
    Dom_html.handler (fun _ -> Js._false);
  progress_step 2;
  let$ e = document##getElementById (Js.string "question_div") in
  appendQuestionNavigation e cred params qs

let createStartButton params intro_div qs =
  let b = Dom_html.createButton document in
  b##.style##.fontSize := Js.string "20px";
  let t = document##createTextNode (Js.string (s_ "here")) in
  b##.onclick := Dom_html.handler (fun _ ->
    (match prompt (s_ "Please enter your credential:") with
     | Some cred ->
        let cred = String.trim cred in
        (match Credential.parse cred with
         | `Valid -> proceedWithCredential params intro_div qs cred
         | `Invalid ->
            alert (s_ "Invalid credential!")
         | `MaybePassword ->
            alert (s_ "This looks like a password... Maybe you looked at the wrong e-mail?")
        )
    | None -> ()
    );
    Js._false
  );
  Dom.appendChild b t;
  b

let loadElection credential () =
  set_element_display "election_loader" "none";
  set_element_display "wait_div" "none";
  set_element_display "booth_div" "block";
  let module R =
    struct
      let raw_election =
        match get_textarea_opt "election_params" with
        | Some x -> String.trim x
        | None -> failwith "election_params is missing"
    end
  in
  let module P = Election.Make (R) (LwtJsRandom) () in
  let params = P.election in
  set_content_with_br "election_name" params.e_name;
  set_content_with_br "election_description" params.e_description;
  set_content "election_uuid" (raw_string_of_uuid params.e_uuid);
  set_content "election_fingerprint" P.fingerprint;
  let$ e = document##getElementById (Js.string "intro") in
  match credential with
  | None ->
     let b = createStartButton (module P) e params.e_questions in
     let$ e = document##getElementById (Js.string "input_code") in
     Dom.appendChild e b
  | Some credential ->
     proceedWithCredential (module P) e params.e_questions credential

let get_prefix str =
  let n = String.length str in
  if n >= 4 then String.sub str 0 (n-4) else str

let get_params x =
  let n = String.length x in
  if n <= 1 || String.sub x 0 1 <> "#" then
    []
  else
    Url.decode_arguments (String.sub x 1 (n-1))

let load_uuid uuid credential =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* raw =
    let* x = Printf.ksprintf get "elections/%s/election.json" uuid in
    if x.code = 404 then (
      let* x = Printf.ksprintf get "draft/preview/%s/election.json" uuid in
      Lwt.return x.content
    ) else Lwt.return x.content
  in
  set_textarea "election_params" raw;
  run_handler (loadElection credential) ();
  Lwt.return_unit

let load_uuid_handler lang credential =
  match get_textarea_opt "uuid" with
  | Some uuid ->
     let encoded = Url.encode_arguments ["uuid", uuid; "lang", lang] in
     Dom_html.window##.location##.hash := Js.string encoded;
     load_uuid uuid credential
  | None -> Lwt.return_unit

let load_params_handler credential =
  set_element_display "div_ballot" "block";
  set_element_display "div_submit" "none";
  set_element_display "div_submit_manually" "block";
  run_handler (loadElection credential) ();
  Lwt.return_unit

let () =
  Lwt.async (fun () ->
      let* _ = Lwt_js_events.onload () in
      let params = get_params (Js.to_string Dom_html.window##.location##.hash) in
      let lang =
        match List.assoc_opt "lang" params with
        | Some x -> x
        | None -> "en"
      in
      let credential = List.assoc_opt "credential" params in
      let* () = Belenios_js.I18n.init "static" "voter" lang in
      let () =
        let$ e = document##getElementById (Js.string "load_uuid") in
        Lwt_js_events.async (fun () ->
            let* _ = Lwt_js_events.click e in
            load_uuid_handler lang credential
          )
      in
      let () =
        let$ e = document##getElementById (Js.string "load_params") in
        Lwt_js_events.async (fun () ->
            let* _ = Lwt_js_events.click e in
            load_params_handler credential
          )
      in
      match List.assoc_opt "uuid" params with
      | None ->
         set_element_display "wait_div" "none";
         set_element_display "election_loader" "block";
         Lwt.return_unit
      | Some uuid -> load_uuid uuid credential
    )
