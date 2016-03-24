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

open Platform
open Serializable_j
open Signatures
open Common

let document = Dom_html.window##document

let withElementById x f =
  Js.Opt.iter (document##getElementById (Js.string x)) f

let alert s : unit =
  let open Js.Unsafe in
  fun_call (variable "alert") [| s |> Js.string |> inject |]

let prompt s =
  let open Js.Unsafe in
  Js.Opt.map
    (fun_call (variable "prompt") [| s |> Js.string |> inject |])
    Js.to_string |> Js.Opt.to_option

let runHandler handler () =
  (try handler ()
   with e ->
     let msg = "Unexpected error: " ^ Printexc.to_string e in
     alert msg
  ); Js._false

let installHandler id handler =
  let f _ = runHandler handler () in
  withElementById id (fun e -> e##onclick <- Dom_html.handler f)

let getTextarea id =
  let res = ref None in
  withElementById id (fun e ->
    Js.Opt.iter
      (Dom_html.CoerceTo.textarea e)
      (fun x -> res := Some (Js.to_string (x##value)))
  );
  match !res with
  | None -> raise Not_found
  | Some x -> x

let setTextarea id z =
  withElementById id (fun e ->
    Js.Opt.iter
      (Dom_html.CoerceTo.textarea e)
      (fun x -> x##value <- Js.string z)
  )

let setNodeById id x =
  withElementById id (fun e ->
    let t = document##createTextNode (Js.string x) in
    Dom.appendChild e t
  )

let setDisplayById id x =
  withElementById id (fun e -> e##style##display <- Js.string x)

let prng = lazy (pseudo_rng (random_string secure_rng 16))

module LwtJsRandom = struct
  type 'a t = unit -> 'a Lwt.t
  let return x () = Lwt.return x
  let bind x f () = Lwt.bind (x ()) (fun y -> f y ())
  let fail x () = Lwt.fail x

  let random q =
    let size = Z.bit_length q / 8 + 1 in
    fun () ->
      lwt () = Lwt_js.yield () in
      let r = random_string (Lazy.force prng) size in
      Lwt.return Z.(of_bits r mod q)
end

let encryptBallot params cred plaintext () =
  let module P = (val params : ELECTION_DATA) in
  let module G = P.G in
  let module E = Election.MakeElection (G) (LwtJsRandom) in
  let module CD = Credential.MakeDerive (G) in
  let sk = CD.derive P.election.e_params.e_uuid cred in
  lwt randomness = E.make_randomness P.election () in
  lwt b = E.create_ballot P.election ~sk randomness plaintext () in
  let s = string_of_ballot G.write b in
  setTextarea "ballot" s;
  setNodeById "ballot_tracker" (sha256_b64 s);
  setDisplayById "encrypting_div" "none";
  setDisplayById "ballot_div" "block";
  Dom_html.window##onbeforeunload <- Dom_html.no_handler;
  Lwt.return ()

let progress_step n =
  let old_ = Printf.sprintf "progress%d" (n-1) in
  let new_ = Printf.sprintf "progress%d" n in
  withElementById old_ (fun e -> e##setAttribute (Js.string "style", Js.string ""));
  withElementById new_ (fun e -> e##setAttribute (Js.string "style", Js.string "font-weight: bold;"))

let rec createQuestionNode sk params question_div num_questions i prev (q, answers) next =
  (* Create div element for the current question. [i] and [(q,
     answers)] point to the current question. [List.rev prev @ [q,
     answers] @ next] is the list of all questions. *)
  let div = document##createElement (Js.string "div") in
  let () =
    let c = document##createElement (Js.string "h2") in
    let t = document##createTextNode (Js.string q.q_question) in
    Dom.appendChild c t;
    Dom.appendChild div c
  in
  let () =
    let c = document##createElement (Js.string "div") in
    let s = Printf.sprintf
      "Question #%d of %d — select between %d and %d answer(s)"
      (i + 1) num_questions q.q_min q.q_max
    in
    let t = document##createTextNode (Js.string s) in
    Dom.appendChild c t;
    Dom.appendChild div c
  in
  let () =
    let choices = document##createElement (Js.string "div") in
    Array.iteri (fun i a ->
      let div = document##createElement (Js.string "div") in
      let checkbox = document##createElement (Js.string "input") in
      let cb =
        match Js.Opt.to_option (Dom_html.CoerceTo.input checkbox) with
        | Some x -> x
        | None -> failwith "error while casting checkbox"
      in
      if answers.(i) > 0 then cb##checked <- Js.bool true;
      checkbox##setAttribute (Js.string "type", Js.string "checkbox");
      checkbox##setAttribute (Js.string "style", Js.string "cursor: pointer;");
      Dom.appendChild div checkbox;
      let t = document##createTextNode (Js.string a) in
      checkbox##onclick <- Dom_html.handler (fun _ ->
        answers.(i) <- if Js.to_bool cb##checked then 1 else 0;
        Js._true
      );
      Dom.appendChild div t;
      Dom.appendChild choices div
    ) q.q_answers;
    Dom.appendChild div choices
  in
  let check_constraints () =
    let total = Array.fold_left (+) 0 answers in
    if total < q.q_min then (
      Printf.ksprintf alert "You must select at least %d answer(s)" q.q_min;
      false
    ) else if total > q.q_max then (
      Printf.ksprintf alert "You must select at most %d answer(s)" q.q_max;
      false
    ) else true
  in
  let () =
    (* previous button *)
    let btns = document##createElement (Js.string "div") in
    btns##setAttribute (Js.string "style", Js.string "text-align: center;");
    let () =
      match prev with
      | [] ->
        (* first question, no "Previous" button *)
        ()
      | r :: prev ->
        let b = document##createElement (Js.string "button") in
        let t = document##createTextNode (Js.string "Previous") in
        b##onclick <- Dom_html.handler (fun _ ->
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
        let b = document##createElement (Js.string "button") in
        let t = document##createTextNode (Js.string "Next") in
        b##onclick <- Dom_html.handler (fun _ ->
         if check_constraints () then (
          let all = (q, answers) :: prev in
          let all_answers = List.rev_map snd all |> Array.of_list in
          let all_questions = List.rev_map fst all |> Array.of_list in
          setTextarea "choices" (string_of_plaintext all_answers);
          question_div##style##display <- Js.string "none";
          withElementById "pretty_choices" (fun e ->
            Array.iteri (fun i a ->
              let q = all_questions.(i) in
              let h = document##createElement (Js.string "h3") in
              let t = document##createTextNode (Js.string q.q_question) in
              Dom.appendChild h t;
              Dom.appendChild e h;
              let ul = document##createElement (Js.string "ul") in
              let checked = ref 0 in
              Array.iteri (fun i a ->
                if a > 0 then (
                  incr checked;
                  let li = document##createElement (Js.string "li") in
                  let t = document##createTextNode (Js.string q.q_answers.(i)) in
                  Dom.appendChild li t;
                  Dom.appendChild ul li;
                )
              ) a;
              if !checked = 0 then (
                let t = document##createTextNode (Js.string "(nothing)") in
                Dom.appendChild ul t
              );
              Dom.appendChild e ul;
            ) all_answers
          );
          Lwt_js_events.async (encryptBallot params sk all_answers);
          setDisplayById "plaintext_div" "block";
          progress_step 3;
          Js._false
         ) else Js._false
        );
        Dom.appendChild b t;
        Dom.appendChild btns b
      | r :: next ->
        let b = document##createElement (Js.string "button") in
        let t = document##createTextNode (Js.string "Next") in
        b##onclick <- Dom_html.handler (fun _ ->
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
  withElementById "question_div" (fun e ->
    let n = Array.length qs in
    let qs =
      Array.to_list qs |>
      List.map (fun q -> q, Array.make (Array.length q.q_answers) 0)
    in
    match qs with
    | [] -> failwith "no questions"
    | q :: next ->
      let div = createQuestionNode sk params e n 0 [] q next in
      Dom.appendChild e div
  )

let createStartButton params intro_div qs =
  let b = document##createElement (Js.string "button") in
  b##setAttribute (Js.string "style", Js.string "font-size:20px;");
  let t = document##createTextNode (Js.string "here") in
  b##onclick <- Dom_html.handler (fun _ ->
    (match prompt "Please enter your credential:" with
    | Some cred when Credential.check cred ->
      intro_div##style##display <- Js.string "none";
      setDisplayById "question_div" "block";
      Dom_html.window##onbeforeunload <- Dom_html.handler (fun _ ->
        Js._false
      );
      progress_step 2;
      addQuestions cred params qs
    | Some _ ->
       alert "Invalid credential!"
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
  setDisplayById "election_loader" "none";
  setDisplayById "booth_div" "block";
  let election_raw = getTextarea "election_params" |> drop_trailing_newline in
  let election_params = Group.election_params_of_string election_raw in
  let module P = (val election_params : ELECTION_DATA) in
  let params = P.election.e_params in
  setNodeById "election_name" params.e_name;
  setNodeById "election_description" params.e_description;
  setNodeById "election_uuid" (Uuidm.to_string params.e_uuid);
  setNodeById "election_fingerprint" P.election.e_fingerprint;
  withElementById "intro" (fun e ->
    let b = createStartButton election_params e params.e_questions in
    withElementById "input_code" (fun e -> Dom.appendChild e b)
  )

let split str prefix =
  let n = String.length str in
  let p = String.length prefix in
  if p <= n && String.sub str 0 p = prefix then
    Some (String.sub str p (n-p))
  else None

let () =
  Dom_html.window##onload <- Dom_html.handler (fun _ ->
    let s = Js.to_string Dom_html.window##location##search in
    (match split s "?election_url=" with
    | Some url ->
      let url = Url.urldecode url in
      withElementById "ballot_form" (fun e ->
        Js.Opt.iter
          (Dom_html.CoerceTo.form e)
          (fun e -> e##action <- Js.string (url ^ "cast"))
      );
      let open XmlHttpRequest in
      Lwt.async (fun () ->
        lwt raw = get (url ^ "election.json") in
        let () = setTextarea "election_params" raw.content in
        Lwt.return (runHandler loadElection ())
      )
    | None ->
      setDisplayById "election_loader" "block";
      installHandler "load_election" loadElection;
    );
    Js._false
  )
