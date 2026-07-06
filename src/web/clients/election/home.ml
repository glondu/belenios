(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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
open Js_of_ocaml_tyxml
open Tyxml_js.Html
open Belenios
open Belenios_web_api
open Belenios_js.Common
open Common

let format_period x =
  let ( // ) x m =
    let r = Float.floor (x /. m) in
    (x -. (r *. m), r)
  in
  let x, days = x // 86400. in
  let x, hours = x // 3600. in
  let seconds, minutes = x // 60. in
  let b = Buffer.create 16 in
  let () = if days > 0. then Printf.bprintf b "%g:" days in
  let () = if days > 0. || hours > 0. then Printf.bprintf b "%02g:" hours in
  let () = Printf.bprintf b "%02g:%02g" minutes seconds in
  Buffer.contents b

let countdown update_state fmt end_ =
  let end_ = Int64.to_float end_ in
  let endf =
    let t = new%js Js.date_fromTimeValue (Js.float (end_ *. 1000.)) in
    Js.to_string t##toLocaleString
  in
  let elt = span [] in
  let dom = Tyxml_js.To_dom.of_span elt in
  let interval = ref None in
  let timer () =
    let delta =
      Float.ceil (end_ -. (Js.to_float (new%js Js.date_now)##valueOf /. 1000.))
    in
    if delta >= 0. then
      dom##.textContent :=
        Js.some (Js.string (Printf.sprintf fmt endf (format_period delta)))
    else
      let () =
        match !interval with
        | None -> ()
        | Some i -> Dom_html.window##clearInterval i
      in
      let@ () = Lwt.async in
      let* () = Lwt_js.sleep 1. in
      clear_cache ();
      update_state ()
  in
  timer ();
  interval :=
    Some (Dom_html.window##setInterval (Js.wrap_callback timer) (Js.float 500.));
  elt

let make_object_link uuid h =
  let href = !/Belenios_web_api.Endpoints.((election_object uuid h).path) in
  a ~href (Hash.to_b64 h)

let make_audit_div (election : Election.t) cache =
  let open (val !Belenios_js.I18n.gettext) in
  let open (val election) in
  let audit_election =
    [
      tr
        [
          td [ txt @@ s_ "Election identifier" ];
          td [ code [ txt @@ Uuid.to_string uuid ] ];
        ];
      tr
        [
          td [ txt @@ s_ "Election fingerprint" ];
          td [ code [ make_object_link uuid fingerprint ] ];
        ];
    ]
  in
  let audit_admin =
    [
      tr
        [
          td [ txt @@ s_ "Administrator" ];
          td [ txt @@ Option.value template.administrator ~default:"N/A" ];
        ];
    ]
  in
  let audit_voters =
    [
      tr
        ~a:[ a_id "voters" ]
        [
          td [ txt @@ s_ "Number of voters" ];
          td [ txt @@ string_of_int cache.checksums.num_voters ];
        ];
      tr
        [
          td [ txt @@ s_ "Voter list fingerprint" ];
          td [ code [ txt @@ Hash.to_b64 cache.voters_hash ] ];
        ];
    ]
  in
  let audit_sealing =
    match cache.sealing_log with
    | None -> []
    | Some h ->
        [
          tr
            [
              td [ txt @@ s_ "Sealing log fingerprint" ];
              td [ code [ txt @@ Hash.to_b64 h ] ];
            ];
        ]
  in
  let audit_voter_weight =
    match cache.checksums.weights with
    | Some { total; min; max } ->
        [
          tr
            [
              td [ txt @@ s_ "Voters' total weight" ];
              td
                [
                  Printf.ksprintf txt "%s (min: %s, max: %s)"
                    (Weight.to_string total) (Weight.to_string min)
                    (Weight.to_string max);
                ];
            ];
        ]
    | _ -> []
  in
  let checksums = cache.checksums in
  let format_tc id xs =
    ul
      ~a:[ a_id id ]
      (List.map
         (fun (x : trustee_checksum) ->
           let name =
             match x.name with None -> em [ txt "(server)" ] | Some x -> txt x
           in
           li
             [
               name;
               txt " ";
               code [ Printf.ksprintf txt "(%s)" (Hash.to_b64 x.checksum) ];
             ])
         xs)
  in
  let audit_trustees_mandatory =
    match checksums.trustees_basic with
    | [] -> []
    | l ->
        [
          tr
            [
              td
                [
                  txt
                  @@ s_
                       "All of the following trustees (verification keys) are \
                        needed to decrypt the result:";
                ];
              td [ format_tc "trustees" l ];
            ];
        ]
  in
  let audit_trustees_threshold =
    let format_ttc className xs =
      ul
        ~a:[ a_class [ className ] ]
        (List.map
           (fun (x : trustee_checksum) ->
             li
               [
                 Printf.ksprintf txt "%s " @@ Option.value ~default:"N/A" x.name;
                 code [ Printf.ksprintf txt "(%s)" (Hash.to_b64 x.checksum) ];
               ])
           xs)
    in
    List.map
      (fun (x : trustee_threshold_set) ->
        tr
          [
            td
              [
                Printf.ksprintf txt
                  (f_
                     "%d of the following %d trustees [public keys] are needed \
                      to decrypt the election result:")
                  x.threshold (List.length x.trustees);
              ];
            td [ format_ttc "trustees_threshold" x.trustees ];
          ])
      checksums.trustees_threshold
  in
  let audit_credentials =
    [
      tr
        [
          td [ txt @@ s_ "Credentials authority" ];
          td
            [ txt @@ Option.value template.credential_authority ~default:"N/A" ];
        ];
      tr
        ~a:[ a_id "credentials" ]
        [
          td [ txt @@ s_ "Credentials fingerprint" ];
          td [ code [ make_object_link uuid checksums.public_credentials ] ];
        ];
    ]
  in
  let audit_shuffles =
    match checksums.shuffles with
    | None -> []
    | Some xs ->
        [
          tr
            [
              td
                [
                  txt
                  @@ s_ "Trustees shuffled the ballots in the following order:";
                ];
              td [ format_tc "shuffles" xs ];
            ];
        ]
  in
  let audit_tally =
    match checksums.encrypted_tally with
    | None -> []
    | Some x ->
        [
          tr
            [
              td [ txt @@ s_ "Fingerprint of the encrypted tally" ];
              td [ code [ make_object_link uuid x ] ];
            ];
        ]
  in
  let audit_final =
    match checksums.final with
    | None -> []
    | Some x ->
        [
          tr
            [
              td [ txt @@ s_ "Final fingerprint" ];
              td [ code [ make_object_link uuid x ] ];
            ];
        ]
  in
  div
    ~a:[ a_class [ "home-audit" ] ]
    [
      div
        ~a:[ a_class [ "home-audit__title" ] ]
        [ txt @@ s_ "About this election" ];
      table
        (List.concat
           [
             audit_election;
             audit_admin;
             audit_voters;
             audit_voter_weight;
             audit_trustees_mandatory;
             audit_trustees_threshold;
             audit_credentials;
             audit_shuffles;
             audit_tally;
             audit_final;
             audit_sealing;
           ]);
    ]

let markup =
  let module M = Belenios_ui.Markup_light.Make (Tyxml_js) in
  M.markup

let majority_judgment_content uuid q (r : Method_mj.result) =
  let open Method_mj in
  let open (val !Belenios_js.I18n.gettext) in
  let explicit_winners =
    let open Question.Non_homomorphic in
    List.map (List.map (fun i -> q.answers.(i))) r.winners
  in
  let pretty_winners =
    List.map
      (fun l ->
        li
          [
            (match l with
            | [] -> failwith "anomaly in majority_judgment_content"
            | [ x ] -> markup x
            | l ->
                div
                  [
                    txt @@ s_ "Tie:"; ul (List.map (fun x -> li [ markup x ]) l);
                  ]);
          ])
      explicit_winners
  in
  let valid_format =
    match r.blank with
    | Some _ -> f_ "%d valid (non-blank) ballot(s)"
    | None -> f_ "%d valid ballot(s)"
  in
  let valid = div [ Printf.ksprintf txt valid_format r.valid ] in
  let blank =
    match r.blank with
    | Some b -> div [ Printf.ksprintf txt (f_ "%d blank ballot(s)") b ]
    | None -> txt ""
  in
  let invalid =
    a_data ~mime_type:"application/json"
      ~filename:(Printf.sprintf "invalid_ballots-%s.json" (Uuid.to_string uuid))
      ~data:(!+yojson_of_ballots r.invalid)
    @@ Printf.sprintf (f_ "%d invalid ballot(s)") (Array.length r.invalid)
  in
  let invalid = div [ invalid ] in
  [
    div
      [
        txt @@ s_ "According to Majority Judgment, the ranking is:";
        ol ~a:[ a_class [ "majority_judgment_ranking" ] ] pretty_winners;
      ];
    valid;
    blank;
    invalid;
  ]

let schulze_content q (r : Method_schulze.result) =
  let open (val !Belenios_js.I18n.gettext) in
  let valid_format =
    match r.blank with
    | Some _ -> f_ "%d valid (non-blank) ballot(s)"
    | None -> f_ "%d valid ballot(s)"
  in
  let valid = div [ Printf.ksprintf txt valid_format r.valid ] in
  let blank =
    match r.blank with
    | Some b -> div [ Printf.ksprintf txt (f_ "%d blank ballot(s)") b ]
    | None -> txt ""
  in
  let explicit_winners =
    let open Question.Non_homomorphic in
    List.map (List.map (fun i -> q.answers.(i))) r.winners
  in
  let pretty_winners =
    List.map
      (fun l ->
        li
          [
            (match l with
            | [] -> failwith "anomaly in schulze_content"
            | [ x ] -> markup x
            | l ->
                div
                  [
                    txt @@ s_ "Tie:"; ul (List.map (fun x -> li [ markup x ]) l);
                  ]);
          ])
      explicit_winners
  in
  let explanation =
    div
      ~a:[ a_class [ "schulze_explanation" ] ]
      [
        txt
        @@ s_
             "A Condorcet winner is a candidate that is preferred over all the \
              other candidates.";
        txt " ";
        txt
        @@ s_
             "Several techniques exist to decide which candidate to elect when \
              there is no Condorcet winner.";
        txt " ";
        txt @@ s_ "We use here the Schulze method and we refer voters to ";
        a ~href:"https://en.wikipedia.org/wiki/Condorcet_method#Schulze_method"
          (s_ "the Wikipedia page");
        txt @@ s_ " for more information.";
      ]
  in
  [
    explanation;
    txt @@ s_ "The Schulze winners are:";
    ol pretty_winners;
    valid;
    blank;
  ]

let stv_content uuid q (r : Method_stv.result) =
  let open Method_stv in
  let open (val !Belenios_js.I18n.gettext) in
  let winners =
    let open Question.Non_homomorphic in
    r.winners
    |> List.map (fun i -> q.answers.(i))
    |> List.map (fun l -> li [ txt l ])
  in
  let invalid =
    ( r.invalid |> !+yojson_of_raw_ballots |> fun data ->
      a_data ~mime_type:"application/json"
        ~filename:
          (Printf.sprintf "invalid_ballots-%s.json" (Uuid.to_string uuid))
        ~data
      @@ Printf.sprintf (f_ "%d invalid ballot(s)") (Array.length r.invalid) )
    |> fun x ->
    div
      [
        x;
        txt ". ";
        txt
        @@ s_
             "A ballot is invalid if two candidates have been given the same \
              preference order or if a rank is missing.";
      ]
  in
  let events =
    r.events |> !+yojson_of_events |> fun data ->
    a_data ~mime_type:"application/json"
      ~filename:(Printf.sprintf "raw_stv_events-%s.json" (Uuid.to_string uuid))
      ~data
    @@ s_ "Raw events"
  in
  let tie =
    if
      List.exists
        (function `TieWin _ | `TieLose _ -> true | _ -> false)
        r.events
    then
      div
        [
          txt @@ s_ "There has been at least one tie.";
          txt " ";
          txt
          @@ s_
               "Many variants of STV exist, depending for example on how to \
                break ties.";
          txt " ";
          txt
          @@ s_
               "In our implementation, when several candidates have the same \
                number of votes when they are ready to be elected or \
                eliminated, we follow the order in which candidates were \
                listed in the election.";
          txt " ";
          txt
          @@ s_
               "Such candidates are marked as \"TieWin\" when they are elected \
                and as \"TieLose\" if they have lost.";
          txt " ";
          txt @@ s_ "Look at the raw events for more details.";
        ]
    else txt ""
  in
  [
    div [ txt @@ s_ "The Single Transferable Vote winners are:"; ul winners ];
    tie;
    div [ events ];
    div [ invalid ];
  ]

let format_question_result uuid r (Q question : Question.t) =
  let open (val !Belenios_js.I18n.gettext) in
  let open Question in
  let module Q = (val question.type_) in
  (fun (type a) (id : a id) (q : a) ->
    match id with
    | Homomorphic.Id ->
        let open Homomorphic in
        let r = !*result_of_yojson r in
        let answers = Array.to_list q.answers in
        let answers =
          match q.blank with
          | true -> s_ "Blank vote" :: answers
          | false -> answers
        in
        let answers =
          List.mapi
            (fun j x ->
              tr [ td [ markup x ]; td [ txt @@ Weight.to_string r.(j) ] ])
            answers
        in
        let answers =
          match answers with
          | [] -> txt ""
          | y :: ys -> (
              match q.blank with
              | true -> table (ys @ [ y ])
              | false -> table (y :: ys))
        in
        li
          ~a:[ a_class [ "result_question_item" ] ]
          [
            div ~a:[ a_class [ "result_question" ] ] [ markup q.question ];
            answers;
          ]
    | Non_homomorphic.Id ->
        let open Non_homomorphic in
        let ballots = !*result_of_yojson r in
        let applied_counting_method, show_others =
          match Non_homomorphic.get_counting_method question.extra with
          | `None -> (txt "", true)
          | `MajorityJudgment o ->
              let ngrades = Array.length o.grades in
              let nchoices = Array.length q.answers in
              let blank_allowed = o.blank in
              let mj =
                Method_mj.compute ~nchoices ~ngrades ~blank_allowed ballots
              in
              let contents = majority_judgment_content uuid q mj in
              (div ~a:[ a_class [ "majority_judgment_result" ] ] contents, false)
          | `Schulze o ->
              let nchoices = Array.length q.answers in
              let blank_allowed = o.blank in
              let r = Method_schulze.compute ~nchoices ~blank_allowed ballots in
              let contents = schulze_content q r in
              (div ~a:[ a_class [ "schulze_result" ] ] contents, false)
          | `STV o ->
              let nseats = o.seats in
              let r = Method_stv.compute ~nseats ballots in
              let contents = stv_content uuid q r in
              (div ~a:[ a_class [ "stv_result" ] ] contents, false)
        in
        let others =
          if show_others then
            div
              [
                txt
                  (s_ "It is up to you to apply your favorite counting method.");
              ]
          else txt ""
        in
        li
          ~a:[ a_class [ "result_question_item" ] ]
          [
            div ~a:[ a_class [ "result_question" ] ] [ markup q.question ];
            applied_counting_method;
            div
              [
                txt (s_ "The raw results can be viewed in the ");
                a_data ~mime_type:"application/json"
                  ~filename:
                    (Printf.sprintf "raw_result-%s.json" (Uuid.to_string uuid))
                  ~data:r
                @@ s_ "JSON result";
                txt ". ";
                txt
                  (s_
                     "It contains all submitted ballots in clear, in random \
                      order.");
                others;
              ];
          ]
    | Lists.Id ->
        let open Lists in
        let r = r |> !*result_of_yojson in
        let answers = Array.to_list q.answers in
        let line_of_candidate name votes =
          tr [ td [ markup name ]; td [ txt @@ Weight.to_string votes ] ]
        in
        let answers =
          List.flatten
            (List.mapi
               (fun list_index x ->
                 Array.to_list
                   (Array.mapi
                      (fun candidate_index x ->
                        line_of_candidate x r.(list_index).(candidate_index))
                      x))
               answers)
        in
        let answers =
          match answers with [] -> txt "" | y :: ys -> table (y :: ys)
        in
        li
          ~a:[ a_class [ "result_question_item" ] ]
          [
            div ~a:[ a_class [ "result_question" ] ] [ markup q.question ];
            answers;
          ]
    | _ ->
        li
          ~a:[ a_class [ "result_question_item" ] ]
          [ div [ txt @@ s_ "Unsupported question type" ] ])
    Q.Id question.value

let make_result_div (election : Election.t) t ~result =
  let open (val !Belenios_js.I18n.gettext) in
  let open (val election) in
  let questions = template.questions in
  let r = election_result_of_yojson result_of_yojson result in
  let nballots = t.num_tallied in
  let total_weight = t.total_weight in
  let div_total_weight =
    if not Weight.(is_int total_weight nballots) then
      div
        [
          txt @@ s_ "Total weight of accepted ballots:";
          txt " ";
          txt @@ Weight.to_string total_weight;
        ]
    else txt ""
  in
  let raw_result =
    a_data ~mime_type:"application/json"
      ~filename:(Printf.sprintf "raw_result-%s.json" (Uuid.to_string uuid))
      ~data:(Json.to_string result)
    @@ s_ "raw result"
  in
  [
    ul
      (Array.map2
         (format_question_result uuid)
         (to_generic_result r.result |> Array.map Json.to_string)
         questions
      |> Array.to_list);
    div
      [ txt @@ s_ "Number of accepted ballots: "; txt (string_of_int nballots) ];
    div_total_weight;
    div [ txt @@ s_ "You can also download the "; raw_result; txt "." ];
  ]

let home configuration ?credential uuid =
  let open (val !Belenios_js.I18n.gettext) in
  let@ election cont =
    let* x = get_election uuid in
    match x with
    | None -> Lwt.return @@ error "Could not get election parameters!"
    | Some x -> cont x
  in
  let module W = (val election) in
  let@ dates cont =
    let* x = get_dates uuid in
    match x with
    | None -> Lwt.return @@ error "Could not get automatic dates!"
    | Some x -> cont x
  in
  let now = datetime_now () in
  let state = div [] in
  let middle = div [] in
  let rec update_state () =
    let@ status cont =
      let* x = get_status uuid in
      match x with
      | None ->
          replace_contents !!!state [ txt "Could not get election status!" ];
          clear_content !!!middle;
          Lwt.return_unit
      | Some x -> cont x
    in
    let refresh () =
      let handler _ =
        clear_cache ();
        Lwt.async update_state;
        false
      in
      let href = Printf.sprintf "#%s" (Uuid.to_string uuid) in
      a ~href ~a:[ a_onclick handler ] @@ s_ "Refresh status"
    in
    let state' =
      match status.state with
      | `Draft ->
          [
            b
              [
                txt
                @@ s_ "The election is being prepared. Please come back later.";
              ];
            txt " ";
            refresh ();
          ]
      | `Closed ->
          let it_will_open =
            match dates.open_ with
            | Some t when now < t ->
                [
                  div
                    [
                      countdown update_state
                        (f_ "It will open at %s (time left: %s).")
                        t;
                    ];
                ]
            | _ -> [ txt " "; refresh () ]
          in
          List.concat
            [
              [ b [ txt @@ s_ "This election is currently closed." ] ];
              it_will_open;
            ]
      | `Open ->
          let it_will_close =
            match dates.close with
            | Some t when now < t ->
                [
                  div
                    [
                      countdown update_state
                        (f_ "The election will close at %s (time left: %s).")
                        t;
                    ];
                ]
            | _ -> [ txt " "; refresh () ]
          in
          it_will_close
      | `Shuffling ->
          [
            b [ txt @@ s_ "The election is closed and being tallied." ];
            txt " ";
            refresh ();
          ]
      | `EncryptedTally ->
          [
            b [ txt @@ s_ "The election is closed and being tallied." ];
            txt " ";
            refresh ();
          ]
      | `Tallied -> [ b [ txt @@ s_ "This election has been tallied." ] ]
      | `Archived -> [ b [ txt @@ s_ "This election is archived." ] ]
    in
    let go_to_the_booth () =
      let disabled =
        match status.state with `Open -> [] | _ -> [ a_disabled () ]
      in
      let button =
        let uri = configuration.uris.home ^ "vote" in
        let handler _ =
          let params =
            match credential with None -> [] | Some c -> [ ("credential", c) ]
          in
          let params =
            ("uuid", Uuid.to_string uuid) :: params |> Url.encode_arguments
          in
          let href = Printf.sprintf "%s#%s" uri params in
          Dom_html.window##.location##.href := Js.string href;
          false
        in
        let a =
          a_id "start" :: a_onclick handler
          :: a_class [ "nice-button"; "nice-button--blue" ]
          :: disabled
        in
        Tyxml_js.Html.button ~a [ txt @@ s_ "Start" ]
      in
      div ~a:[ a_class [ "container--center"; "noprint" ] ] [ div [ button ] ]
    in
    let* middle' =
      let* result = get_result uuid in
      let* t = get_sized_encrypted_tally uuid in
      match (result, t) with
      | Some result, Some t -> Lwt.return @@ make_result_div election t ~result
      | _ -> Lwt.return [ go_to_the_booth () ]
    in
    replace_contents !!!state state';
    replace_contents !!!middle middle';
    Lwt.return_unit
  in
  let* () = update_state () in
  let ballots_link =
    let href = Printf.sprintf "#%s/ballots" (Uuid.to_string uuid) in
    p
      ~a:[ a_class [ "container--center"; "noprint" ] ]
      [
        a ~href
          ~a:
            [
              a_id "see-accepted-ballots";
              a_class [ "nice-button"; "nice-button--grey" ];
            ]
          (s_ "See accepted ballots");
      ]
  in
  let* audit_div =
    let* x = get_audit_cache uuid in
    match x with
    | None -> Lwt.return @@ div [ txt @@ s_ "Audit data not available." ]
    | Some audit_cache -> Lwt.return @@ make_audit_div election audit_cache
  in
  let contents =
    [
      div ~a:[ a_class [ "clear" ] ] [];
      state;
      br ();
      middle;
      br ();
      ballots_link;
      br ();
      audit_div;
    ]
  in
  let footer = [ make_audit_footer election ] in
  Lwt.return { title = W.template.name; contents; footer }
