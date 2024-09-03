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
open Js_of_ocaml_tyxml
open Tyxml_js.Html
open Belenios
open Belenios_api.Serializable_j
open Belenios_js.Common
open Belenios_js.Session
open Belenios_js.Secondary_ui

let format_period x =
  let open (val !Belenios_js.I18n.gettext) in
  let ( // ) x m =
    let r = Float.floor (x /. m) in
    (x -. (r *. m), r)
  in
  let x, days = x // 86400. in
  let x, hours = x // 3600. in
  let x, minutes = x // 60. in
  let seconds = Float.round x in
  let ( ++ ) x s = if x = 0. then "" else Printf.sprintf "%g%s" x s in
  let days = days ++ s_ " day(s)" in
  let hours = hours ++ s_ " hour(s)" in
  let minutes = minutes ++ s_ " minute(s)" in
  let seconds = seconds ++ s_ " second(s)" in
  [ days; hours; minutes; seconds ]
  |> List.filter (fun x -> x <> "")
  |> String.concat " "

let make_audit_footer election =
  let open (val !Belenios_js.I18n.gettext) in
  let open (val election : Election.ELECTION) in
  let uuid = Uuid.unwrap uuid in
  let parameters = !/(Printf.sprintf "elections/%s/election" uuid) in
  let public_data = !/(Printf.sprintf "elections/%s/archive" uuid) in
  let advanced = !!(Printf.sprintf "actions/cast?uuid=%s" uuid) in
  let administer = !!(Printf.sprintf "actions/admin?uuid=%s" uuid) in
  div
    ~a:[ a_style "line-height:1.5em;" ]
    [
      div
        [
          div [ txt @@ s_ "Election fingerprint: "; code [ txt fingerprint ] ];
          div
            [
              txt @@ s_ "Audit data: ";
              a ~href:parameters (s_ "parameters");
              txt ", ";
              a ~href:public_data (s_ "public data");
              txt ". ";
              a ~href:advanced (s_ "Advanced mode");
              txt ". ";
              a ~href:administer
                ~a:[ a_id @@ Printf.sprintf "election_admin_%s" uuid ]
                (s_ "Administer this election");
              txt ".";
            ];
        ];
    ]

let make_audit_div template cache =
  let open (val !Belenios_js.I18n.gettext) in
  let audit_admin =
    [
      tr
        [
          td [ txt @@ s_ "Administrator" ];
          td [ txt @@ Option.value template.t_administrator ~default:"N/A" ];
        ];
    ]
  in
  let audit_voters =
    [
      tr
        ~a:[ a_id "voters" ]
        [
          td [ txt @@ s_ "Number of voters" ];
          td [ txt @@ string_of_int cache.cache_checksums.ec_num_voters ];
        ];
      tr
        [
          td [ txt @@ s_ "Voter list fingerprint" ];
          td [ code [ txt @@ Hash.to_b64 cache.cache_voters_hash ] ];
        ];
    ]
  in
  let audit_voter_weight =
    match cache.cache_checksums.ec_weights with
    | Some { w_total; w_min; w_max } ->
        [
          tr
            [
              td [ txt @@ s_ "Voters' total weight" ];
              td
                [
                  Printf.ksprintf txt "%s (min: %s, max: %s)"
                    (Weight.to_string w_total) (Weight.to_string w_min)
                    (Weight.to_string w_max);
                ];
            ];
        ]
    | _ -> []
  in
  let checksums = cache.cache_checksums in
  let format_tc id xs =
    ul
      ~a:[ a_id id ]
      (List.map
         (fun x ->
           let name = Option.value x.tc_name ~default:"N/A" in
           li
             [
               Printf.ksprintf txt "%s " name;
               code [ Printf.ksprintf txt "(%s)" (Hash.to_b64 x.tc_checksum) ];
             ])
         xs)
  in
  let audit_trustees_mandatory =
    match checksums.ec_trustees with
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
           (fun x ->
             let name = Option.value x.ttc_name ~default:"N/A" in
             li
               [
                 Printf.ksprintf txt "%s " name;
                 code [ Printf.ksprintf txt "[%s]" (Hash.to_b64 x.ttc_pki_key) ];
               ])
           xs)
    in
    List.map
      (fun x ->
        tr
          [
            td
              [
                Printf.ksprintf txt
                  (f_
                     "%d of the following %d trustees [public keys] are needed \
                      to decrypt the election result:")
                  x.ts_threshold
                  (List.length x.ts_trustees);
              ];
            td [ format_ttc "trustees_threshold" x.ts_trustees ];
          ])
      checksums.ec_trustees_threshold
  in
  let audit_credentials =
    [
      tr
        [
          td [ txt @@ s_ "Credentials authority" ];
          td
            [
              txt @@ Option.value template.t_credential_authority ~default:"N/A";
            ];
        ];
      tr
        ~a:[ a_id "credentials" ]
        [
          td [ txt @@ s_ "Credentials fingerprint" ];
          td [ code [ txt @@ Hash.to_b64 checksums.ec_public_credentials ] ];
        ];
    ]
  in
  let audit_shuffles =
    match checksums.ec_shuffles with
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
    match checksums.ec_encrypted_tally with
    | None -> []
    | Some x ->
        [
          tr
            [
              td [ txt @@ s_ "Fingerprint of the encrypted tally" ];
              td [ code [ txt @@ Hash.to_b64 x ] ];
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
             audit_admin;
             audit_voters;
             audit_voter_weight;
             audit_trustees_mandatory;
             audit_trustees_threshold;
             audit_credentials;
             audit_shuffles;
             audit_tally;
           ]);
    ]

let markup x =
  let open Belenios_ui in
  let p =
    {
      Markup.bold = (fun _ xs -> span ~a:[ a_class [ "markup-b" ] ] xs);
      text = (fun _ x -> txt x);
      br = (fun _ -> br ());
      italic = (fun _ xs -> span ~a:[ a_class [ "markup-i" ] ] xs);
    }
  in
  try
    let lexbuf = Lexing.from_string x in
    let xs = Markup_parser.full Markup_lexer.token lexbuf in
    let xs = Markup.render p xs in
    span xs
  with _ -> span ~a:[ a_class [ "markup-error" ] ] [ txt x ]

let majority_judgment_content q r =
  let open (val !Belenios_js.I18n.gettext) in
  let explicit_winners =
    let open Belenios_question.Non_homomorphic.Syntax in
    List.map (List.map (fun i -> q.q_answers.(i))) r.mj_winners
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
    match r.mj_blank with
    | Some _ -> f_ "%d valid (non-blank) ballot(s)"
    | None -> f_ "%d valid ballot(s)"
  in
  let valid = div [ Printf.ksprintf txt valid_format r.mj_valid ] in
  let blank =
    match r.mj_blank with
    | Some b -> div [ Printf.ksprintf txt (f_ "%d blank ballot(s)") b ]
    | None -> txt ""
  in
  let invalid =
    string_of_mj_ballots r.mj_invalid
    |> encode_data_uri ~mime_type:"application/json"
  in
  let invalid =
    a ~href:invalid
      (Printf.sprintf (f_ "%d invalid ballot(s)") (Array.length r.mj_invalid))
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

let schulze_content q r =
  let open (val !Belenios_js.I18n.gettext) in
  let valid_format =
    match r.schulze_blank with
    | Some _ -> f_ "%d valid (non-blank) ballot(s)"
    | None -> f_ "%d valid ballot(s)"
  in
  let valid = div [ Printf.ksprintf txt valid_format r.schulze_valid ] in
  let blank =
    match r.schulze_blank with
    | Some b -> div [ Printf.ksprintf txt (f_ "%d blank ballot(s)") b ]
    | None -> txt ""
  in
  let explicit_winners =
    let open Belenios_question.Non_homomorphic.Syntax in
    List.map (List.map (fun i -> q.q_answers.(i))) r.schulze_winners
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

let stv_content q r =
  let open (val !Belenios_js.I18n.gettext) in
  let winners =
    let open Belenios_question.Non_homomorphic.Syntax in
    r.stv_winners
    |> List.map (fun i -> q.q_answers.(i))
    |> List.map (fun l -> li [ txt l ])
  in
  let invalid =
    ( r.stv_invalid |> string_of_mj_ballots
    |> encode_data_uri ~mime_type:"application/json"
    |> fun href ->
      a ~href
        (Printf.sprintf
           (f_ "%d invalid ballot(s)")
           (Array.length r.stv_invalid)) )
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
    r.stv_events |> string_of_stv_events
    |> encode_data_uri ~mime_type:"application/json"
    |> fun href -> a ~href (s_ "Raw events")
  in
  let tie =
    if
      List.exists
        (function `TieWin _ | `TieLose _ -> true | _ -> false)
        r.stv_events
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

let format_question_result r question =
  let open (val !Belenios_js.I18n.gettext) in
  let open Belenios_question in
  match question.value with
  | Homomorphic.Q x ->
      let open Homomorphic.Syntax in
      let r = result_of_string r in
      let answers = Array.to_list x.q_answers in
      let answers =
        match x.q_blank with
        | Some true -> s_ "Blank vote" :: answers
        | _ -> answers
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
            match x.q_blank with
            | Some true -> table (ys @ [ y ])
            | _ -> table (y :: ys))
      in
      li
        ~a:[ a_class [ "result_question_item" ] ]
        [
          div ~a:[ a_class [ "result_question" ] ] [ markup x.q_question ];
          answers;
        ]
  | Non_homomorphic.Q q ->
      let open Non_homomorphic.Syntax in
      let ballots = result_of_string r in
      let applied_counting_method, show_others =
        match get_counting_method question.extra with
        | `None -> (txt "", true)
        | `MajorityJudgment o ->
            let ngrades = Array.length o.mj_extra_grades in
            let nchoices = Array.length q.q_answers in
            let blank_allowed = o.mj_extra_blank in
            let mj =
              Methods.Majority_judgment.compute ~nchoices ~ngrades
                ~blank_allowed ballots
            in
            let contents = majority_judgment_content q mj in
            (div ~a:[ a_class [ "majority_judgment_result" ] ] contents, false)
        | `Schulze o ->
            let nchoices = Array.length q.q_answers in
            let blank_allowed = o.schulze_extra_blank in
            let r = Methods.Schulze.compute ~nchoices ~blank_allowed ballots in
            let contents = schulze_content q r in
            (div ~a:[ a_class [ "schulze_result" ] ] contents, false)
        | `STV o ->
            let nseats = o.stv_extra_seats in
            let r = Methods.Stv.compute ~nseats ballots in
            let contents = stv_content q r in
            (div ~a:[ a_class [ "stv_result" ] ] contents, false)
      in
      let others =
        if show_others then
          div
            [
              txt (s_ "It is up to you to apply your favorite counting method.");
            ]
        else txt ""
      in
      li
        ~a:[ a_class [ "result_question_item" ] ]
        [
          div ~a:[ a_class [ "result_question" ] ] [ markup q.q_question ];
          applied_counting_method;
          div
            [
              txt (s_ "The raw results can be viewed in the ");
              a
                ~href:(encode_data_uri ~mime_type:"application/json" r)
                (s_ "JSON result");
              txt ". ";
              txt
                (s_
                   "It contains all submitted ballots in clear, in random \
                    order.");
              others;
            ];
        ]
  | Lists.Q x ->
      let open Lists.Syntax in
      let r = r |> result_of_string in
      let answers = Array.to_list x.q_answers in
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
          div ~a:[ a_class [ "result_question" ] ] [ markup x.q_question ];
          answers;
        ]
  | _ ->
      li
        ~a:[ a_class [ "result_question_item" ] ]
        [ div [ txt @@ s_ "Unsupported question type" ] ]

let make_result_div election t ~result =
  let open (val !Belenios_js.I18n.gettext) in
  let open (val election : Election.ELECTION) in
  let questions = Election.get_questions (Template (witness, template)) in
  let r = election_result_of_string read_result result in
  let nballots = t.sized_num_tallied in
  let total_weight = t.sized_total_weight in
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
  let raw_result = encode_data_uri ~mime_type:"application/json" result in
  div
    [
      ul
        (Array.map2 format_question_result
           (to_generic_result r.result)
           questions
        |> Array.to_list);
      div
        [
          txt @@ s_ "Number of accepted ballots: "; txt (string_of_int nballots);
        ];
      div_total_weight;
      div
        [
          txt @@ s_ "You can also download the ";
          a ~href:raw_result (s_ "raw result");
          txt ".";
        ];
    ]

type page = {
  title : string;
  contents : Html_types.div_content_fun elt list;
  footer : Html_types.div_content_fun elt list;
}

let error x = { title = "Error"; contents = [ txt x ]; footer = [] }

let page configuration ?credential uuid =
  let open (val !Belenios_js.I18n.gettext) in
  let@ status cont =
    let* x = Api.(get (election_status uuid) `Nobody) in
    match x with
    | Error _ -> Lwt.return @@ error "Could not get election status!"
    | Ok (x, _) -> cont x
  in
  let@ election cont =
    let* x = Api.(get (election uuid) `Nobody) in
    match x with
    | Error _ -> Lwt.return @@ error "Could not get election parameters!"
    | Ok (x, _) -> cont @@ Election.of_string (module Random) x
  in
  let module W = (val election) in
  let@ dates cont =
    let* x = Api.(get (election_auto_dates uuid) `Nobody) in
    match x with
    | Error _ -> Lwt.return @@ error "Could not get automatic dates!"
    | Ok (x, _) -> cont x
  in
  let now = (new%js Js.date_now)##valueOf in
  let state =
    match status.status_state with
    | `Draft -> assert false
    | `Closed ->
        let it_will_open =
          match dates.auto_date_open with
          | Some t when now < t ->
              span
                [
                  txt " ";
                  txt @@ s_ "It will open in ";
                  txt @@ format_period (t -. now);
                  txt ".";
                ]
          | _ -> txt ""
        in
        [ b [ txt @@ s_ "This election is currently closed." ]; it_will_open ]
    | `Open ->
        let it_will_close =
          match dates.auto_date_close with
          | Some t when now < t ->
              span
                [
                  txt @@ s_ "The election will close in ";
                  txt @@ format_period (t -. now);
                  txt ".";
                ]
          | _ -> txt ""
        in
        [ it_will_close ]
    | `Shuffling ->
        [ b [ txt @@ s_ "The election is closed and being tallied." ] ]
    | `EncryptedTally ->
        [ b [ txt @@ s_ "The election is closed and being tallied." ] ]
    | `Tallied -> [ b [ txt @@ s_ "This election has been tallied." ] ]
    | `Archived -> [ b [ txt @@ s_ "This election is archived." ] ]
  in
  let go_to_the_booth () =
    let disabled =
      match status.status_state with `Open -> [] | _ -> [ a_disabled () ]
    in
    let button =
      let uri = configuration.uris.home ^ "static/frontend/booth/vote.html" in
      let handler _ =
        let params =
          match credential with None -> [] | Some c -> [ ("credential", c) ]
        in
        let params =
          ("uuid", Uuid.unwrap uuid) :: ("lang", lang) :: params
          |> Url.encode_arguments
        in
        let href = Printf.sprintf "%s#%s" uri params in
        Dom_html.window##.location##.href := Js.string href;
        false
      in
      let a =
        a_id "start" :: a_onclick handler
        :: a_class [ "nice-button"; "nice-button--blue" ]
        :: a_style "font-size:35px;" :: disabled
      in
      Tyxml_js.Html.button ~a [ txt @@ s_ "Start" ]
    in
    div ~a:[ a_style "text-align:center;" ] [ div [ button ] ]
  in
  let* middle =
    let fail () =
      Lwt.return @@ div [ txt @@ s_ "Could not get election data!" ]
    in
    let ( let& ) x f =
      let* x = x in
      match x with Error _ -> fail () | Ok (x, _) -> f x
    in
    let& roots = Api.(get (election_roots uuid) `Nobody) in
    match roots.roots_result with
    | None -> Lwt.return @@ go_to_the_booth ()
    | Some result -> (
        match roots.roots_encrypted_tally with
        | None -> fail ()
        | Some t ->
            let& result = Api.(get (election_object uuid result) `Nobody) in
            let& t = Api.(get (election_object uuid t) `Nobody) in
            let t = sized_encrypted_tally_of_string read_hash t in
            Lwt.return @@ make_result_div election t ~result)
  in
  let ballots_link =
    let href = !!(Printf.sprintf "elections/%s/ballots" (Uuid.unwrap uuid)) in
    p
      ~a:[ a_style "text-align:center;" ]
      [
        a ~href
          ~a:
            [
              a_style "font-size:25px;";
              a_class [ "nice-button"; "nice-button--grey" ];
            ]
          (s_ "See accepted ballots");
      ]
  in
  let* audit_div =
    let* x = Api.(get (election_audit_cache uuid) `Nobody) in
    match x with
    | Error _ ->
        Lwt.return @@ div [ txt @@ s_ "Could not retrieve audit data!" ]
    | Ok (audit_cache, _) -> Lwt.return @@ make_audit_div W.template audit_cache
  in
  let contents =
    [
      div ~a:[ a_class [ "clear" ] ] [];
      p state;
      br ();
      middle;
      br ();
      ballots_link;
      br ();
      audit_div;
    ]
  in
  let footer = [ make_audit_footer election ] in
  Lwt.return { title = W.template.t_name; contents; footer }

module App (U : UI) = struct
  let component = "voter"

  let router configuration path =
    let open (val !Belenios_js.I18n.gettext) in
    U.set_title @@ s_ "Election home";
    match path with
    | [ "" ] -> Lwt.return []
    | uuid :: credential ->
        let credential = match credential with [ c ] -> Some c | _ -> None in
        let* p = page configuration ?credential (Uuid.wrap uuid) in
        U.set_title p.title;
        U.set_footer p.footer;
        Lwt.return p.contents
    | _ -> Lwt.return [ div [ txt @@ s_ "Error" ] ]
end

module _ = Make (App) ()
