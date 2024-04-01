(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Lwt
open Lwt.Syntax
open Belenios
open Web_serializable_j
open Web_common
open Core
open Eliom_content.Html.F
open Eliom_content.Html.F.Form

module Make
    (Web_state : Web_state_sig.S)
    (Web_i18n : Web_i18n_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S) =
struct
  open Web_services
  open Pages_common

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"
  let file uuid x = Eliom_service.preapply ~service:election_dir (uuid, x)

  let audit_footer election =
    let open (val election : Site_common_sig.ELECTION) in
    let* l = get_preferred_gettext () in
    let open (val l) in
    return
    @@ div
         ~a:[ a_style "line-height:1.5em;" ]
         [
           div
             [
               div
                 [ txt (s_ "Election fingerprint: "); code [ txt fingerprint ] ];
               div
                 [
                   txt (s_ "Audit data: ");
                   a ~service:(file uuid ESRaw) [ txt (s_ "parameters") ] ();
                   txt ", ";
                   a
                     ~service:(file uuid (ESArchive uuid))
                     [ txt (s_ "public data") ]
                     ();
                   txt ". ";
                   a
                     ~service:
                       (Eliom_service.preapply ~service:election_cast uuid)
                     [ txt (s_ "Advanced mode") ]
                     ();
                   txt ".";
                 ];
             ];
         ]

  let majority_judgment_content l q r =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
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
              | [] -> failwith "anomaly in Pages_voter.majority_judgment"
              | [ x ] -> markup x
              | l ->
                  div
                    [
                      txt (s_ "Tie:");
                      ul (List.map (fun x -> li [ markup x ]) l);
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
      "data:application/json," ^ string_of_mj_ballots r.mj_invalid
    in
    let invalid =
      direct_a invalid
        (Printf.sprintf (f_ "%d invalid ballot(s)") (Array.length r.mj_invalid))
    in
    let invalid = div [ invalid ] in
    [
      div
        [
          txt (s_ "According to Majority Judgment, the ranking is:");
          ol ~a:[ a_class [ "majority_judgment_ranking" ] ] pretty_winners;
        ];
      valid;
      blank;
      invalid;
    ]

  let schulze_content l q r =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
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
              | [] -> failwith "anomaly in Web_templates.schulze"
              | [ x ] -> markup x
              | l ->
                  div
                    [
                      txt (s_ "Tie:");
                      ul (List.map (fun x -> li [ markup x ]) l);
                    ]);
            ])
        explicit_winners
    in
    let explanation =
      div
        ~a:[ a_class [ "schulze_explanation" ] ]
        [
          txt
            (s_
               "A Condorcet winner is a candidate that is preferred over all \
                the other candidates.");
          txt " ";
          txt
            (s_
               "Several techniques exist to decide which candidate to elect \
                when there is no Condorcet winner.");
          txt " ";
          txt (s_ "We use here the Schulze method and we refer voters to ");
          direct_a
            "https://en.wikipedia.org/wiki/Condorcet_method#Schulze_method"
            (s_ "the Wikipedia page");
          txt (s_ " for more information.");
        ]
    in
    [
      explanation;
      txt (s_ "The Schulze winners are:");
      ol pretty_winners;
      valid;
      blank;
    ]

  let stv_content l q r =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    let winners =
      let open Belenios_question.Non_homomorphic.Syntax in
      r.stv_winners
      |> List.map (fun i -> q.q_answers.(i))
      |> List.map (fun l -> li [ txt l ])
    in
    let invalid =
      ( ( r.stv_invalid |> string_of_mj_ballots |> fun x ->
          "data:application/json," ^ x )
      |> fun x ->
        direct_a x
          (Printf.sprintf
             (f_ "%d invalid ballot(s)")
             (Array.length r.stv_invalid)) )
      |> fun x ->
      div
        [
          x;
          txt ". ";
          txt
            (s_
               "A ballot is invalid if two candidates have been given the same \
                preference order or if a rank is missing.");
        ]
    in
    let events =
      ( r.stv_events |> string_of_stv_events |> fun x ->
        "data:application/json," ^ x )
      |> fun x -> direct_a x (s_ "Raw events")
    in
    let tie =
      if
        List.exists
          (function `TieWin _ | `TieLose _ -> true | _ -> false)
          r.stv_events
      then
        div
          [
            txt (s_ "There has been at least one tie.");
            txt " ";
            txt
              (s_
                 "Many variants of STV exist, depending for example on how to \
                  break ties.");
            txt " ";
            txt
              (s_
                 "In our implementation, when several candidates have the same \
                  number of votes when they are ready to be elected or \
                  eliminated, we follow the order in which candidates were \
                  listed in the election.");
            txt " ";
            txt
              (s_
                 "Such candidates are marked as \"TieWin\" when they are \
                  elected and as \"TieLose\" if they have lost.");
            txt " ";
            txt (s_ "Look at the raw events for more details.");
          ]
      else txt ""
    in
    [
      div [ txt (s_ "The Single Transferable Vote winners are:"); ul winners ];
      tie;
      div [ events ];
      div [ invalid ];
    ]

  let format_question_result uuid l i r question =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    let open Belenios_question in
    match question.value with
    | Homomorphic.Q x ->
        let open Homomorphic.Syntax in
        let r = r |> result_of_string in
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
        let ballots = r |> result_of_string in
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
              let contents = majority_judgment_content l q mj in
              (div ~a:[ a_class [ "majority_judgment_result" ] ] contents, false)
          | `Schulze o ->
              let nchoices = Array.length q.q_answers in
              let blank_allowed = o.schulze_extra_blank in
              let r =
                Methods.Schulze.compute ~nchoices ~blank_allowed ballots
              in
              let contents = schulze_content l q r in
              (div ~a:[ a_class [ "schulze_result" ] ] contents, false)
          | `STV o ->
              let nseats = o.stv_extra_seats in
              let r = Methods.Stv.compute ~nseats ballots in
              let contents = stv_content l q r in
              (div ~a:[ a_class [ "stv_result" ] ] contents, false)
        in
        let others =
          if show_others then
            div
              [
                txt
                  (s_ "It is up to you to apply your favorite counting method.");
                txt " ";
                txt (s_ "Available methods on this server:");
                txt " ";
                a ~service:method_schulze [ txt "Condorcet-Schulze" ] (uuid, i);
                txt ", ";
                a ~service:method_mj
                  [ txt (s_ "Majority Judgment") ]
                  (uuid, (i, None));
                txt ", ";
                a ~service:method_stv
                  [ txt (s_ "Single Transferable Vote") ]
                  (uuid, (i, None));
                txt ".";
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
                a ~service:election_project_result
                  [ txt (s_ "JSON result") ]
                  ((uuid, ()), i);
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

  let election_home s election state () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let module W = (val election : Site_common_sig.ELECTION) in
    let questions =
      Belenios.Election.get_questions (Template (W.witness, W.template))
    in
    let uuid = W.uuid in
    let* metadata = Web_persist.get_election_metadata s uuid in
    let* dates = Web_persist.get_election_dates s uuid in
    let now = Datetime.now () in
    let state_ =
      match state with
      | `Closed ->
          let it_will_open =
            match dates.e_auto_open with
            | Some t when Datetime.compare now t < 0 ->
                span
                  [
                    txt " ";
                    txt (s_ "It will open in ");
                    txt (format_period l (Period.sub t now));
                    txt ".";
                  ]
            | _ -> txt ""
          in
          [
            txt " ";
            b [ txt (s_ "This election is currently closed.") ];
            it_will_open;
          ]
      | `Open ->
          let it_will_close =
            match dates.e_auto_close with
            | Some t when Datetime.compare now t < 0 ->
                span
                  [
                    txt (s_ "The election will close in ");
                    txt (format_period l (Period.sub t now));
                    txt ".";
                  ]
            | _ -> txt ""
          in
          [ it_will_close ]
      | `Shuffling ->
          [
            txt " "; b [ txt (s_ "The election is closed and being tallied.") ];
          ]
      | `EncryptedTally ->
          [
            txt " "; b [ txt (s_ "The election is closed and being tallied.") ];
          ]
      | `Tallied ->
          [ txt " "; b [ txt (s_ "This election has been tallied.") ] ]
      | `Archived -> [ txt " "; b [ txt (s_ "This election is archived.") ] ]
    in
    let ballots_link =
      p
        ~a:[ a_style "text-align:center;" ]
        [
          a
            ~a:
              [
                a_style "font-size:25px;";
                a_class [ "nice-button"; "nice-button--grey" ];
              ]
            ~service:election_pretty_ballots
            [ txt (s_ "See accepted ballots") ]
            (uuid, ());
        ]
    in
    let* footer = audit_footer election in
    let go_to_the_booth =
      let disabled = match state with `Open -> [] | _ -> [ a_disabled () ] in
      let button =
        match get_booth_index metadata.e_booth_version with
        | Some i ->
            let (Booth election_vote) = fst booths.(i) in
            let uri =
              Eliom_uri.make_string_uri ~service:(election_vote ()) ()
            in
            let a =
              a_id "start"
              :: a_class [ "nice-button"; "nice-button--blue" ]
              :: a_user_data "uri" uri
              :: a_user_data "uuid" (Uuid.unwrap uuid)
              :: a_user_data "lang" lang :: a_style "font-size:35px;"
              :: disabled
            in
            Eliom_content.Html.F.button ~a [ txt (s_ "Start") ]
        | None -> span [ txt @@ s_ "Unsupported booth version" ]
      in
      div ~a:[ a_style "text-align:center;" ] [ div [ button ] ]
    in
    let* middle =
      let* result = Public_archive.get_result s uuid in
      let result =
        Option.map (election_result_of_string W.read_result) result
      in
      let* hidden = Web_persist.get_election_result_hidden s uuid in
      let* is_admin =
        let* metadata = Web_persist.get_election_metadata s uuid in
        let* site_user = Eliom_reference.get Web_state.site_user in
        match site_user with
        | Some (_, a, _) -> return @@ Accounts.check a metadata.e_owners
        | _ -> return_false
      in
      match result with
      | Some r when hidden = None || is_admin ->
          let* nballots, total_weight =
            let* x = Public_archive.get_sized_encrypted_tally s uuid in
            match x with
            | None -> assert false
            | Some x ->
                let x = sized_encrypted_tally_of_string read_hash x in
                Lwt.return (x.sized_num_tallied, x.sized_total_weight)
          in
          let div_total_weight =
            if not Weight.(is_int total_weight nballots) then
              div
                [
                  txt (s_ "Total weight of accepted ballots:");
                  txt " ";
                  txt (Weight.to_string total_weight);
                ]
            else txt ""
          in
          return
          @@ div
               [
                 ul
                   (Array.mapi2
                      (format_question_result uuid l)
                      (W.to_generic_result r.result)
                      questions
                   |> Array.to_list);
                 div
                   [
                     txt (s_ "Number of accepted ballots: ");
                     txt (string_of_int nballots);
                   ];
                 div_total_weight;
                 div
                   [
                     txt (s_ "You can also download the ");
                     a ~service:election_dir
                       [ txt (s_ "raw result") ]
                       (uuid, ESResult);
                     txt ".";
                   ];
               ]
      | Some _ ->
          let t =
            match hidden with
            | Some t -> t
            | None -> failwith "Impossible case in election_admin"
          in
          return
          @@ div
               [
                 Printf.ksprintf txt
                   (f_
                      "The result of this election is currently not publicly \
                       available. It will be in %s.")
                   (format_period l (Period.sub t now));
               ]
      | None -> return go_to_the_booth
    in
    let* scd = Eliom_reference.get Web_state.show_cookie_disclaimer in
    let cookie_disclaimer =
      if scd then
        div
          ~a:[ a_class [ "cookie-disclaimer" ] ]
          [
            txt (s_ "By using this site, you accept our ");
            direct_a !Web_config.gdpr_uri (s_ "personal data policy");
            txt ". ";
            a ~service:set_cookie_disclaimer
              ~a:[ a_class [ "nice-button"; "nice-button--default" ] ]
              [ txt (s_ "Accept") ]
              (default_admin (ContSiteElection uuid));
          ]
      else txt ""
    in
    let* cache = Web_persist.get_audit_cache s uuid in
    let audit_admin =
      [
        tr
          [
            td [ txt (s_ "Administrator") ];
            td [ txt (Option.value W.template.t_administrator ~default:"N/A") ];
          ];
      ]
    in
    let audit_voters =
      [
        tr
          ~a:[ a_id "voters" ]
          [
            td [ txt (s_ "Number of voters") ];
            td [ Printf.ksprintf txt "%d" cache.cache_checksums.ec_num_voters ];
          ];
        tr
          [
            td [ txt (s_ "Voter list fingerprint") ];
            td
              [
                code
                  [
                    Printf.ksprintf txt "%s"
                      (Hash.to_b64 cache.cache_voters_hash);
                  ];
              ];
          ];
      ]
    in
    let audit_voter_weight =
      match cache.cache_checksums.ec_weights with
      | Some { w_total; w_min; w_max } ->
          [
            tr
              [
                td [ txt (s_ "Voters' total weight") ];
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
                      (s_
                         "All of the following trustees (verification keys) \
                          are needed to decrypt the result:");
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
                   code
                     [
                       Printf.ksprintf txt "(%s) "
                         (Hash.to_b64 x.ttc_verification_key);
                     ];
                   code
                     [ Printf.ksprintf txt "[%s]" (Hash.to_b64 x.ttc_pki_key) ];
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
                       "%d of the following %d trustees (verification keys) \
                        [public keys] are needed to decrypt the election \
                        result:")
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
            td [ txt (s_ "Credentials authority") ];
            td
              [
                txt
                  (Option.value W.template.t_credential_authority ~default:"N/A");
              ];
          ];
        tr
          ~a:[ a_id "credentials" ]
          [
            td [ txt (s_ "Credentials fingerprint") ];
            td [ code [ txt (Hash.to_b64 checksums.ec_public_credentials) ] ];
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
                      (s_
                         "Trustees shuffled the ballots in the following order:");
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
                td [ txt (s_ "Fingerprint of the encrypted tally") ];
                td [ code [ txt (Hash.to_b64 x) ] ];
              ];
          ]
    in
    let div_audit =
      div
        ~a:[ a_class [ "home-audit" ] ]
        [
          div
            ~a:[ a_class [ "home-audit__title" ] ]
            [ txt (s_ "About this election") ];
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
    in
    let content =
      [
        p state_;
        br ();
        middle;
        br ();
        ballots_link;
        br ();
        div_audit;
        script ~a:[ a_src (static "home.js") ] (txt "");
      ]
    in
    let* lang_box = lang_box (ContSiteElection uuid) in
    let title = W.template.t_name in
    let full_title = title in
    let sticky_footer = cookie_disclaimer in
    base ~lang_box ~full_title ~title ~content ~footer ~uuid ~sticky_footer ()

  let cast_raw election () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let module W = (val election : Site_common_sig.ELECTION) in
    let uuid = W.uuid in
    let form_rawballot =
      post_form ~service:election_submit_ballot
        (fun name ->
          [
            div
              [
                txt
                  "Please paste your encrypted ballot in JSON format in the \
                   following box:";
              ];
            div [ textarea ~a:[ a_rows 10; a_cols 40 ] ~name () ];
            div [ input ~input_type:`Submit ~value:"Submit" string ];
          ])
        ()
    in
    let form_upload =
      post_form ~service:election_submit_ballot_file
        (fun name ->
          [
            div
              [
                txt
                  "Alternatively, you can also upload a file containing your \
                   ballot:";
              ];
            div [ txt "File: "; file_input ~name () ];
            div [ input ~input_type:`Submit ~value:"Submit" string ];
          ])
        ()
    in
    let booths =
      let hash =
        Netencoding.Url.mk_url_encoded_parameters
          [ ("uuid", Uuid.unwrap uuid); ("lang", lang) ]
      in
      let make ~service =
        Eliom_uri.make_string_uri ~service ~absolute:true () |> rewrite_prefix
        |> fun uri -> direct_a (uri ^ "#" ^ hash) "direct link"
      in
      Web_services.booths |> Array.to_list
      |> List.map (fun (Booth service, name) ->
             let service = service () in
             li [ a ~service [ txt name ] (); txt " ("; make ~service; txt ")" ])
    in
    let intro =
      div
        [
          div
            [
              txt
                "You can create an encrypted ballot by using the command-line \
                 tool ";
              txt "(available in the ";
              a ~service:source_code [ txt "sources" ] ();
              txt "), or any compatible booth.";
              txt " ";
              txt
                "A specification of encrypted ballots is also available in the \
                 sources.";
            ];
          div [ txt "Booths available on this server:"; ul booths ];
          div
            [
              a ~service:Web_services.election_home
                [ txt "Back to election home" ]
                (uuid, ());
            ];
        ]
    in
    let content =
      [
        intro;
        h3 [ txt "Submit by copy/paste" ];
        form_rawballot;
        h3 [ txt "Submit by file" ];
        form_upload;
      ]
    in
    let* footer = audit_footer election in
    let title = W.template.t_name in
    let full_title = title in
    base ~full_title ~title ~content ~uuid ~footer ()

  let progress_responsive_step5 l =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    div
      ~a:[ a_class [ "progress" ]; a_style "padding-top: 0;" ]
      [
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:[ a_class [ "progress__step__dot-container"; "line-right" ] ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Input credential") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Input credential");
                ]
              [ txt (s_ "Step 1") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      "progress__step__dot-container"; "line-left"; "line-right";
                    ];
                ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Answer to questions") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Answer to questions");
                ]
              [ txt (s_ "Step 2") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      "progress__step__dot-container"; "line-left"; "line-right";
                    ];
                ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Review and encrypt") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Review and encrypt");
                ]
              [ txt (s_ "Step 3") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--done" ] ]
          [
            div
              ~a:
                [
                  a_class
                    [
                      "progress__step__dot-container"; "line-left"; "line-right";
                    ];
                ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Authenticate") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Authenticate");
                ]
              [ txt (s_ "Step 4") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
        div
          ~a:[ a_class [ "progress__step"; "progress__step--current" ] ]
          [
            div
              ~a:[ a_class [ "progress__step__dot-container"; "line-left" ] ]
              [ div ~a:[ a_class [ "progress__step__dot" ] ] [ txt "" ] ];
            span
              ~a:[ a_class [ "progress__step__title" ] ]
              [ txt (s_ "Confirm") ];
            span
              ~a:
                [
                  a_class [ "progress__step__short-title" ];
                  a_title (s_ "Confirm");
                ]
              [ txt (s_ "Step 5") ];
          ];
        div ~a:[ a_class [ "progress__step-separator" ] ] [];
      ]

  let lost_ballot s election () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let title = template.t_name in
    let full_title = title in
    let* metadata = Web_persist.get_election_metadata s uuid in
    let you_must_restart =
      match get_booth_index metadata.e_booth_version with
      | Some i ->
          let (Booth service) = fst Web_services.booths.(i) in
          let hash =
            Netencoding.Url.mk_url_encoded_parameters
              [ ("uuid", Uuid.unwrap uuid) ]
          in
          div
            [
              txt (s_ "If you want to vote, you must ");
              make_a_with_hash ~service:(service ()) ~hash
                (s_ "start from the beginning");
              txt ".";
            ]
      | None -> txt ""
    in
    let content =
      [
        div
          [
            b [ txt (s_ "Warning:") ];
            txt " ";
            txt (s_ "Your vote was not recorded!");
          ];
        you_must_restart;
        div
          [
            a ~service:Web_services.election_home
              [ txt (s_ "Go back to election") ]
              (uuid, ());
          ];
      ]
    in
    base ~full_title ~title ~content ~uuid ()

  let cast_confirmed election ~result () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let name = template.t_name in
    let result, snippet, step_title =
      match result with
      | Ok (user, hash, revote, weight, email) ->
          let this_is_a_revote =
            if revote then span [ txt @@ s_ "This is a revote."; txt " " ]
            else txt ""
          in
          let your_weight_is =
            if not Weight.(is_int weight 1) then
              span
                [
                  txt
                    (Printf.sprintf (f_ "Your weight is %s.")
                       (Weight.to_string weight));
                  txt " ";
                ]
            else txt ""
          in
          ( [
              txt (s_ " as user ");
              em [ txt user.user_name ];
              txt (s_ " has been accepted.");
              txt " ";
              this_is_a_revote;
              your_weight_is;
              txt (s_ "Your smart ballot tracker is ");
              b ~a:[ a_id "ballot_tracker" ] [ txt hash ];
              txt ". ";
              txt (s_ "You can check its presence in the ");
              a ~service:election_pretty_ballots
                [ txt (s_ "ballot box") ]
                (uuid, ());
              txt (s_ " anytime during the election.");
              txt
                (if email then s_ " A confirmation e-mail has been sent to you."
                 else "");
            ],
            read_snippet ~lang !Web_config.success_snippet,
            s_ "Thank you for voting!" )
      | Error e ->
          ( [
              txt (s_ " is rejected, because ");
              txt (Web_common.explain_error l e);
              txt ".";
            ],
            Lwt.return (txt ""),
            s_ "FAIL!" )
    in
    let* snippet = snippet in
    let content =
      [
        progress_responsive_step5 l;
        div ~a:[ a_class [ "current_step" ] ] [ txt step_title ];
        p ([ txt (s_ "Your ballot for "); em [ markup name ] ] @ result);
        snippet;
        p
          [
            a ~service:Web_services.election_home
              [ txt (s_ "Go back to election") ]
              (uuid, ());
          ];
      ]
    in
    let title = name in
    let full_title = name in
    base ~full_title ~title ~content ~uuid ()

  let pretty_ballots s election =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let* hashes = Public_archive.get_ballot_hashes s uuid in
    let* audit_cache = Web_persist.get_audit_cache s uuid in
    let show_weights = audit_cache.cache_checksums.ec_weights <> None in
    let title = template.t_name ^ " — " ^ s_ "Accepted ballots" in
    let nballots = ref 0 in
    let hashes = List.sort (fun (a, _) (b, _) -> compare_b64 a b) hashes in
    let ballots =
      List.map
        (fun (h, w) ->
          incr nballots;
          li
            [
              a ~service:election_pretty_ballot [ txt h ] ((uuid, ()), h);
              (if show_weights then
                 Printf.ksprintf txt " (%s)" (Weight.to_string w)
               else txt "");
            ])
        hashes
    in
    let links =
      p
        [
          a ~service:Web_services.election_home
            [ txt (s_ "Go back to election") ]
            (uuid, ());
        ]
    in
    let* number =
      let n = !nballots in
      let* x = Public_archive.get_sized_encrypted_tally s uuid in
      let x = Option.map (sized_encrypted_tally_of_string read_hash) x in
      match x with
      | None ->
          div
            [
              txt (string_of_int n);
              txt (s_ " ballot(s) have been accepted so far.");
            ]
          |> Lwt.return
      | Some x when x.sized_num_tallied = n ->
          div
            [ txt (string_of_int n); txt (s_ " ballot(s) have been accepted.") ]
          |> Lwt.return
      | Some x ->
          (* should not happen *)
          div
            [
              txt (string_of_int n);
              txt (s_ " ballot(s) have been accepted, and ");
              txt (string_of_int x.sized_num_tallied);
              txt (s_ " have been tallied.");
            ]
          |> Lwt.return
    in
    let content = [ number; ul ballots; links ] in
    base ~title ~content ~uuid ()

  let schulze q r =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Condorcet-Schulze method" in
    let content = schulze_content l q r in
    base ~title ~content ()

  let majority_judgment_select uuid question =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Majority Judgment method" in
    let form =
      get_form ~service:method_mj (fun (uuidn, (questionn, ngradesn)) ->
          [
            input ~input_type:`Hidden ~name:uuidn ~value:uuid (user Uuid.unwrap);
            input ~input_type:`Hidden ~name:questionn ~value:question int;
            txt (s_ "Number of grades:");
            txt " ";
            input ~input_type:`Text ~name:ngradesn int;
            input ~input_type:`Submit ~value:(s_ "Continue") string;
          ])
    in
    let explanation =
      div
        [
          txt
            (s_
               "In the context of Majority Judgment, a vote gives a grade to \
                each candidate.");
          txt " ";
          txt (s_ "1 is the highest grade, 2 is the second highest grade, etc.");
          txt " ";
          txt
            (s_ "As a convenience, 0 is always interpreted as the lowest grade.");
          txt " ";
          txt
            (s_
               "The winner is the candidate with the highest median (or the \
                2nd highest median if there is a tie, etc.).");
          txt " ";
          txt (s_ "More information can be found ");
          direct_a "https://en.wikipedia.org/wiki/Majority_judgment" (s_ "here");
          txt ".";
        ]
    in
    let explanation_grades =
      div
        [
          txt
            (s_
               "The number of different grades (Excellent, Very Good, etc.) \
                typically varies from 5 to 7.");
          txt " ";
          txt
            (s_
               "Please provide the number of grades to see the result of the \
                election according to the Majority Judgment method.");
        ]
    in
    let content = [ explanation; br (); explanation_grades; form ] in
    base ~title ~content ()

  let majority_judgment q r =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Majority Judgment method" in
    let content = majority_judgment_content l q r in
    base ~title ~content ()

  let stv_select uuid question =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Single Transferable Vote method" in
    let form =
      get_form ~service:method_stv (fun (uuidn, (questionn, nseatsn)) ->
          [
            input ~input_type:`Hidden ~name:uuidn ~value:uuid (user Uuid.unwrap);
            input ~input_type:`Hidden ~name:questionn ~value:question int;
            txt (s_ "Number of seats:");
            txt " ";
            input ~input_type:`Text ~name:nseatsn int;
            input ~input_type:`Submit ~value:(s_ "Continue") string;
          ])
    in
    let explanation =
      div
        [
          txt
            (s_
               "In the context of STV, voters rank candidates by order of \
                preference.");
          txt " ";
          txt
            (s_
               "When a candidate obtains enough votes to be elected, the votes \
                are transferred to the next candidate in the voter ballot, \
                with a coefficient proportional to the \"surplus\" of votes.");
          txt " ";
          txt (s_ "More information can be found ");
          direct_a "https://en.wikipedia.org/wiki/Single_transferable_vote"
            (s_ "here");
          txt ". ";
          txt (s_ "Many variants of STV exist, we documented our choices in ");
          direct_a
            "https://gitlab.inria.fr/belenios/belenios/-/blob/master/src/lib/stv.ml"
            (s_ "our code of STV");
          txt ".";
        ]
    in
    let explanation_nseats =
      div
        [
          txt
            (s_
               "Please provide the number of seats to see the result of the \
                election according to the Single Transferable Vote method.");
        ]
    in
    let content = [ explanation; br (); explanation_nseats; form ] in
    base ~title ~content ()

  let stv q r =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Single Transferable Vote method" in
    let content = stv_content l q r in
    base ~title ~content ()
end
