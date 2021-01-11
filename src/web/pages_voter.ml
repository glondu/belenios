(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Belenios
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common
open Web_services
open Eliom_content.Html.F
open Eliom_content.Html.F.Form
open Pages_common

let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

let file uuid x = Eliom_service.preapply ~service:election_dir (uuid, x)

let audit_footer election =
  let uuid = election.e_params.e_uuid in
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  return @@ div ~a:[a_style "line-height:1.5em;"] [
    div [
      div [
        txt (s_ "Election fingerprint: ");
        code [ txt election.e_fingerprint ];
      ];
      div [
        txt (s_ "Audit data: ");
        a ~service:(file uuid ESRaw) [
          txt (s_ "parameters")
        ] ();
        txt ", ";
        a ~service:(file uuid ESTrustees) [txt (s_ "trustees")] ();
        txt ", ";
        a ~service:(file uuid ESCreds) [
          txt (s_ "public credentials")
        ] ();
        txt ", ";
        a ~service:(file uuid ESBallots) [
          txt (s_ "ballots")
        ] ();
        txt ".";
      ];
    ]
  ]

let format_question_result uuid l (i, q) r =
  let open (val l : Web_i18n_sig.GETTEXT) in
  match q with
  | Question.Homomorphic x ->
     let r = Shape.to_array r in
     let open Question_h_t in
     let answers = Array.to_list x.q_answers in
     let answers = match x.q_blank with
       | Some true -> s_ "Blank vote" :: answers
       | _ -> answers
     in
     let answers =
       List.mapi (fun j x ->
           tr [td [txt x]; td [txt @@ string_of_int r.(j)]]
         ) answers
     in
     let answers =
       match answers with
       | [] -> txt ""
       | y :: ys ->
          match x.q_blank with
          | Some true -> table (ys @ [y])
          | _ -> table (y :: ys)
     in
     li [
         div ~a:[a_class ["result_question"]] [txt x.q_question];
         answers;
       ]
  | Question.NonHomomorphic x ->
     let open Question_nh_t in
     li [
         div ~a:[a_class ["result_question"]] [txt x.q_question];
         div [
             txt (s_ "The raw results can be viewed in the ");
             a ~service:election_project_result [txt (s_ "JSON result")] ((uuid, ()), i);
             txt (s_ ". It contains all submitted ballots in clear, in random order. It is up to you to apply your favorite counting method (e.g. Condorcet, STV, majority judgement).");
             txt " ";
             txt (s_ "Available methods on this server:");
             txt " ";
             a ~service:method_schulze [txt "Condorcet-Schulze"] (uuid, i);
             txt ", ";
             a ~service:method_mj [txt (s_ "Majority Judgment")] (uuid, (i, None));
             txt ", ";
             a ~service:method_stv [txt (s_ "Single Transferable Vote")] (uuid, (i, None));
             txt ".";
           ];
       ]

let election_home election state () =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let%lwt dates = Web_persist.get_election_dates uuid in
  let now = now () in
  let state_ =
    match state with
    | `Closed ->
       let it_will_open =
         match dates.e_auto_open with
         | Some t when datetime_compare now t < 0 ->
            span [
                txt " ";
                txt (s_ "It will open in ");
                txt (format_period l (datetime_sub t now));
                txt ".";
              ]
         | _ -> txt ""
       in
      [
        txt " ";
        b [txt (s_ "This election is currently closed.")];
        it_will_open;
      ]
    | `Open ->
       let it_will_close =
         match dates.e_auto_close with
         | Some t when datetime_compare now t < 0 ->
            span [
                txt (s_ "The election will close in ");
                txt (format_period l (datetime_sub t now));
                txt ".";
              ]
         | _ -> txt ""
       in
       [it_will_close]
    | `Shuffling ->
       [
         txt " ";
         b [txt (s_ "The election is closed and being tallied.")];
       ]
    | `EncryptedTally _ ->
       [
         txt " ";
         b [txt (s_ "The election is closed and being tallied.")];
       ]
    | `Tallied ->
       [
         txt " ";
         b [txt (s_ "This election has been tallied.")];
       ]
    | `Archived ->
       [
         txt " ";
         b [txt (s_ "This election is archived.")];
       ]
  in
  let ballots_link =
    p ~a:[a_style "text-align:center;"] [
        a
          ~a:[a_style "font-size:25px;"]
          ~service:election_pretty_ballots [
            txt (s_ "See accepted ballots")
          ] (uuid, ())
      ]
  in
  let%lwt footer = audit_footer election in
  let go_to_the_booth =
    let disabled = match state with
      | `Open -> false
      | _ -> true
    in
    div ~a:[a_style "text-align:center;"] [
      div [
          let hash = Netencoding.Url.mk_url_encoded_parameters ["uuid", raw_string_of_uuid uuid; "lang", lang] in
          make_button ~service:election_vote ~hash ~style:"font-size:35px;" ~disabled (s_ "Start");
        ];
      div [
        a
          ~service:(Eliom_service.preapply ~service:election_cast uuid)
          [txt (s_ "Advanced mode")] ();
      ];
    ]
  in
  let%lwt middle =
    let%lwt result = Web_persist.get_election_result uuid in
    let%lwt hidden = Web_persist.get_election_result_hidden uuid in
    let%lwt is_admin =
      let%lwt metadata = Web_persist.get_election_metadata uuid in
      let%lwt site_user = Eliom_reference.get Web_state.site_user in
      return (metadata.e_owner = site_user)
    in
    match result with
    | Some r when hidden = None || is_admin ->
       let result = Shape.to_shape_array r.result in
       return @@ div [
         ul (
             Array.map2 (format_question_result uuid l) (Array.mapi (fun i q -> i, q) election.e_params.e_questions) result
             |> Array.to_list
           );
         div [
           txt (s_ "Number of accepted ballots: ");
           txt (string_of_int r.num_tallied);
         ];
         div [
           txt (s_ "You can also download the ");
           a ~service:election_dir
             [txt (s_ "result with cryptographic proofs")]
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
       return @@
         div [
             Printf.ksprintf txt
               (f_ "The result of this election is currently not publicly available. It will be in %s.")
               (format_period l (datetime_sub t now));
           ]
    | None -> return go_to_the_booth
  in
  let%lwt scd = Eliom_reference.get Web_state.show_cookie_disclaimer in
  let cookie_disclaimer =
    if scd then
      div
        ~a:[a_class ["cookie-disclaimer"]; a_style "border-style: solid; border-width: 1px;"]
        [
          txt (s_ "By using this site, you accept our ");
          direct_a !Web_config.gdpr_uri (s_ "personal data policy");
          txt ". ";
          a ~service:set_cookie_disclaimer [txt (s_ "Accept")] (ContSiteElection uuid);
        ]
    else txt ""
  in
  let%lwt cache = Web_persist.get_audit_cache uuid in
  let checksums = cache.cache_checksums in
  let div_admin =
    div [
        Printf.ksprintf txt
          (f_ "This election is administered by %s.")
          (Option.get params.e_administrator "N/A");
      ]
  in
  let div_voters =
    div [
        Printf.ksprintf txt
          (f_ "The voter list has %d voter(s) and fingerprint %s.")
          cache.cache_num_voters cache.cache_voters_hash;
      ]
  in
  let format_tc id xs =
    ul ~a:[a_id id] (
        List.map
          (fun x ->
            let name = Option.get x.tc_name "N/A" in
            li [Printf.ksprintf txt "%s (%s)" name x.tc_checksum]
          ) xs
      )
  in
  let div_trustees_mandatory =
    match checksums.ec_trustees with
    | [] -> txt ""
    | l ->
       div [
           txt (s_ "All of the following trustees (verification keys) are needed to decrypt the result:");
           format_tc "trustees" l;
         ]
  in
  let format_ttc className xs =
    ul ~a:[a_class [className]] (
        List.map
          (fun x ->
            let name = Option.get x.ttc_name "N/A" in
            li [
                Printf.ksprintf txt "%s (%s) [%s]"
                  name x.ttc_verification_key x.ttc_pki_key
              ]
          ) xs
      )
  in
  let divs_trustees_threshold =
    match checksums.ec_trustees_threshold with
    | None -> []
    | Some l ->
       List.map
         (fun x ->
           div [
               Printf.ksprintf txt
                 (f_ "%d of the following %d trustees (verification keys) [public keys] are needed to decrypt the election result:")
                 x.ts_threshold (List.length x.ts_trustees);
               format_ttc "trustees_threshold" x.ts_trustees;
             ]
         ) l
  in
  let div_trustees = div (div_trustees_mandatory :: divs_trustees_threshold) in
  let div_credentials =
    div [
        Printf.ksprintf txt
          (f_ "Credentials were generated and sent by %s and have fingerprint %s.")
          (Option.get params.e_credential_authority "N/A")
          checksums.ec_public_credentials;
      ]
  in
  let div_shuffles =
    match checksums.ec_shuffles with
    | None -> txt ""
    | Some xs ->
       div [
           txt (s_ "Trustees shuffled the ballots in the following order:");
           format_tc "shuffles" xs;
         ]
  in
  let div_tally =
    match checksums.ec_encrypted_tally with
    | None -> txt ""
    | Some x ->
       div [
           Printf.ksprintf txt
             (f_ "The fingerprint of the encrypted tally is %s.")
             x
         ]
  in
  let div_audit =
    div ~a:[a_class ["hybrid_box"]] [
      div_admin;
      div_voters;
      div_trustees;
      div_credentials;
      div_shuffles;
      div_tally;
    ]
  in
  let content = [
    cookie_disclaimer;
    p state_;
    br ();
    middle;
    br ();
    ballots_link;
    br ();
    div_audit;
  ] in
  let%lwt lang_box = lang_box (ContSiteElection uuid) in
  responsive_base ~lang_box ~title:params.e_name ~content ~footer ~uuid ()

let cast_raw election () =
  let params = election.e_params in
  let uuid = params.e_uuid in
  let form_rawballot = post_form ~service:election_submit_ballot
    (fun name ->
      [
        div [txt "Please paste your encrypted ballot in JSON format in the following box:"];
        div [textarea ~a:[a_rows 10; a_cols 40] ~name ()];
        div [input ~input_type:`Submit ~value:"Submit" string];
      ]
    ) ()
  in
  let form_upload = post_form ~service:election_submit_ballot_file
    (fun name ->
      [
        div [txt "Alternatively, you can also upload a file containing your ballot:"];
        div [
          txt "File: ";
          file_input ~name ();
        ];
        div [input ~input_type:`Submit ~value:"Submit" string];
      ]
    ) ()
  in
  let intro = div [
    div [
      txt "You can create an encrypted ballot by using the command-line tool ";
      txt "(available in the ";
      a ~service:source_code [txt "sources"] ();
      txt "), or any booth (you can use the ";
      a ~service:election_vote [txt "booth of this server"] ();
      txt " or any other booth of the same version). A specification of encrypted ballots is also available in the sources.";
    ];
    div [
      a ~service:Web_services.election_home
        [txt "Back to election home"] (uuid, ());
    ];
  ] in
  let content = [
    intro;
    h3 [ txt "Submit by copy/paste" ];
    form_rawballot;
    h3 [ txt "Submit by file" ];
    form_upload;
  ] in
  let%lwt footer = audit_footer election in
  responsive_base ~title:params.e_name ~content ~uuid ~footer ()

let cast_confirmation election hash () =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let%lwt user = Web_state.get_election_user uuid in
  let name = params.e_name in
  let user_div = match user with
    | Some u ->
      post_form ~service:election_cast_confirm (fun () -> [
        p ~a:[a_style "text-align: center; padding: 10px;"] [
          txt (s_ "I am ");
          format_user ~site:false u;
          txt (s_ " and ");
          input
            ~a:[a_style "font-size: 20px; cursor: pointer;"]
            ~input_type:`Submit ~value:(s_ "I cast my vote") string;
          txt ".";
        ]
      ]) uuid
    | None ->
      div [
        txt (s_ "Please log in to confirm your vote.");
      ]
  in
  let%lwt div_revote =
    match user with
    | None -> return @@ txt ""
    | Some u ->
       let%lwt revote = Web_persist.has_voted uuid u in
       if revote then
         return @@ p [b [txt (s_ "Note: You have already voted. Your vote will be replaced.")]]
       else
         return @@ txt ""
  in
  let progress = div ~a:[a_style "text-align:center;margin-bottom:20px;"] [
    txt (s_ "Input credential");
    txt " — ";
    txt (s_ "Answer to questions");
    txt " — ";
    txt (s_ "Review and encrypt");
    txt " — ";
    txt (s_ "Authenticate");
    txt " — ";
    b [txt (s_ "Confirm")];
    txt " — ";
    txt (s_ "Done");
    hr ();
  ] in
  let content = [
    progress;
    div ~a:[a_class ["current_step"]] [
        txt (s_ "Step 5/6: Confirm");
    ];
    p [
      txt (s_ "Your ballot for ");
      em [txt name];
      txt (s_ " has been received, but not recorded yet. ");
      txt (s_ "Your smart ballot tracker is ");
      b ~a:[a_id "ballot_tracker"] [
        txt hash
      ];
      txt ".";
      br ();
    ];
    br ();
    p [txt (s_ "Note: your ballot is encrypted and nobody can see its contents.")];
    div_revote;
    user_div;
    p [
      (let service =
        Eliom_service.preapply
          ~service:Web_services.election_home (uuid, ())
      in
      a ~service [
        txt (s_ "Go back to election")
      ] ());
      txt ".";
    ];
  ] in
  responsive_base ~title:name ~content ~uuid ()

let lost_ballot election () =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let title = election.e_params.e_name in
  let uuid = election.e_params.e_uuid in
  let service = Web_services.election_vote in
  let hash = Netencoding.Url.mk_url_encoded_parameters ["uuid", raw_string_of_uuid uuid] in
  let content =
    [
      div [
          b [txt (s_ "Warning:")];
          txt " ";
          txt (s_ "Your vote was not recorded!");
        ];
      div [
          txt (s_ "If you want to vote, you must ");
          make_a_with_hash ~service ~hash (s_ "start from the beginning");
          txt ".";
        ];
      div [
          a ~service:Web_services.election_home [
              txt (s_ "Go back to election")
            ] (uuid, ());
        ];
    ]
  in
  responsive_base ~title ~content ~uuid ()

let cast_confirmed election ~result () =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let name = params.e_name in
  let progress = div ~a:[a_style "text-align:center;margin-bottom:20px;"] [
    txt (s_ "Input credential");
    txt " — ";
    txt (s_ "Answer to questions");
    txt " — ";
    txt (s_ "Review and encrypt");
    txt " — ";
    txt (s_ "Authenticate");
    txt " — ";
    txt (s_ "Confirm");
    txt " — ";
    b [txt (s_ "Done")];
    hr ();
  ] in
  let result, step_title =
    match result with
    | Ok hash ->
       [txt (s_ " has been accepted.");
        txt " ";
        txt (s_ "Your smart ballot tracker is ");
        b ~a:[a_id "ballot_tracker"] [
          txt hash
        ];
        txt ". ";
        txt (s_ "You can check its presence in the ");
        a ~service:election_pretty_ballots [txt (s_ "ballot box")] (uuid, ());
        txt (s_ " anytime during the election.");
        txt (s_ " A confirmation e-mail has been sent to you.");
       ], s_ "Thank you for voting!"
    | Error e ->
       [txt (s_ " is rejected, because ");
        txt (Web_common.explain_error l e);
        txt ".";
       ], s_ "FAIL!"
  in
  let content = [
    progress;
    div ~a:[a_class ["current_step"]] [
        txt (s_ "Step 6/6: ");
        txt step_title;
    ];
    p ([
      txt (s_ "Your ballot for ");
      em [txt name];
      ] @ result);
    p
      [a
         ~service:Web_services.election_home
         [txt (s_ "Go back to election")]
         (uuid, ())];
  ] in
  responsive_base ~title:name ~content ~uuid ()

let pretty_ballots election hashes result () =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let title = params.e_name ^ " — " ^ s_ "Accepted ballots" in
  let nballots = ref 0 in
  let hashes = List.sort compare_b64 hashes in
  let ballots =
    List.map
      (fun h ->
       incr nballots;
       li
         [a
            ~service:election_pretty_ballot
            [txt h]
            ((uuid, ()), h)]
      ) hashes
  in
  let links =
    p
      [a
         ~service:Web_services.election_home
         [txt (s_ "Go back to election")]
         (uuid, ())]
  in
  let number = match !nballots, result with
    | n, None ->
       div [
         txt (string_of_int n);
         txt (s_ " ballot(s) have been accepted so far.");
       ]
    | n, Some r when n = r.num_tallied ->
       div [
         txt (string_of_int n);
         txt (s_ " ballot(s) have been accepted.");
       ]
    | n, Some r -> (* should not happen *)
       div [
         txt (string_of_int n);
         txt (s_ " ballot(s) have been accepted, and ");
         txt (string_of_int r.num_tallied);
         txt (s_ " have been tallied.");
       ]
  in
  let content = [
    number;
    ul ballots;
    links;
  ] in
  responsive_base ~title ~content ~uuid ()

let booth () =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let head = head (title (txt (s_ "Belenios Booth"))) [
    link ~rel:[`Stylesheet] ~href:(static "booth.css") ();
    script ~a:[a_src (static "tool_js_booth.js")] (txt "");
  ] in
  let wait_div =
    div ~a:[a_id "wait_div"] [
        txt (s_ "Please wait… ");
        img ~src:(static "encrypting.gif") ~alt:(s_ "Loading…") ();
      ]
  in
  let election_loader =
    div ~a:[a_id "election_loader"; a_style "display:none;"] [
      h1 [txt (s_ "Belenios Booth")];
      br ();
      txt "Load an election on this server by giving its UUID:";
      div [raw_textarea "uuid" ""];
      div [button_no_value ~button_type:`Button ~a:[a_id "load_uuid"] [txt "Load from UUID"]];
      br ();
      txt "Load any election by giving its parameters:";
      div [raw_textarea "election_params" ""];
      div [button_no_value ~button_type:`Button ~a:[a_id "load_params"] [txt "Load parameters"]];
    ]
  in
  let text_choices = raw_textarea "choices" "" in
  let ballot_form =
    post_form ~a:[a_id "ballot_form"] ~service:election_submit_ballot
      (fun encrypted_vote -> [
        div ~a:[a_id "div_ballot"; a_style "display:none;"] [
          txt "Encrypted ballot:";
          div [
            textarea
              ~a:[a_id "ballot"; a_rows 1; a_cols 80; a_readonly ()]
              ~name:encrypted_vote ();
          ];
        ];
        p [
          txt (s_ "Your ballot has been encrypted, ");
          b [txt (s_ "but not cast yet")];
          txt (s_ "!");
        ];
        p [
          txt (s_ "Your smart ballot tracker is ");
          span ~a:[a_id "ballot_tracker"] [];
        ];
        p [
          txt (s_ "Save it to check that it is taken into account later.");
        ];
        br ();
        div ~a:[a_id "div_submit"] [
            input ~input_type:`Submit ~value:(s_ "Continue") ~a:[a_style "font-size:30px;"] string;
          ];
        div ~a:[a_id "div_submit_manually"; a_style "display:none;"] [
            txt "You must submit your ballot manually.";
          ];
        br (); br ();
       ])
      ()
  in
  let main =
    div ~a:[a_id "main"] [
      div ~a:[a_style "text-align:center; margin-bottom:20px;"] [
        span ~a:[a_id "progress1"; a_style "font-weight:bold;"] [txt (s_ "Input credential")];
        txt " — ";
        span ~a:[a_id "progress2"] [txt (s_ "Answer to questions")];
        txt " — ";
        span ~a:[a_id "progress3"] [txt (s_ "Review and encrypt")];
        txt " — ";
        span ~a:[a_id "progress4"] [txt (s_ "Authenticate")];
        txt " — ";
        span ~a:[a_id "progress5"] [txt (s_ "Confirm")];
        txt " — ";
        span ~a:[a_id "progress6"] [txt (s_ "Done")];
        hr ();
      ];
      div ~a:[a_id "intro"; a_style "text-align:center;"] [
        div ~a:[a_class ["current_step"]] [
          txt (s_ "Step 1/6: Input credential");
        ];
        br (); br ();
        p ~a:[a_id "input_code"; a_style "font-size:20px;"] [
          txt (s_ "Input your credential ");
        ];
        br (); br ();
      ];
      div ~a:[a_id "question_div"; a_style "display:none;"] [
        div ~a:[a_class ["current_step"]] [
          txt (s_ "Step 2/6: Answer to questions");
        ];
      ];
      div ~a:[a_id "plaintext_div"; a_style "display:none;"] [
        div ~a:[a_class ["current_step"]] [
          txt (s_ "Step 3/6: Review and encrypt");
        ];
        div ~a:[a_id "pretty_choices"] [];
        div ~a:[a_style "display:none;"] [
          txt "Plaintext raw ballot:";
          div [text_choices];
        ];
        div ~a:[a_style "text-align:center;"] [
          div ~a:[a_id "encrypting_div"] [
            p [txt (s_ "Please wait while your ballot is being encrypted…")];
            img ~src:(static "encrypting.gif") ~alt:(s_ "Encrypting…") ();
          ];
          div ~a:[a_id "ballot_div"; a_style "display:none;"] [ballot_form];
          Unsafe.data ("<button onclick=\"location.reload();\">" ^ s_ "Restart" ^ "</button>");
          br (); br ();
        ];
      ];
    ]
  in
  let booth_div =
    div ~a:[a_id "booth_div"; a_style "display:none;"] [
      div ~a:[a_id "header"] [
        div ~a:[a_style "float: left; padding: 15px;"] [
          img ~alt:(s_ "Election server") ~a:[a_height 70]
            ~src:(static "logo.png") ();
        ];
        div ~a:[a_style "float: right; padding: 15px;"] [
          img ~alt:"" ~a:[a_height 70]
            ~src:(static "placeholder.png") ();
        ];
        div ~a:[a_style "text-align:center; padding: 20px;"] [
          h1 ~a:[a_id "election_name"] [];
          p ~a:[a_id "election_description"] [];
        ];
        div ~a:[a_style "clear: both;"] [];
      ];
      main;
      div ~a:[a_id "footer"] [
        div ~a:[a_id "bottom"] [
          div [
            txt (s_ "Election UUID: ");
            span ~a:[a_id "election_uuid"] [];
          ];
          div [
            txt (s_ "Election fingerprint: ");
            span ~a:[a_id "election_fingerprint"] [];
          ];
        ];
      ];
    ]
  in
  let body = body [
    wait_div;
    election_loader;
    div ~a:[a_id "wrapper"] [
      booth_div;
    ];
  ] in
  return @@ html ~a:[a_dir `Ltr; a_xml_lang lang] head body

let schulze q r =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let title = s_ "Condorcet-Schulze method" in
  let explicit_winners =
    List.map
      (List.map
         (fun i -> q.Question_nh_t.q_answers.(i))
      ) r.schulze_winners
  in
  let pretty_winners =
    List.map
      (fun l ->
        li [match l with
            | [] -> failwith "anomaly in Web_templates.schulze"
            | [x] -> txt x
            | l -> div [
                       txt (s_ "Tie:");
                       ul (List.map (fun x -> li [txt x]) l);
                     ]
          ]
      ) explicit_winners
  in
  let explanation =
    div
      [
        txt (s_ "A Condorcet winner is a candidate that is preferred over all the other candidates.");
        txt " ";
        txt (s_ "Several techniques exist to decide which candidate to elect when there is no Condorcet winner.");
        txt " ";
        txt (s_ "We use here the Schulze method and we refer voters to ");
        direct_a "https://en.wikipedia.org/wiki/Condorcet_method#Schulze_method" (s_ "the Wikipedia page");
        txt (s_ " for more information.");
      ]
  in
  let content =
    [
      explanation;
      txt (s_ "The Schulze winners are:");
      ol pretty_winners;
    ]
  in
  responsive_base ~title ~content ()

let majority_judgment_select uuid question =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let title = s_ "Majority Judgment method" in
  let form =
    get_form ~service:method_mj
      (fun (uuidn, (questionn, ngradesn)) -> [
           input ~input_type:`Hidden ~name:uuidn ~value:uuid (user raw_string_of_uuid);
           input ~input_type:`Hidden ~name:questionn ~value:question int;
           txt (s_ "Number of grades:");
           txt " ";
           input ~input_type:`Text ~name:ngradesn int;
           input ~input_type:`Submit ~value:(s_ "Continue") string;
         ]
      )
  in
  let explanation =
    div
      [
         txt (s_ "In the context of Majority Judgment, a vote gives a grade to each candidate.");
         txt " ";
         txt (s_ "1 is the highest grade, 2 is the second highest grade, etc.");
         txt " ";
         txt (s_ "As a convenience, 0 is always interpreted as the lowest grade.");
         txt " ";
         txt (s_ "The winner is the candidate with the highest median (or the 2nd highest median if there is a tie, etc.).");
         txt " ";
         txt (s_ "More information can be found ");
         direct_a "https://en.wikipedia.org/wiki/Majority_judgment" (s_ "here");
         txt "."
      ]
  in
  let explanation_grades =
    div
      [
        txt (s_ "The number of different grades (Excellent, Very Good, etc.) typically varies from 5 to 7.");
        txt " ";
        txt (s_ "Please provide the number of grades to see the result of the election according to the Majority Judgment method.");
      ]
  in
  let content =
    [
      explanation;
      br ();
      explanation_grades;
      form;
    ]
  in
  responsive_base ~title ~content ()

let majority_judgment q r =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let title = s_ "Majority Judgment method" in
  let explicit_winners =
    List.map
      (List.map
         (fun i -> q.Question_nh_t.q_answers.(i))
      ) r.mj_winners
  in
  let pretty_winners =
    List.map
      (fun l ->
        li [match l with
            | [] -> failwith "anomaly in Pages_voter.majority_judgment"
            | [x] -> txt x
            | l -> div [
                       txt (s_ "Tie:");
                       ul (List.map (fun x -> li [txt x]) l);
                     ]
          ]
      ) explicit_winners
  in
  let invalid = "data:application/json," ^ string_of_mj_ballots r.mj_invalid in
  let invalid = direct_a invalid (Printf.sprintf (f_ "%d invalid ballot(s)") (Array.length r.mj_invalid)) in
  let invalid =
    div
      [
        invalid;
        txt ". ";
        txt (s_ "A ballot is invalid if a voter has entered a number that is greater than the number of grades.");
      ]
  in
  let content =
    [
      div [
          txt (s_ "The Majority Judgment winners are:");
          ol pretty_winners;
        ];
      invalid;
    ]
  in
  base ~title ~content ()

let stv_select uuid question =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let title = s_ "Single Transferable Vote method" in
  let form =
    get_form ~service:method_stv
      (fun (uuidn, (questionn, nseatsn)) -> [
           input ~input_type:`Hidden ~name:uuidn ~value:uuid (user raw_string_of_uuid);
           input ~input_type:`Hidden ~name:questionn ~value:question int;
           txt (s_ "Number of seats:");
           txt " ";
           input ~input_type:`Text ~name:nseatsn int;
           input ~input_type:`Submit ~value:(s_ "Continue") string;
         ]
      )
  in
  let explanation =
    div
      [
        txt (s_ "In the context of STV, voters rank candidates by order of preference.");
        txt " ";
        txt (s_ "When a candidate obtains enough votes to be elected, the votes are transferred to the next candidate in the voter ballot, with a coefficient proportional to the \"surplus\" of votes.");
        txt " ";
        txt (s_ "More information can be found ");
        direct_a "https://en.wikipedia.org/wiki/Single_transferable_vote" (s_ "here");
        txt ". ";
        txt (s_ "Many variants of STV exist, we documented our choices in ");
        direct_a "https://gitlab.inria.fr/belenios/belenios/-/blob/master/src/lib/stv.ml" (s_ "our code of STV");
        txt ".";
      ]
  in
  let explanation_nseats =
    div
      [
        txt (s_ "Please provide the number of seats to see the result of the election according to the Single Transferable Vote method.");
      ]
  in
  let content =
    [
      explanation;
      br ();
      explanation_nseats;
      form;
    ]
  in
  base ~title ~content ()

let stv q r =
  let%lwt l = get_preferred_gettext () in
  let open (val l) in
  let title = s_ "Single Transferable Vote method" in
  let winners =
    r.stv_winners
    |> List.map (fun i -> q.Question_nh_t.q_answers.(i))
    |> List.map (fun l -> li [txt l])
  in
  let invalid =
    r.stv_invalid
    |> string_of_mj_ballots
    |> (fun x -> "data:application/json," ^ x)
    |> (fun x -> direct_a x (Printf.sprintf (f_ "%d invalid ballot(s)") (Array.length r.stv_invalid)))
    |> (fun x ->
      div
        [
          x;
          txt ". ";
          txt (s_ "A ballot is invalid if two candidates have been given the same preference order or if a rank is missing.");
        ]
    )
  in
  let events =
    r.stv_events
    |> string_of_stv_events
    |> (fun x -> "data:application/json," ^ x)
    |> (fun x -> direct_a x (s_ "Raw events"))
  in
  let tie =
    if
      List.exists
        (function
         | `TieWin _ | `TieLose _ -> true
         | _ -> false
        ) r.stv_events
    then (
      div
        [
          txt (s_ "There has been at least one tie.");
          txt " ";
          txt (s_ "Many variants of STV exist, depending for example on how to break ties.");
          txt " ";
          txt (s_ "In our implementation, when several candidates have the same number of votes when they are ready to be elected or eliminated, we follow the order in which candidates were listed in the election.");
          txt " ";
          txt (s_ "Such candidates are marked as \"TieWin\" when they are elected and as \"TieLose\" if they have lost.");
          txt " ";
          txt (s_ "Look at the raw events for more details.");
        ]
    ) else txt ""
  in
  let content =
    [
      div [
          txt (s_ "The Single Transferable Vote winners are:");
          ul winners;
        ];
      tie;
      div [events];
      div [invalid];
    ]
  in
  responsive_base ~title ~content ()

let contact_footer l metadata =
  let open (val l : Web_i18n_sig.GETTEXT) in
  match metadata.e_contact with
  | None -> fun _ -> ()
  | Some x ->
     fun b ->
     let open Mail_formatter in
     add_newline b;
     add_newline b;
     add_sentence b (s_ "To get more information, please contact:");
     add_newline b;
     add_string b "  ";
     add_string b x

let mail_password l title login password url metadata =
  let open (val l : Web_i18n_sig.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "You are listed as a voter for the election"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b title; add_newline b;
  add_newline b;
  add_sentence b (s_ "You will find below your login and password.");
  add_sentence b (s_ "To cast a vote, you will also need a credential, sent in a separate email.");
  add_sentence b (s_ "Be careful, passwords and credentials look similar but play different roles.");
  add_sentence b (s_ "You will be asked to enter your credential before entering the voting booth.");
  add_sentence b (s_ "Login and passwords are required once your ballot is ready to be cast.");
  add_newline b;
  add_newline b;
  add_string b (s_ "Username:"); add_string b " "; add_string b login; add_newline b;
  add_string b (s_ "Password:"); add_string b " "; add_string b password; add_newline b;
  add_string b (s_ "Page of the election:"); add_string b " "; add_string b url; add_newline b;
  add_newline b;
  add_sentence b (s_ "Note that you are allowed to vote several times.");
  add_sentence b (s_ "Only the last vote counts.");
  contact_footer l metadata b;
  contents b

open Belenios_platform.Platform

let generate_password metadata langs title uuid url id =
  let recipient, login = split_identity id in
  let%lwt salt = generate_token () in
  let%lwt password = generate_token () in
  let hashed = sha256_hex (salt ^ password) in
  let%lwt bodies = Lwt_list.map_s (fun lang ->
    let%lwt l = Web_i18n.get_lang_gettext "voter" lang in
    return (mail_password l title login password url metadata)
  ) langs in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \nBelenios" in
  let%lwt subject =
    let%lwt l = Web_i18n.get_lang_gettext "voter" (List.hd langs) in
    let open (val l) in
    Printf.kprintf return (f_ "Your password for election %s") title
  in
  let%lwt () = send_email (MailPassword uuid) ~recipient ~subject ~body in
  return (salt, hashed)

let mail_credential l title cas ~login cred url metadata =
  let open (val l : Web_i18n_sig.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (s_ "You are listed as a voter for the election"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b title; add_newline b;
  add_newline b;
  add_sentence b (s_ "You will find below your credential.");
  if not cas then (
    add_sentence b (s_ "To cast a vote, you will also need a password, sent in a separate email.");
    add_sentence b (s_ "Be careful, passwords and credentials look similar but play different roles.");
    add_sentence b (s_ "You will be asked to enter your credential before entering the voting booth.");
    add_sentence b (s_ "Login and passwords are required once your ballot is ready to be cast.");
  );
  add_newline b;
  add_newline b;
  add_string b (s_ "Username:"); add_string b " "; add_string b login; add_newline b;
  add_string b (s_ "Credential:"); add_string b " "; add_string b cred; add_newline b;
  add_string b (s_ "Page of the election:"); add_string b " "; add_string b url; add_newline b;
  add_newline b;
  add_sentence b (s_ "Note that you are allowed to vote several times.");
  add_sentence b (s_ "Only the last vote counts.");
  contact_footer l metadata b;
  contents b

let generate_mail_credential langs title cas ~login cred url metadata =
  let%lwt bodies =
    Lwt_list.map_s
      (fun lang ->
        let%lwt l = Web_i18n.get_lang_gettext "voter" lang in
        return (mail_credential l title cas ~login cred url metadata)
      ) langs
  in
  let body = String.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \nBelenios" in
  let%lwt subject =
    let%lwt l = Web_i18n.get_lang_gettext "voter" (List.hd langs) in
    let open (val l) in
    Printf.ksprintf return (f_ "Your credential for election %s") title
  in
  return (subject, body)

let mail_confirmation l user title hash revote url1 url2 metadata =
  let open (val l : Web_i18n_sig.GETTEXT) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") user); add_newline b;
  add_newline b;
  add_sentence b (s_ "Your vote for election"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b title; add_newline b;
  add_newline b;
  add_sentence b (s_ "has been recorded.");
  add_sentence b (s_ "Your smart ballot tracker is"); add_newline b;
  add_newline b;
  add_string b "  "; add_string b hash; add_newline b;
  if revote then (
     add_newline b;
     add_sentence b (s_ "This vote replaces any previous vote.");
     add_newline b;
  );
  add_newline b;
  add_sentence b (s_ "You can check its presence in the ballot box, accessible at");
  add_newline b;
  add_string b "  "; add_string b url1; add_newline b;
  add_newline b;
  add_sentence b (s_ "Results will be published on the election page");
  add_newline b;
  add_string b "  "; add_string b url2;
  contact_footer l metadata b;
  add_newline b;
  add_newline b;
  add_string b "-- "; add_newline b;
  add_string b "Belenios";
  contents b
