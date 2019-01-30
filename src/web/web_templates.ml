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

open Lwt
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

(* TODO: these pages should be redesigned *)

let site_title = "Election Server"
let admin_background = " background: #FF9999;"

let unsafe_a uri text =
  Printf.ksprintf Unsafe.data "<a href=\"%s\">%s</a>" uri text

let static x =
  let service = Eliom_service.static_dir () in
  make_uri ~service ["static"; x]

let format_user ~site u =
  em [pcdata (if site then string_of_user u else u.user_name)]

let login_box ?cont () =
  let style = "float: right; text-align: right;" ^ admin_background in
  let%lwt user = Eliom_reference.get Web_state.site_user in
  let auth_systems = List.map (fun x -> x.auth_instance) !Web_config.site_auth_config in
  let cont = match cont with
    | None -> ContSiteHome
    | Some x -> x
  in
  let login service = Eliom_service.preapply site_login (Some service, cont) in
  let logout () = Eliom_service.preapply logout cont in
  let body =
    match user with
    | Some user ->
      [
        div [
          pcdata "Logged in as ";
          format_user ~site:true user;
          pcdata ".";
        ];
        div [
          a ~a:[a_id "logout"] ~service:(logout ()) [pcdata "Log out"] ();
          pcdata ".";
        ];
      ]
    | None ->
      [
        div [
          pcdata "Not logged in.";
        ];
        let auth_systems =
          List.map (fun name ->
              a ~a:[a_id ("login_" ^ name)]
                ~service:(login name) [pcdata name] ()
            ) auth_systems |> List.join (pcdata ", ")
        in
        div (
          [pcdata "Log in: ["] @ auth_systems @ [pcdata "]"]
        );
      ]
  in
  return (div ~a:[a_style style] body)

let belenios_url = Eliom_service.extern
  ~prefix:"http://www.belenios.org"
  ~path:[]
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let base ~title ?login_box ~content ?(footer = div []) ?uuid () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let administer =
    match uuid with
    | None ->
       a ~service:admin [pcdata L.administer_elections] ()
    | Some uuid ->
       a ~service:election_admin [pcdata L.administer_this_election] uuid
  in
  let login_box = match login_box with
    | None ->
       div ~a:[a_style "float: right; padding: 10px;"] [
         img ~a:[a_height 70] ~alt:""
           ~src:(static "placeholder.png") ();
       ]
    | Some x -> x
  in
  let%lwt warning = match !Web_config.warning_file with
    | None -> return @@ pcdata ""
    | Some f -> match%lwt read_file f with
                | None -> return @@ pcdata ""
                | Some x -> return @@ Unsafe.data (String.concat "\n" x)
  in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang L.lang]
    (head (Eliom_content.Html.F.title (pcdata title)) [
      script (pcdata "window.onbeforeunload = function () {};");
      link ~rel:[`Stylesheet] ~href:(static "site.css") ();
    ])
    (body [
      div ~a:[a_id "wrapper"] [
      div ~a:[a_id "header"] [
        div [
          div ~a:[a_style "float: left; padding: 10px;"] [
            a ~service:home [
              img ~alt:L.election_server ~a:[a_height 70]
                ~src:(static "logo.png") ();
            ] ();
          ];
          login_box;
          h1 ~a:[a_style "text-align: center; padding: 20px;"] [pcdata title];
          div ~a:[a_style "clear: both;"] [];
        ];
      ];
      warning;
      div ~a:[a_id "main"] content;
      div ~a:[a_id "footer"; a_style "text-align: center;" ] [
        div ~a:[a_id "bottom"] [
          footer;
          pcdata L.powered_by;
          a ~service:belenios_url [pcdata "Belenios"] ();
          Belenios_version.(
            Printf.ksprintf pcdata " %s (%s). " version build
          );
          a ~service:source_code [pcdata L.get_the_source_code] ();
          pcdata ". ";
          unsafe_a !Web_config.gdpr_uri "Privacy policy";
          pcdata ". ";
          administer;
          pcdata ".";
        ]
      ]]
     ]))

let privacy_notice cont =
  let title = site_title ^ " — Personal data processing notice" in
  let service = Eliom_service.preapply privacy_notice_accept cont in
  let content =
    [
      div [
          pcdata "To use this site, you must accept our ";
          unsafe_a !Web_config.gdpr_uri "personal data policy";
          pcdata ".";
        ];
      post_form ~service
        (fun () ->
          [
            div [
                input ~input_type:`Submit ~value:"Accept" string;
              ];
          ]
        ) ();
    ]
  in
  base ~title ~content ()

let format_election (uuid, name) =
  li [
    a ~service:election_admin [pcdata name] uuid;
  ]

let format_draft_election (uuid, name) =
  li [
    a ~service:election_draft [pcdata name] uuid;
  ]

let admin ~elections () =
  let title = site_title ^ " — Administration" in
  match elections with
  | None ->
     let contact = match !Web_config.contact_uri with
       | None -> pcdata ""
       | Some uri ->
          div [
              pcdata "If you do not have any account, you may ";
              unsafe_a uri "contact us";
              pcdata ".";
            ]
     in
     let content = [
       div [
         pcdata "To administer an election, you need to log in using one";
         pcdata " of the authentication methods available in the upper";
         pcdata " right corner of this page.";
         contact;
       ]
     ] in
     let%lwt login_box = login_box ~cont:ContSiteAdmin () in
     base ~title ~login_box ~content ()
  | Some (draft, elections, tallied, archived) ->
    let draft =
      match draft with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@ List.map format_draft_election draft
    in
    let elections =
      match elections with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@ List.map format_election elections
    in
    let tallied =
      match tallied with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@ List.map format_election tallied
    in
    let archived =
      match archived with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@ List.map format_election archived
    in
    let content = [
      div [
        div [
          a ~service:election_draft_pre [
            pcdata "Prepare a new election";
          ] ();
        ];
        div [br ()];
        h2 [pcdata "Elections being prepared"];
        draft;
        div [br ()];
        h2 [pcdata "Elections you can administer"];
        elections;
        div [br ()];
        h2 [pcdata "Tallied elections"];
        tallied;
        div [br ()];
        h2 [pcdata "Archived elections"];
        archived;
      ];
    ] in
    let%lwt login_box = login_box () in
    base ~title ~login_box ~content ()

let make_button ~service ?hash ~disabled contents =
  let uri = Eliom_uri.make_string_uri ~service () in
  let uri = match hash with
    | None -> uri
    | Some x -> uri ^ "#" ^ x
  in
  Printf.ksprintf Unsafe.data (* FIXME: unsafe *)
    "<button onclick=\"location.href='%s';\" style=\"font-size:35px;\"%s>%s</button>"
    uri (if disabled then " disabled" else "")
    contents

let a_mailto ~dest ~subject ~body contents =
  let uri = Printf.sprintf "mailto:%s?subject=%s&amp;body=%s" dest
    (Netencoding.Url.encode ~plus:false subject)
    (Netencoding.Url.encode ~plus:false body)
  in
  Printf.ksprintf Unsafe.data "<a href=\"%s\">%s</a>"
    uri contents

let new_election_failure reason () =
  let title = "Create new election" in
  let reason =
    match reason with
    | `Exists -> pcdata "An election with the same UUID already exists."
    | `Exception e -> pcdata @@ Printexc.to_string e
  in
  let content = [
    div [
      p [pcdata "The creation failed."];
      p [reason];
    ]
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let generic_page ~title ?service message () =
  let proceed = match service with
    | None -> pcdata ""
    | Some service ->
       div [
         a ~service [pcdata "Proceed"] ();
       ]
  in
  let content = [
    p [pcdata message];
    proceed;
  ] in
  base ~title ~content ()

let election_draft_pre () =
  let title = "Prepare a new election" in
  let cred_info = Eliom_service.extern
    ~prefix:"http://www.belenios.org"
    ~path:["setup.php"]
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()
  in
  let form =
    post_form ~service:election_draft_new
      (fun (credmgmt, (auth, cas_server)) ->
        [
          fieldset
            ~legend:(legend [
              pcdata "Credential management (";
              a ~service:cred_info [pcdata "more info"] ();
              pcdata ")";
            ])
            [
              div [
                radio ~checked:true ~name:credmgmt ~value:"auto" string;
                pcdata " Automatic (degraded mode - credentials will be handled by the server)";
              ];
              div [
                radio ~name:credmgmt ~value:"manual" string;
                pcdata " Manual (safe mode - a third party will handle the credentials)";
              ];
            ];
          fieldset
            ~legend:(legend [pcdata "Authentication"])
            [
              div [
                radio ~checked:true ~name:auth ~value:"password" string;
                pcdata " Password (passwords will be emailed to voters)";
              ];
              div [
                radio ~name:auth ~value:"cas" string;
                pcdata " CAS (external authentication server), server address: ";
                input ~input_type:`Text ~name:cas_server string;
                pcdata " (for example: https://cas.inria.fr/cas)";
              ];
            ];
          div [
            input ~input_type:`Submit ~value:"Proceed" string;
          ];
        ]
      ) ()
  in
  let content = [
    form
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let preview_booth uuid =
  let url =
    Eliom_uri.make_string_uri
      ~service:election_draft_preview ~absolute:true (uuid, ()) |>
      rewrite_prefix |>
      (fun x -> Filename.chop_suffix x "election.json")
  in
  let hash = Netencoding.Url.mk_url_encoded_parameters ["url", url] in
  let service =
    Eliom_uri.make_string_uri
      ~service:election_vote ~absolute:true () |> rewrite_prefix
  in
  span [
      unsafe_a (service ^ "#" ^ hash) "Preview booth";
      pcdata " (you can use any credential such as HsqB3C3y62Ekq4D)."
    ]

let election_draft uuid se () =
  let title = "Preparation of election " ^ se.se_questions.t_name in
  let form_languages =
    post_form ~service:election_draft_languages
      (fun languages ->
        [
          div [
              pcdata "Languages: ";
              input ~name:languages ~input_type:`Text
                ~value:(string_of_languages se.se_metadata.e_languages) string;
              pcdata " (Available languages: ";
              pcdata (string_of_languages (Some available_languages));
              pcdata ")";
            ];
          div [
              pcdata "This is a space-separated list of languages that will be used in emails sent by the server.";
            ];
          div [
              input ~input_type:`Submit ~value:"Save changes" string;
            ];
        ]) uuid
  in
  let div_languages =
    div [
        h2 [pcdata "Languages"];
        form_languages;
      ]
  in
  let form_description =
    post_form ~service:election_draft_description
      (fun (name, description) ->
        [
          div [
            pcdata "Name of the election: ";
            input ~name:name
              ~input_type:`Text ~value:se.se_questions.t_name string;
          ];
          div [
            div [pcdata "Description of the election: "];
            div [
              textarea ~name:description ~a:[a_cols 80]
                ~value:se.se_questions.t_description ();
            ];
          ];
          div [
            input ~input_type:`Submit ~value:"Save changes" string;
          ];
        ]
      ) uuid
  in
  let div_description =
    div [
      h2 [pcdata "Name and description of the election"];
      form_description;
    ]
  in
  let form_contact =
    post_form ~service:election_draft_contact
      (fun contact ->
        [
          div [
              pcdata "Contact: ";
              let value =
                match se.se_metadata.e_contact with
                | Some x -> x
                | None -> default_contact
              in
              input ~name:contact ~input_type:`Text ~value string;
            ];
          div [
              pcdata "This contact will be added to emails sent to the voters.";
            ];
          div [
              input ~input_type:`Submit ~value:"Save changes" string;
            ];
        ]) uuid
  in
  let div_contact =
    div [
        h2 [pcdata "Contact"];
        form_contact;
      ]
  in
  let has_credentials = match se.se_metadata.e_cred_authority with
    | None -> false
    | Some _ -> true
  in
  let auth = match se.se_metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] -> `Password
    | Some [{auth_system = "dummy"; _}] -> `Dummy
    | Some [{auth_system = "cas"; auth_config = ["server", server]; _}] -> `CAS server
    | _ -> failwith "unknown authentication scheme in election_draft"
  in
  let div_auth =
    div [
      h2 [pcdata "Authentication"];
      match auth with
      | `Password ->
         div [
           pcdata "Authentication scheme: password ";
           post_form ~service:election_draft_auth_genpwd
             (fun () ->
               [input ~input_type:`Submit ~value:"Generate and mail missing passwords" string]
             ) uuid;
         ]
      | `Dummy ->
         div [
           pcdata "Authentication scheme: dummy"
         ]
      | `CAS server ->
         div [
           pcdata "Authentication scheme: CAS with server ";
           pcdata server;
         ]
    ]
  in
  let div_questions =
    div [
      h2 [
        a ~a:[a_id "edit_questions"] ~service:election_draft_questions
          [pcdata "Edit questions"]
          uuid;
      ];
      preview_booth uuid;
    ]
  in
  let div_voters =
    div [
      h2 [
        a ~a:[a_id "edit_voters"] ~service:election_draft_voters
          [pcdata "Edit voters"]
          uuid
      ];
      div [
        pcdata @@ string_of_int @@ List.length se.se_voters;
        pcdata " voter(s) registered";
      ];
    ]
  in
  let div_trustees =
    div [
      h2 [pcdata "Trustees"];
      div [
          pcdata "By default, the election server manages the keys of the election (degraded privacy mode). ";
          pcdata "For real elections, the key must be shared among independent trustees. Click ";
          a ~service:election_draft_trustees [pcdata "here"] uuid;
          pcdata " to set up the election key.";
        ];
    ]
  in
  let div_credentials =
    div [
      h2 [pcdata "Credentials"];
      if se.se_public_creds_received then (
        div [
          pcdata "Credentials have already been generated!"
        ]
      ) else (
        div [
          pcdata "Warning: this will freeze the voter list!";
          if has_credentials then (
            post_form ~service:election_draft_credentials_server
              (fun () ->
                [input ~input_type:`Submit ~value:"Generate on server" string]
              ) uuid
          ) else (
            div [
              a ~service:election_draft_credential_authority [pcdata "Credential management"] uuid;
            ]
          );
        ]
      )
    ]
  in
  let link_confirm = div [
    h2 [pcdata "Validate creation"];
    a ~service:election_draft_confirm [pcdata "Create election"] uuid;
  ] in
  let form_destroy =
    let t = Option.get se.se_creation_date default_creation_date in
    let t = datetime_add t (day days_to_delete) in
    post_form
      ~service:election_draft_destroy
      (fun () ->
        [
          div [
              h2 [pcdata "Destroy election"];
              div [
                  pcdata "Note: this election will be automatically destroyed after ";
                  pcdata (format_datetime t);
                  pcdata ".";
                ];
              input ~input_type:`Submit ~value:"Destroy election" string;
            ]
        ]
      ) uuid
  in
  let content = [
    div_description;
    hr ();
    div_languages;
    hr ();
    div_contact;
    hr ();
    div_questions;
    hr ();
    div_voters;
    hr ();
    div_credentials;
    hr ();
    div_auth;
    hr ();
    div_trustees;
    hr ();
    link_confirm;
    hr ();
    form_destroy;
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let mail_trustee_generation : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Dear trustee,

You will find below the link to generate your private decryption key, used to tally the election.

  %s

Here's the instructions:
1. click on the link
2. click on \"generate a new key pair\"
3. your private key will appear in another window or tab. Make sure
   you SAVE IT properly otherwise it will not possible to tally and the
   election will be canceled.
4. in the first window, click on \"submit\" to send the public part of
   your key, used encrypt the votes. For verification purposes, you
   should save this part (that starts with {\"pok\":{\"challenge\":\") ), for
   example sending yourself an email.

Regarding your private key, it is crucial you save it (otherwise the
election will be canceled) and store it securely (if your private key
is known together with the private keys of the other trustees, then
vote privacy is no longer guaranteed). We suggest two options:
1. you may store the key on a USB stick and store it in a safe.
2. Or you may simply print it and store it in a safe.
Of course, more cryptographic solutions are welcome as well.

Thank you for your help,

-- \nThe election administrator."

let election_draft_trustees uuid se () =
  let title = "Trustees for election " ^ se.se_questions.t_name in
  let form_trustees_add =
    post_form
      ~service:election_draft_trustee_add
      (fun name ->
        [
          pcdata "Trustee's e-mail address: ";
          input ~input_type:`Text ~name string;
          input ~input_type:`Submit ~value:"Add" string;
        ]
      ) uuid
  in
  let mk_form_trustee_del value =
    post_form
      ~service:election_draft_trustee_del
      (fun name ->
        [
          input ~input_type:`Hidden ~name ~value int;
          input ~input_type:`Submit ~value:"Remove" string;
        ]) uuid
  in
  let trustees = match se.se_public_keys with
    | [] -> pcdata ""
    | ts ->
       table (
         tr [
           th [pcdata "Trustee"];
           th [pcdata "Mail"];
           th [pcdata "Link"];
           th [pcdata "Done?"];
           th [pcdata "Remove"];
         ] ::
           List.mapi (fun i t ->
             tr [
               td [
                 pcdata t.st_id;
               ];
               td [
                   if t.st_token <> "" then (
                 let uri = rewrite_prefix @@ Eliom_uri.make_string_uri
                   ~absolute:true ~service:election_draft_trustee (uuid, t.st_token)
                 in
                 let body = Printf.sprintf mail_trustee_generation uri in
                 let subject = "Link to generate the decryption key" in
                 a_mailto ~dest:t.st_id ~subject ~body "Mail"
                   ) else (
                     pcdata "(server)"
                   )
               ];
               td [
                   if t.st_token <> "" then (
                   a ~service:election_draft_trustee [pcdata "Link"] (uuid, t.st_token);
                   ) else (
                     pcdata "(server)"
                   )
               ];
               td [
                 pcdata (if t.st_public_key = "" then "No" else "Yes");
               ];
               td [if t.st_id = "server" then pcdata "(cannot be removed)" else mk_form_trustee_del i];
             ]
           ) ts
       )
  in
  let import_link = div [
                        a ~service:Web_services.election_draft_import_trustees
                          [pcdata "Import trustees from another election"] uuid
                      ]
  in
  let div_trustees =
    if se.se_threshold_trustees = None then
      div [
          trustees;
          (if se.se_public_keys <> [] then
             div [
                 pcdata "There is one link per trustee. Send each trustee her link.";
                 br ();
                 br ();
               ]
           else pcdata "");
          form_trustees_add;
        ]
    else pcdata ""
  in
  let div_content =
    div [
      div [
          pcdata "To set up the election key, you need to nominate trustees. Each trustee will create her own secret key. ";
          pcdata "To set up the election so that only a subset of trustees is needed, go to the ";
          a ~service:election_draft_threshold_trustees [pcdata "threshold mode"] uuid;
          pcdata ".";
        ];
      br ();
      div_trustees;
    ]
  in
  let back_link = div [
    a ~service:Web_services.election_draft
      [pcdata "Go back to election draft"] uuid;
  ] in
  let content = [
    div_content;
    import_link;
    back_link;
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let election_draft_threshold_trustees uuid se () =
  let title = "Trustees for election " ^ se.se_questions.t_name in
  let show_add_remove = se.se_threshold = None in
  let form_trustees_add =
    if show_add_remove then
      post_form
        ~service:election_draft_threshold_trustee_add
        (fun name ->
          [
            pcdata "Trustee's e-mail address: ";
            input ~input_type:`Text ~name string;
            input ~input_type:`Submit ~value:"Add" string;
          ]
        ) uuid
    else pcdata ""
  in
  let mk_form_trustee_del value =
    post_form
      ~service:election_draft_threshold_trustee_del
      (fun name ->
        [
          input ~input_type:`Hidden ~name ~value int;
          input ~input_type:`Submit ~value:"Remove" string;
      ]) uuid
  in
  let trustees = match se.se_threshold_trustees with
    | None -> pcdata ""
    | Some ts ->
       div [
       table (
         tr (
           [
             th [pcdata "Trustee"];
             th [pcdata "Mail"];
             th [pcdata "Link"];
             th [pcdata "State"];
           ] @ (if show_add_remove then [th [pcdata "Remove"]] else [])
         ) ::
           List.mapi (fun i t ->
               let state =
                 match t.stt_step with
                 | None -> "init"
                 | Some 1 -> "1a"
                 | Some 2 -> "1b"
                 | Some 3 -> "2a"
                 | Some 4 -> "2b"
                 | Some 5 -> "3a"
                 | Some 6 -> "3b"
                 | Some 7 -> "done"
                 | _ -> "unknown"
               in
             tr (
                 [
                   td [
                       pcdata t.stt_id;
                     ];
                   td [
                       let uri = rewrite_prefix @@
                                   Eliom_uri.make_string_uri
                                     ~absolute:true ~service:election_draft_threshold_trustee (uuid, t.stt_token)
                       in
                       let body = Printf.sprintf mail_trustee_generation uri in
                       let subject = "Link to generate the decryption key" in
                       a_mailto ~dest:t.stt_id ~subject ~body "Mail"
                     ];
                   td [
                       a ~service:election_draft_threshold_trustee [pcdata "Link"] (uuid, t.stt_token);
                     ];
                   td [
                       pcdata state;
                     ];
                 ] @ (if show_add_remove then [td [mk_form_trustee_del i]] else [])
               )
             ) ts
         );
       div [
           pcdata "Meaning of states:";
           ul [
               li [pcdata "init: administrator needs to set threshold"];
               li [pcdata "1a: action needed from trustee: generate private key"];
               li [pcdata "2a, 3a: action needed from trustee: enter private key"];
               li [pcdata "1b, 2b, 3b: waiting for other trustees"];
               li [pcdata "done: the key establishment protocol is finished"];
             ];
         ];
       br ();
       ]
  in
  let form_threshold, form_reset =
    match se.se_threshold_trustees with
    | None -> pcdata "", pcdata ""
    | Some ts ->
       match se.se_threshold with
       | None ->
          post_form ~service:election_draft_threshold_set
            (fun name ->
              [
                pcdata "Threshold: ";
                input ~input_type:`Text ~name int;
                input ~input_type:`Submit ~value:"Set" string;
                pcdata " (the threshold must be smaller than the number of trustees)";
              ]
            ) uuid,
          pcdata ""
       | Some i ->
          div [
              pcdata (string_of_int i);
              pcdata " out of ";
              pcdata (string_of_int (List.length ts));
              pcdata " trustees will be needed to decrypt the result.";
            ],
          post_form ~service:election_draft_threshold_set
            (fun name ->
              [
                input ~input_type:`Hidden ~name ~value:0 int;
                input ~input_type:`Submit ~value:"Reset threshold" string;
              ]
            ) uuid
  in
  let maybe_error =
    match se.se_threshold_error with
    | None -> pcdata ""
    | Some e -> div [b [pcdata "ERROR: "]; pcdata e; br (); br ()]
  in
  let div_content =
    div [
      div [pcdata "On this page, you can configure a group of trustees such that only a subset of them is needed to perform the decryption."];
      br ();
      form_threshold;
      br ();
      trustees;
      (if se.se_threshold_trustees <> None then
          div [
            pcdata "There is one link per trustee. Send each trustee her link.";
            br ();
            br ();
            maybe_error;
          ]
       else pcdata "");
      form_trustees_add;
      form_reset;
    ]
  in
  let back_link = div [
    a ~service:Web_services.election_draft
      [pcdata "Go back to election draft"] uuid;
  ] in
  let content = [
    div_content;
    br ();
    back_link;
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let election_draft_credential_authority uuid se () =
  let title = "Credentials for election " ^ se.se_questions.t_name in
  let content = [
    div [
      pcdata "Please send the credential authority the following link:";
    ];
    ul [
      li [
        a
          ~service:election_draft_credentials
          [
            pcdata @@ rewrite_prefix @@ Eliom_uri.make_string_uri
              ~absolute:true
              ~service:election_draft_credentials
              (uuid, se.se_public_creds)
          ]
          (uuid, se.se_public_creds);
      ];
    ];
    div [
      pcdata "Note that this authority will have to send each credential to each voter herself.";
    ];
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let election_draft_questions uuid se () =
  let title = "Questions for election " ^ se.se_questions.t_name in
  let form =
    let value = string_of_template se.se_questions in
    post_form
      ~service:election_draft_questions_post
      (fun name ->
       [
         div [pcdata "Questions:"];
         div [textarea ~a:[a_id "questions"; a_rows 5; a_cols 80] ~name ~value ()];
         div [input ~input_type:`Submit ~value:"Save changes" string]])
      uuid
  in
  let interactivity =
    div
      ~a:[a_id "interactivity"]
      [
        script ~a:[a_src (static "sjcl.js")] (pcdata "");
        script ~a:[a_src (static "jsbn.js")] (pcdata "");
        script ~a:[a_src (static "jsbn2.js")] (pcdata "");
        script ~a:[a_src (static "random.js")] (pcdata "");
        script ~a:[a_src (static "tool_js_questions.js")] (pcdata "");
      ]
  in
  let preview = div [hr (); preview_booth uuid] in
  let content = [
    interactivity;
    form;
    preview;
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let election_draft_voters uuid se maxvoters () =
  let title = "Voters for election " ^ se.se_questions.t_name in
  let form =
    post_form
      ~service:election_draft_voters_add
      (fun name ->
        [
          div [textarea ~a:[a_rows 20; a_cols 50] ~name ()];
          div [input ~input_type:`Submit ~value:"Add" string]])
      uuid
  in
  let mk_remove_button id =
    post_form
      ~service:election_draft_voters_remove
      (fun name ->
        [
          input ~input_type:`Hidden ~name ~value:id string;
          input ~input_type:`Submit ~value:"Remove" string;
        ]
      ) uuid
  in
  let has_passwords = match se.se_metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] -> true
    | _ -> false
  in
  let mk_regen_passwd value =
    post_form ~service:election_draft_voters_passwd
      ~a:[a_style "display: inline;"]
      (fun name ->
        [
          input ~input_type:`Hidden ~name ~value string;
          input ~input_type:`Submit ~value:"Send again" string;
        ]
      ) uuid
  in
  let format_password_cell x = match x.sv_password with
    | Some _ -> [pcdata "Yes "; mk_regen_passwd x.sv_id]
    | None -> [pcdata "No"]
  in
  let voters =
    List.map (fun v ->
      tr (
        [td [pcdata v.sv_id]] @
        (if has_passwords then [td (format_password_cell v)] else []) @
        (if se.se_public_creds_received then [] else [td [mk_remove_button v.sv_id]])
      )
    ) se.se_voters
  in
  let form_passwords =
    if has_passwords then
      post_form ~service:election_draft_auth_genpwd
        (fun () ->
          [input ~input_type:`Submit ~value:"Generate and mail missing passwords" string]
        ) uuid
    else pcdata ""
  in
  let voters =
    match voters with
    | [] -> div [pcdata "No voters"]
    | _ :: _ ->
       div [
         form_passwords;
         br ();
         table
           (tr (
             [th [pcdata "Identity"]] @
               (if has_passwords then [th [pcdata "Password sent?"]] else []) @
               (if se.se_public_creds_received then [] else [th [pcdata "Remove"]])
            ) :: voters)
       ]
  in
  let back = div [
    a ~service:Web_services.election_draft [pcdata "Return to draft page"] uuid;
  ] in
  let div_add =
    if se.se_public_creds_received then
      pcdata ""
    else
      div [
        div [
            pcdata "Please enter the identities of voters to add, one per line (max ";
            pcdata (string_of_int maxvoters);
            pcdata ").";
            br ();
            b [pcdata "Warning:"];
            pcdata " you have to make sure that these email addresses are valid. You won't be able to change the email addresses once the election is set up. Voters with invalid email addresses won't be able to vote.";
          ];
        form;
        div [
          b [pcdata "Note:"];
          pcdata " An identity is either an e-mail address, or \"address,login\",";
          pcdata " where \"address\" is an e-mail address and \"login\" the";
          pcdata " associated login for authentication.";
        ];
      ]
  in
  let div_import = div [
    a ~service:election_draft_import
      [pcdata "Import voters from another election"]
      uuid
  ] in
  let content = [
    back;
    div_import;
    br ();
    voters;
    div_add;
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let unsafe_textarea ?rows ?cols id contents =
  let rows = match rows with
    | None -> ""
    | Some i -> Printf.sprintf " rows=\"%d\"" i
  in
  let cols = match cols with
    | None -> ""
    | Some i -> Printf.sprintf " cols=\"%d\"" i
  in
  Printf.ksprintf Unsafe.data
    "<textarea id=\"%s\"%s%s>%s</textarea>"
    id rows cols contents

let election_draft_credentials token uuid se () =
  let title = "Credentials for election " ^ se.se_questions.t_name in
  let div_link =
    let url = Eliom_uri.make_string_uri ~absolute:true
                ~service:election_home (uuid, ()) |> rewrite_prefix
    in
    div [
        pcdata "The link to the election will be:";
        ul [li [pcdata url]];
      ]
  in
  let form_textarea =
    post_form ~a:[a_id "submit_form"; a_style "display:none;"]
      ~service:election_draft_credentials_post
      (fun name ->
       [div
          [div [pcdata "Public credentials:"];
           div [textarea ~a:[a_id "pks"; a_rows 5; a_cols 40] ~name ()];
           div ~a:[a_style "display:none;"] [a ~service:home ~a:[a_id "hashed"] [pcdata "Hashed public credentials"] ()];
           div [
               b [pcdata "Instructions:"];
               ol [
                   li [
                       pcdata "Download ";
                       a ~service:home ~a:[a_id "creds"] [pcdata "private credentials"] ();
                       pcdata " and save the file to a secure location.";
                       br ();
                       pcdata "You will use it to send credentials to voters.";
                     ];
                   li [
                       pcdata "Download ";
                       a ~service:home ~a:[a_id "public_creds"] [pcdata "public credentials"] ();
                       pcdata " and save the file.";
                       br ();
                       pcdata "Once the election is open, you must check that";
                       pcdata " the file published by the server matches.";
                     ];
                   li [pcdata "Submit public credentials using the button below."];
                 ];
             ];
           div [input ~input_type:`Submit ~value:"Submit public credentials" string]]])
      (uuid, token)
  in
  let disclaimer =
    p
      [
        b [pcdata "Note:"];
        pcdata " submitting a large (> 200) number of credentials using the above form may fail; in this case, you have to use the command-line tool and the form below.";
      ]
  in
  let form_file =
    post_form
      ~service:election_draft_credentials_post_file
      (fun name ->
       [div
          [h2 [pcdata "Submit by file"];
           div [pcdata "Use this form to upload public credentials generated with the command-line tool."];
           div [file_input ~name ()];
           div [input ~input_type:`Submit ~value:"Submit" string]]])
      (uuid, token)
  in
  let group =
    div
      ~a:[a_style "display:none;"]
      [
        div [pcdata "UUID:"];
        div [unsafe_textarea "uuid" (raw_string_of_uuid uuid)];
        div [pcdata "Group parameters:"];
        div [unsafe_textarea "group" se.se_group];
      ]
  in
  let voters =
    let value = String.concat "\n" (List.map (fun x -> x.sv_id) se.se_voters) in
    div [
      div [pcdata "List of voters:"];
      div [unsafe_textarea ~rows:5 ~cols:40 "voters" value];
    ]
  in
  let interactivity =
    div
      ~a:[a_id "interactivity"]
      [
        script ~a:[a_src (static "sjcl.js")] (pcdata "");
        script ~a:[a_src (static "jsbn.js")] (pcdata "");
        script ~a:[a_src (static "jsbn2.js")] (pcdata "");
        script ~a:[a_src (static "random.js")] (pcdata "");
        script ~a:[a_src (static "tool_js_credgen.js")] (pcdata "");
      ]
  in
  let div_textarea = div [group; voters; interactivity; form_textarea; disclaimer] in
  let content =
    if se.se_public_creds_received then (
      [
        div [pcdata "Credentials have already been generated!"];
      ]
    ) else (
      [
        div_link;
        div_textarea;
        form_file;
      ]
    ) in
  base ~title ~content ()

let election_draft_trustee token uuid se () =
  let title = "Trustee for election " ^ se.se_questions.t_name in
  let div_link =
    let url = Eliom_uri.make_string_uri ~absolute:true
                ~service:election_home (uuid, ()) |> rewrite_prefix
    in
    div [
        pcdata "The link to the election will be:";
        ul [li [pcdata url]];
      ]
  in
  let form =
    let trustee = List.find (fun x -> x.st_token = token) se.se_public_keys in
    let value = trustee.st_public_key in
    let service = Eliom_service.preapply election_draft_trustee_post (uuid, token) in
    post_form
      ~service
      (fun name ->
       [
         div ~a:[a_id "submit_form"; a_style "display:none;"] [
           div [pcdata "Public key:"];
           div [textarea ~a:[a_rows 5; a_cols 40; a_id "pk"] ~name ~value ()];
           div [
               b [pcdata "Instructions:"];
               ol [
                   li [
                       pcdata "Download your ";
                       a ~service:home ~a:[a_id "private_key"] [pcdata "private key"] ();
                       pcdata " and save it to a secure location.";
                       br ();
                       pcdata "You will use it to decrypt the final result.";
                     ];
                   li [
                       pcdata "Download your ";
                       a ~service:home ~a:[a_id "public_key"] [pcdata "public key"] ();
                       pcdata " and save it.";
                       br ();
                       pcdata "Once the election is open, you must check that";
                       pcdata " it is present in the set of public keys";
                       pcdata " published by the server.";
                     ];
                   li [pcdata "Submit your public key using the button below."];
                 ];
             ];
           div [input ~input_type:`Submit ~value:"Submit public key" string];
         ]
       ]
      ) ()
  in
  let group =
    div
      ~a:[a_style "display:none;"]
      [
        div [pcdata "Group parameters:"];
        div [unsafe_textarea "group" se.se_group];
      ]
  in
  let interactivity =
    div
      ~a:[a_id "interactivity"]
      [
        script ~a:[a_src (static "sjcl.js")] (pcdata "");
        script ~a:[a_src (static "jsbn.js")] (pcdata "");
        script ~a:[a_src (static "jsbn2.js")] (pcdata "");
        script ~a:[a_src (static "random.js")] (pcdata "");
        script ~a:[a_src (static "tool_js_tkeygen.js")] (pcdata "");
      ]
  in
  let content = [
    div_link;
    group;
    interactivity;
    form;
  ] in
  base ~title ~content ()

let election_draft_threshold_trustee token uuid se () =
  let title = "Trustee for election " ^ se.se_questions.t_name in
  let header =
    div ~a:[a_style "text-align:center;"] [
        h2 [pcdata "Collaborative key generation"];
        div ~a:[a_id "current_step"] [
            pcdata "Step 0/3"
          ];
      ]
  in
  let div_link =
    let url = Eliom_uri.make_string_uri ~absolute:true
                ~service:election_home (uuid, ()) |> rewrite_prefix
    in
    div [
        pcdata "The link to the election will be:";
        ul [li [pcdata url]];
      ]
  in
  let%lwt trustee =
    match se.se_threshold_trustees with
    | None -> fail_http 404
    | Some ts ->
       match List.find_opt (fun x -> x.stt_token = token) ts with
       | Some x -> return x
       | None -> fail_http 404
  in
  let%lwt certs =
    match se.se_threshold_trustees with
    | None -> fail_http 404
    | Some ts ->
       let certs = List.fold_left (fun accu x ->
           match x.stt_cert with
           | None -> accu
           | Some c -> c :: accu
         ) [] ts |> List.rev |> Array.of_list
       in return {certs}
  in
  let threshold =
    match se.se_threshold with
    | None -> 0
    | Some t -> t
  in
  let inputs =
    div ~a:[a_style "display:none;"] [
        div [
            pcdata "Step: ";
            unsafe_textarea "step" (match trustee.stt_step with None -> "0" | Some x -> string_of_int x);
          ];
        div [
            pcdata "Group parameters: ";
            unsafe_textarea "group" se.se_group;
          ];
        div [
            pcdata "Certificates: ";
            unsafe_textarea "certs" (string_of_certs certs);
          ];
        div [
            pcdata "Threshold: ";
            unsafe_textarea "threshold" (string_of_int threshold);
          ];
        div [
            pcdata "Vinput: ";
            unsafe_textarea "vinput" (match trustee.stt_vinput with None -> "" | Some x -> string_of_vinput x);
          ];
      ]
  in
  let form =
    post_form
      ~service:election_draft_threshold_trustee_post
      ~a:[a_id "data_form"]
      (fun data ->
        [
          div ~a:[a_id "key_helper"; a_style "display:none;"] [
              b [pcdata "Instructions:"];
              ol [
                  li [
                      pcdata "Download your ";
                      a ~service:home ~a:[a_id "private_key"] [pcdata "private key"] ();
                      pcdata " and save it to a secure location."
                    ];
                  li [
                      pcdata "Submit data using the following button: ";
                      input ~input_type:`Submit ~value:"Submit" string;
                      pcdata ".";
                      div [
                          pcdata "Data: ";
                          textarea ~a:[a_id "data"] ~name:data ();
                        ];
                    ];
                ];
            ];
        ]
      ) (uuid, token)
  in
  let form_compute =
    div ~a:[a_id "compute_form"; a_style "display: none;"] [
        b [pcdata "Instructions:"];
        ol [
            li [
                pcdata "Enter your private key: ";
                input ~input_type:`Text ~a:[a_id "compute_private_key"] string;
                pcdata " ";
                button_no_value ~a:[a_id "compute_button"] ~button_type:`Button [
                    pcdata "Proceed";
                  ];
              ];
            li [
                pcdata "Submit data using the following button:";
                post_form
                  ~service:election_draft_threshold_trustee_post
                  (fun data ->
                    [
                      input ~input_type:`Submit ~value:"Submit" string;
                      div [
                          pcdata "Data: ";
                          textarea ~a:[a_id "compute_data"] ~name:data ();
                        ];
                    ]
                  ) (uuid, token);
              ];
          ];
      ]
  in
  let interactivity =
    div
      ~a:[a_id "interactivity"]
      [
        script ~a:[a_src (static "sjcl.js")] (pcdata "");
        script ~a:[a_src (static "jsbn.js")] (pcdata "");
        script ~a:[a_src (static "jsbn2.js")] (pcdata "");
        script ~a:[a_src (static "random.js")] (pcdata "");
        script ~a:[a_src (static "tool_js_ttkeygen.js")] (pcdata "");
      ]
  in
  let content = [
      header;
      div_link;
      br ();
      div ~a:[a_id "explain"] [];
      inputs;
      interactivity;
      form;
      form_compute;
    ]
  in
  base ~title ~content ()

let election_draft_importer ~service ~title uuid (elections, tallied, archived) () =
  let format_election (from_uuid, name) =
    let from_uuid = raw_string_of_uuid from_uuid in
    let form = post_form ~service
      (fun from ->
        [
          div [
              pcdata name;
              pcdata " (";
              pcdata from_uuid;
              pcdata ")"
            ];
          div [
            input
              ~input_type:`Hidden
              ~name:from
              ~value:from_uuid string;
            input ~input_type:`Submit ~value:"Import from this election" string;
          ]
        ]
      ) uuid
    in
    li [form]
  in
  let itemize xs = match xs with
    | [] -> p [pcdata "You own no such elections!"]
    | _ -> ul @@ List.map format_election xs
  in
  let content = [
    h2 [pcdata "Elections you can administer"];
    itemize elections;
    h2 [pcdata "Tallied elections"];
    itemize tallied;
    h2 [pcdata "Archived elections"];
    itemize archived;
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let election_draft_import uuid se elections =
  let title = "Election " ^ se.se_questions.t_name ^ " — Import voters from another election" in
  let service = election_draft_import_post in
  election_draft_importer ~service ~title uuid elections

let election_draft_import_trustees uuid se elections =
  let title = "Election " ^ se.se_questions.t_name ^ " — Import trustees from another election" in
  let service = election_draft_import_trustees_post in
  election_draft_importer ~service ~title uuid elections

let election_draft_confirm uuid se () =
  let notok x = span ~a:[a_style "color: red;"] [pcdata x] in
  let ok x = pcdata x in
  let title = "Election " ^ se.se_questions.t_name ^ " — Validate creation" in
  let ready = true in
  let ready, name =
    if se.se_questions.t_name = default_name then
      false, notok "Not edited"
    else
      ready, ok "OK"
  in
  let ready, description =
    if se.se_questions.t_description = default_description then
      false, notok "Not edited"
    else
      ready, ok "OK"
  in
  let ready, questions =
    if se.se_questions.t_questions = default_questions then
      false, notok "Not edited"
    else
      ready, ok "OK"
  in
  let ready, voters =
    let b = not (se.se_voters = []) in
    ready && b,
    (if b then ok else notok) (Printf.sprintf "%d voter(s)" (List.length se.se_voters))
  in
  let ready, passwords =
    match se.se_metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] ->
       if List.for_all (fun v -> v.sv_password <> None) se.se_voters then ready, ok "OK"
       else false, notok "Missing"
    | _ -> ready, ok "Not applicable"
  in
  let ready, credentials =
    if se.se_public_creds_received then
      ready, ok (if se.se_metadata.e_cred_authority = None then "Received" else "Sent")
    else false, notok "Missing"
  in
  let ready, trustees =
    match se.se_public_keys with
    | [] -> ready, ok "OK"
    | _ :: _ ->
       match se.se_threshold_trustees with
       | None -> if List.for_all (fun {st_public_key; _} ->
                        st_public_key <> ""
                      ) se.se_public_keys then ready, ok "OK" else false, notok "Missing"
       | Some _ ->
          if se.se_threshold_parameters <> None &&
               match se.se_threshold_trustees with
               | None -> false
               | Some ts ->
                  List.for_all (fun {stt_step; _} -> stt_step = Some 7) ts
          then ready, ok "OK"
          else false, notok "Missing"
  in
  let div_trustee_warning =
    match se.se_threshold_trustees, se.se_public_keys with
    | None, [] ->
       div [
           b [pcdata "Warning:"];
           pcdata " No trustees were set. This means that the server will manage the election key by itself.";
         ]
    | _, _ -> pcdata ""
  in
  let contact, div_contact_warning =
    match se.se_metadata.e_contact with
    | None ->
       "No",
       div [
           b [pcdata "Warning:"];
           pcdata " No contact was set!";
         ]
    | Some _ -> "Yes", pcdata ""
  in
  let table_checklist = table [
    tr [
      td [pcdata "Name?"];
      td [name];
    ];
    tr [
      td [pcdata "Description?"];
      td [description];
    ];
    tr [
      td [pcdata "Questions?"];
      td [questions; pcdata " "; preview_booth uuid];
    ];
    tr [
      td [pcdata "Voters?"];
      td [voters];
    ];
    tr [
      td [pcdata "Passwords?"];
      td [passwords];
    ];
    tr [
      td [pcdata "Credentials?"];
      td [credentials];
    ];
    tr [
      td [pcdata "Trustees?"];
      td [trustees];
    ];
    tr [
      td [pcdata "Contact?"];
      td [pcdata contact];
    ];
  ] in
  let status =
    if ready then
      span ~a:[a_style "color: green;"] [pcdata "election ready"]
    else
      span ~a:[a_style "color: red;"] [pcdata "election not ready"]
  in
  let checklist = div [
    h2 [pcdata "Checklist: "; status];
    table_checklist;
    div_trustee_warning;
    div_contact_warning;
  ] in
  let form_create =
    if ready then
      post_form
        ~service:election_draft_create
        (fun () ->
          [div
              [h2 [pcdata "Validate creation"];
               input ~input_type:`Submit ~value:"Create election" string;
               pcdata " (Warning: this action is irreversible.)";
              ]]
        ) uuid
    else div []
  in
  let back = div [
    a ~service:Web_services.election_draft [pcdata "Return to draft page"] uuid;
  ] in
  let content = [
    back;
    checklist;
    form_create;
  ] in
  let%lwt login_box = login_box () in
  base ~title ~login_box ~content ()

let file uuid x = Eliom_service.preapply election_dir (uuid, x)

let audit_footer election =
  let uuid = election.e_params.e_uuid in
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let%lwt pk_or_tp =
    match%lwt Web_persist.get_threshold election.e_params.e_uuid with
    | None ->
       return (a ~service:(file uuid ESKeys) [
                   pcdata L.trustee_public_keys
                 ] ())
    | Some _ ->
       return (a ~service:(file uuid ESTParams) [
                   pcdata "threshold parameters"
                 ] ())
  in
  return @@ div ~a:[a_style "line-height:1.5em;"] [
    div [
      div [
        pcdata L.election_fingerprint;
        code [ pcdata election.e_fingerprint ];
      ];
      div [
        pcdata L.audit_data;
        a ~service:(file uuid ESRaw) [
          pcdata L.parameters
        ] ();
        pcdata ", ";
        pk_or_tp;
        pcdata ", ";
        a ~service:(file uuid ESCreds) [
          pcdata L.public_credentials
        ] ();
        pcdata ", ";
        a ~service:(file uuid ESBallots) [
          pcdata L.ballots
        ] ();
        pcdata ".";
      ];
    ]
  ]

let rec list_concat elt = function
  | x :: ((_ :: _) as xs) -> x :: elt :: (list_concat elt xs)
  | ([_] | []) as xs -> xs

let election_home election state () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let l = Web_i18n.get_lang language in
  let module L = (val l) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let%lwt d = Web_persist.get_election_auto_dates uuid in
  let now = now () in
  let state_ =
    match state with
    | `Closed ->
       let it_will_open =
         match d.Web_persist.auto_open with
         | Some t when datetime_compare now t < 0 ->
            span [
                pcdata " ";
                pcdata L.it_will_open_in;
                pcdata (format_period l (datetime_sub t now));
                pcdata ".";
              ]
         | _ -> pcdata ""
       in
      [
        pcdata " ";
        b [pcdata L.election_currently_closed];
        it_will_open;
      ]
    | `Open ->
       let it_will_close =
         match d.Web_persist.auto_close with
         | Some t when datetime_compare now t < 0 ->
            span [
                pcdata L.the_election_will_close_in;
                pcdata (format_period l (datetime_sub t now));
                pcdata ".";
              ]
         | _ -> pcdata ""
       in
       [it_will_close]
    | `EncryptedTally (_, _, hash) ->
       [
         pcdata " ";
         b [pcdata L.election_closed_being_tallied];
         pcdata L.the;
         a
           ~service:election_dir
           [pcdata L.encrypted_tally]
           (uuid, ESETally);
         pcdata L.hash_is;
         b [pcdata hash];
         pcdata ".";
       ]
    | `Tallied ->
       [
         pcdata " ";
         b [pcdata L.election_has_been_tallied];
       ]
    | `Archived ->
       [
         pcdata " ";
         b [pcdata L.election_archived];
       ]
  in
  let ballots_link =
    p ~a:[a_style "text-align:center;"] [
        a
          ~a:[a_style "font-size:25px;"]
          ~service:election_pretty_ballots [
            pcdata L.see_accepted_ballots
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
          let url =
            Eliom_uri.make_string_uri
              ~service:election_home ~absolute:true (uuid, ()) |>
              rewrite_prefix
          in
          let hash = Netencoding.Url.mk_url_encoded_parameters ["url", url] in
          make_button ~service:election_vote ~hash ~disabled L.start;
        ];
      div [
        a
          ~service:(Eliom_service.preapply election_cast uuid)
          [pcdata L.advanced_mode] ();
      ];
    ]
  in
  let%lwt middle =
    let%lwt result = Web_persist.get_election_result uuid in
    match result with
    | Some r ->
       let result = r.result in
       let questions = Array.to_list election.e_params.e_questions in
       return @@ div [
         ul (List.mapi (fun i x ->
           let answers = Array.to_list x.q_answers in
           let answers = match x.q_blank with
             | Some true -> L.blank_vote :: answers
             | _ -> answers
           in
           let answers = List.mapi (fun j x ->
             tr [td [pcdata x]; td [pcdata @@ string_of_int result.(i).(j)]]
           ) answers in
           let answers =
             match answers with
             | [] -> pcdata ""
             | y :: ys ->
                match x.q_blank with
                | Some true -> table (ys @ [y])
                | _ -> table (y :: ys)
           in
           li [
             pcdata x.q_question;
             answers;
           ]
         ) questions);
         div [
           pcdata L.number_accepted_ballots;
           pcdata (string_of_int r.num_tallied);
         ];
         div [
           pcdata L.you_can_also_download;
           a ~service:election_dir
             [pcdata L.result_with_crypto_proofs]
             (uuid, ESResult);
           pcdata ".";
         ];
       ]
    | None -> return go_to_the_booth
  in
  let languages =
    div ~a:[a_class ["languages"]]
      (list_concat (pcdata " ") @@ List.map (fun lang ->
        a ~service:set_language [pcdata lang] (lang, ContSiteElection uuid)
       ) available_languages)
  in
  let%lwt scd = Eliom_reference.get Web_state.show_cookie_disclaimer in
  let cookie_disclaimer =
    if scd then
      div
        ~a:[a_style "border-style: solid; border-width: 1px;"]
        [
          pcdata L.by_using_you_accept;
          unsafe_a !Web_config.gdpr_uri L.privacy_policy;
          pcdata ". ";
          a ~service:set_cookie_disclaimer [pcdata L.accept] (ContSiteElection uuid);
        ]
    else pcdata ""
  in
  let content = [
    cookie_disclaimer;
    languages;
    p state_;
    br ();
    middle;
    br ();
    ballots_link;
  ] in
  base ~title:params.e_name ~content ~footer ~uuid ()

let mail_trustee_tally : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "Dear trustee,

The election is now closed. Here's the link to proceed to tally:

  %s

Here's the instructions:
1. Follow the link.
2. Enter your private decryption key in the first box and click on
   \"generate decryption factors\"
3. The second box is now filled with crypto material. Please press the
   button \"submit\".

Thank you again for your help,

-- \nThe election administrator."

let election_admin election metadata state get_tokens_decrypt () =
  let uuid = election.e_params.e_uuid in
  let title = election.e_params.e_name ^ " — Administration" in
  let auto_form () =
    let open Web_persist in
    let%lwt auto_dates = get_election_auto_dates uuid in
    let format = function
      | None -> ""
      | Some x -> String.sub (string_of_datetime x) 1 19
    in
    return @@ post_form ~service:election_auto_post
      (fun (lopen, lclose) ->
        [
          div [pcdata "Alternatively, you may setup automatic dates."];
          div [
              b [pcdata "Note:"];
              pcdata " times are in UTC. Now is ";
              pcdata (format (Some (now ())));
              pcdata ".";
            ];
          div ~a:[a_style "margin-left: 3em;"] [
              div [
                  pcdata "Automatically open the election at: ";
                  input ~name:lopen ~input_type:`Text ~value:(format auto_dates.auto_open) string;
                ];
              div [
                  pcdata "Automatically close the election at: ";
                  input ~name:lclose ~input_type:`Text ~value:(format auto_dates.auto_close) string;
                ];
              div [
                  pcdata "Enter dates in UTC, in format YYYY-MM-DD HH:MM:SS, leave empty for no date.";
                ];
            ];
          div [
              input ~input_type:`Submit ~value:"Change automatic dates" string;
            ];
        ]
      ) uuid
  in
  let state_form checked =
    let service, value, msg, msg2 =
      if checked then
        election_close, "Close election",
        "The election is open. Voters can vote. ",
        " You may re-open the election when it is closed."
      else
        election_open, "Open election",
        "The election is closed. No one can vote. ",
        ""
    in
    post_form
      ~service
      (fun () ->
       [
         div ~a:[a_style "text-align: center;"] [
             pcdata msg;
           ];
         br ();
         input ~input_type:`Submit ~value string;
         pcdata msg2;
       ]) uuid
  in
  let%lwt state_div =
    match state with
    | `Open ->
       let%lwt auto_form = auto_form () in
       return @@ div [
         state_form true;
         br ();
         auto_form;
       ]
    | `Closed ->
       let%lwt auto_form = auto_form () in
       return @@ div [
         state_form false;
         br ();
         auto_form;
         br ();
         hr ();
         post_form
           ~service:election_compute_encrypted_tally
           (fun () ->
             [input
                 ~input_type:`Submit
                 ~value:"Proceed to vote counting"
                 string;
              pcdata " Warning: this action is irreversible; the election will be definitively closed.";
             ]) uuid;
       ]
    | `EncryptedTally (npks, _, hash) ->
       let%lwt pds = Web_persist.get_partial_decryptions uuid in
       let%lwt tp = Web_persist.get_threshold uuid in
       let tp =
         match tp with
         | None -> None
         | Some tp -> Some (threshold_parameters_of_string Yojson.Safe.read_json tp)
       in
       let threshold_or_not =
         match tp with
         | None -> pcdata ""
         | Some tp -> pcdata (Printf.sprintf " At least %d trustee(s) must act." tp.t_threshold)
       in
       let trustees =
         let rec loop i ts =
           if i <= npks then
             match ts with
             | t :: ts -> (Some t, i) :: (loop (i+1) ts)
             | [] -> (None, i) :: (loop (i+1) ts)
           else []
         in
         match metadata.e_trustees with
         | None -> loop 1 []
         | Some ts -> loop 1 ts
       in
       let rec seq i j = if i >= j then [] else i :: (seq (i+1) j) in
       let%lwt trustee_tokens =
         match tp with
         | None -> return (List.map string_of_int (seq 1 (npks+1)))
         | Some _ -> get_tokens_decrypt ()
       in
       let trustees = List.combine trustees trustee_tokens in
       let trustees =
         List.map
           (fun ((name, trustee_id), token) ->
             let service = election_tally_trustees in
             let x = (uuid, token) in
             let uri = rewrite_prefix @@ Eliom_uri.make_string_uri
               ~absolute:true ~service x
             in
             let link_content, dest = match name with
               | None -> uri, !Web_config.server_mail
               | Some name -> name, name
             in
             let mail, link =
               if link_content = "server" then (
                 pcdata "(server)",
                 pcdata "(server)"
               ) else (
                 let body = Printf.sprintf mail_trustee_tally uri in
                 let subject = "Link to tally the election" in
                 a_mailto ~dest ~subject ~body "Mail",
                 a ~service [pcdata "Link"] x
               )
             in
             tr [
               td [pcdata link_content];
               td [mail];
               td [link];
               td [
                 pcdata (if List.mem_assoc trustee_id pds then "Yes" else "No")
               ];
             ]
           ) trustees
       in
       let release_form =
         post_form
           ~service:election_tally_release
           (fun () ->
             [input
                 ~input_type:`Submit
                 ~value:"Compute the result"
                 string
             ]) uuid
       in
       return @@ div [
         div [
           pcdata "The ";
           a
             ~service:election_dir
             [pcdata "encrypted tally"]
             (uuid, ESETally);
           pcdata " has been computed. Its hash is ";
           b ~a:[a_id "encrypted_tally_hash"] [
             pcdata hash
           ];
           pcdata ".";
         ];
         div [
           div [pcdata "We are now waiting for trustees..."; threshold_or_not];
           table
             (tr [
               th [pcdata "Trustee"];
               th [pcdata "Mail"];
               th [pcdata "Link"];
               th [pcdata "Done?"];
             ] :: trustees)
         ];
         release_form;
       ]
    | `Tallied ->
       return @@ div [
         pcdata "This election has been tallied.";
       ]
    | `Archived ->
       return @@ div [
         pcdata "This election is archived. ";
         a ~service:election_download_archive [
             pcdata "Download archive.";
           ] (uuid, ());
       ]
  in
  let%lwt archive_date = match state with
    | `Tallied ->
       let%lwt t = Web_persist.get_election_date `Tally uuid in
       let t = datetime_add (Option.get t default_tally_date) (day days_to_archive) in
       return @@
         div [
             pcdata "This election will be automatically archived after ";
             pcdata (format_datetime t);
             pcdata ".";
           ]
    | _ -> return @@ pcdata ""
  in
  let div_archive = match state with
    | `Archived -> pcdata ""
    | _ -> div [
      br ();
      hr ();
      archive_date;
      post_form ~service:election_archive (fun () ->
        [
          input ~input_type:`Submit ~value:"Archive election" string;
          pcdata " Warning: this action is irreversible. Archiving an election makes it read-only; in particular, the election will be definitively closed (no vote submission, no vote counting).";
        ]
      ) uuid;
    ]
  in
  let%lwt deletion_date = match state with
    | `Open | `Closed | `EncryptedTally _ ->
       let%lwt t = Web_persist.get_election_date `Validation uuid in
       let dt = day days_to_delete in
       return @@ datetime_add (Option.get t default_validation_date) dt
    | `Tallied ->
       let%lwt t = Web_persist.get_election_date `Tally uuid in
       let dt = day (days_to_archive + days_to_delete) in
       return @@ datetime_add (Option.get t default_tally_date) dt
    | `Archived ->
       let%lwt t = Web_persist.get_election_date `Archive uuid in
       let dt = day days_to_delete in
       return @@ datetime_add (Option.get t default_archive_date) dt
  in
  let div_delete =
    div [
        br ();
        hr ();
        div [
            pcdata "This election will be automatically deleted after ";
            pcdata (format_datetime deletion_date);
            pcdata ".";
          ];
        post_form ~service:election_delete (fun () ->
            [
              input ~input_type:`Submit ~value:"Delete election" string;
              pcdata " Warning: this action is irreversible.";
            ]
          ) uuid;
      ]
  in
  let update_credential =
    match metadata.e_cred_authority with
    | Some "server" ->
       pcdata ""
    | _ ->
       div [
         a ~service:election_update_credential [pcdata "Update a credential"] uuid;
       ];
  in
  let cas = match metadata.e_auth_config with
    | Some [{auth_system = "cas"; _}] -> true
    | _ -> false
  in
  let div_regenpwd =
    if cas then
      pcdata ""
    else
      div [
          a ~service:election_regenpwd [pcdata "Regenerate and mail a password"] uuid;
        ]
  in
  let content = [
    div [
      a ~service:Web_services.election_home [pcdata "Election home"] (uuid, ());
    ];
    update_credential;
    div [
      a ~service:election_dir [pcdata "Voter list"] (uuid, ESVoters);
    ];
    div [
      a ~service:election_pretty_records [pcdata "Voting records"] (uuid, ());
    ];
    div [
      a ~service:election_missing_voters [pcdata "Missing voters"] (uuid, ());
    ];
    div_regenpwd;
    hr ();
    div [state_div];
    div_archive;
    div_delete;
  ] in
  let%lwt login_box = login_box ~cont:(ContSiteElection uuid) () in
  base ~title ~login_box ~content ()

let update_credential election () =
  let params = election.e_params in
  let uuid = params.e_uuid in
  let form = post_form ~service:election_update_credential_post
    (fun (old, new_) ->
      [
        div [
          p [
            pcdata "\
              This form allows you to change a single credential at \
              a time. To get the hash of a credential, run the \
              following command:\
            ";
          ];
          pre [
            pcdata "printf old-credential | sha256sum";
          ];
          p [
            pcdata "In the above command, ";
            code [pcdata "old-credential"];
            pcdata " should look like a big number written in base 10.";
          ];
        ];
        p [
          pcdata "Hash of the old credential: ";
          input ~name:old ~input_type:`Text ~a:[a_size 64] string;
        ];
        p [
          pcdata "New credential: ";
          input ~name:new_ ~input_type:`Text ~a:[a_size 617] string;
        ];
        p [input ~input_type:`Submit ~value:"Submit" string];
      ]
    ) uuid
  in
  let content = [
    form;
  ] in
  let%lwt login_box = login_box ~cont:(ContSiteElection uuid) () in
  base ~title:params.e_name ~login_box ~content ~uuid ()

let regenpwd uuid () =
  let form = post_form ~service:election_regenpwd_post
    (fun user ->
      [
        div [
          pcdata "Username: ";
          input ~name:user ~input_type:`Text string;
        ];
        div [input ~input_type:`Submit ~value:"Submit" string];
      ]
    ) uuid
  in
  let content = [ form ] in
  let title = "Regenerate and mail password" in
  let%lwt login_box = login_box ~cont:(ContSiteElection uuid) () in
  base ~title ~login_box ~content ~uuid ()

let cast_raw election () =
  let params = election.e_params in
  let uuid = params.e_uuid in
  let form_rawballot = post_form ~service:election_submit_ballot
    (fun name ->
      [
        div [pcdata "Please paste your encrypted ballot in JSON format in the following box:"];
        div [textarea ~a:[a_rows 10; a_cols 40] ~name ()];
        div [input ~input_type:`Submit ~value:"Submit" string];
      ]
    ) ()
  in
  let form_upload = post_form ~service:election_submit_ballot_file
    (fun name ->
      [
        div [pcdata "Alternatively, you can also upload a file containing your ballot:"];
        div [
          pcdata "File: ";
          file_input ~name ();
        ];
        div [input ~input_type:`Submit ~value:"Submit" string];
      ]
    ) ()
  in
  let intro = div [
    div [
      pcdata "You can create an encrypted ballot by using the command line tool ";
      pcdata "(available in the ";
      a ~service:source_code [pcdata "sources"] ();
      pcdata "), or its ";
      a ~service:(Eliom_service.static_dir ()) [
        pcdata "web interface";
      ] ["static"; "belenios-tool.html"];
      pcdata ". A specification of encrypted ballots is also available in the ";
      pcdata "sources.";
    ];
    div [
      a ~service:Web_services.election_home
        [pcdata "Back to election home"] (uuid, ());
    ];
  ] in
  let content = [
    intro;
    h3 [ pcdata "Submit by copy/paste" ];
    form_rawballot;
    h3 [ pcdata "Submit by file" ];
    form_upload;
  ] in
  let%lwt footer = audit_footer election in
  base ~title:params.e_name ~content ~uuid ~footer ()

let cast_confirmation election hash () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let%lwt user = Web_state.get_election_user uuid in
  let name = params.e_name in
  let user_div = match user with
    | Some u ->
      post_form ~service:election_cast_confirm (fun () -> [
        p ~a:[a_style "text-align: center; padding: 10px;"] [
          pcdata L.i_am;
          format_user ~site:false u;
          pcdata L.and_;
          input
            ~a:[a_style "font-size: 20px; cursor: pointer;"]
            ~input_type:`Submit ~value:L.i_cast_my_vote string;
          pcdata ".";
        ]
      ]) uuid
    | None ->
      div [
        pcdata L.please_login_to_confirm;
      ]
  in
  let%lwt div_revote =
    match user with
    | None -> return @@ pcdata ""
    | Some u ->
       let%lwt revote = Web_persist.has_voted uuid u in
       if revote then
         return @@ p [b [pcdata L.you_have_already_voted]]
       else
         return @@ pcdata ""
  in
  let progress = div ~a:[a_style "text-align:center;margin-bottom:20px;"] [
    pcdata L.input_credential;
    pcdata " — ";
    pcdata L.answer_to_questions;
    pcdata " — ";
    pcdata L.review_and_encrypt;
    pcdata " — ";
    pcdata L.authenticate;
    pcdata " — ";
    b [pcdata L.confirm];
    pcdata " — ";
    pcdata L.done_;
    hr ();
  ] in
  let content = [
    progress;
    div ~a:[a_class ["current_step"]] [
        pcdata L.booth_step5;
    ];
    p [
      pcdata L.your_ballot_for;
      em [pcdata name];
      pcdata L.has_been_received;
      pcdata L.your_tracker_is;
      b ~a:[a_id "ballot_tracker"] [
        pcdata hash
      ];
      pcdata ".";
      br ();
    ];
    br ();
    p [pcdata L.nobody_can_see];
    div_revote;
    user_div;
    p [
      (let service =
        Eliom_service.preapply
          Web_services.election_home (uuid, ())
      in
      a ~service [
        pcdata L.go_back_to_election
      ] ());
      pcdata ".";
    ];
  ] in
  base ~title:name ~content ~uuid ()

let cast_confirmed election ~result () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let l = Web_i18n.get_lang language in
  let module L = (val l) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let name = params.e_name in
  let progress = div ~a:[a_style "text-align:center;margin-bottom:20px;"] [
    pcdata L.input_credential;
    pcdata " — ";
    pcdata L.answer_to_questions;
    pcdata " — ";
    pcdata L.review_and_encrypt;
    pcdata " — ";
    pcdata L.authenticate;
    pcdata " — ";
    pcdata L.confirm;
    pcdata " — ";
    b [pcdata L.done_];
    hr ();
  ] in
  let result, step_title =
    match result with
    | Ok hash ->
       [pcdata L.has_been_accepted;
        pcdata " ";
        pcdata L.your_tracker_is;
        b ~a:[a_id "ballot_tracker"] [
          pcdata hash
        ];
        pcdata ". ";
        pcdata L.you_can_check_its_presence;
        a ~service:election_pretty_ballots [pcdata L.ballot_box] (uuid, ());
        pcdata L.anytime_during_the_election;
        pcdata L.confirmation_email;
       ], L.thank_you_for_voting
    | Error e ->
       [pcdata L.is_rejected_because;
        pcdata (Web_common.explain_error l e);
        pcdata ".";
       ], L.fail
  in
  let content = [
    progress;
    div ~a:[a_class ["current_step"]] [
        pcdata L.booth_step6;
        pcdata step_title;
    ];
    p ([
      pcdata L.your_ballot_for;
      em [pcdata name];
      ] @ result);
    p
      [a
         ~service:Web_services.election_home
         [pcdata L.go_back_to_election]
         (uuid, ())];
  ] in
  base ~title:name ~content ~uuid ()

let pretty_ballots election hashes result () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let title = params.e_name ^ " — " ^ L.accepted_ballots in
  let nballots = ref 0 in
  let hashes = List.sort compare_b64 hashes in
  let ballots =
    List.map
      (fun h ->
       incr nballots;
       li
         [a
            ~service:election_pretty_ballot
            [pcdata h]
            ((uuid, ()), h)]
      ) hashes
  in
  let links =
    p
      [a
         ~service:Web_services.election_home
         [pcdata L.go_back_to_election]
         (uuid, ())]
  in
  let number = match !nballots, result with
    | n, None ->
       div [
         pcdata (string_of_int n);
         pcdata L.ballots_have_been_accepted_so_far;
       ]
    | n, Some r when n = r.num_tallied ->
       div [
         pcdata (string_of_int n);
         pcdata L.ballots_have_been_accepted;
       ]
    | n, Some r -> (* should not happen *)
       div [
         pcdata (string_of_int n);
         pcdata L.ballots_have_been_accepted_and;
         pcdata (string_of_int r.num_tallied);
         pcdata L.have_been_tallied;
       ]
  in
  let content = [
    number;
    ul ballots;
    links;
  ] in
  base ~title ~content ~uuid ()

let pretty_records election records () =
  let uuid = election.e_params.e_uuid in
  let title = election.e_params.e_name ^ " — Records" in
  let records = List.map (fun (date, voter) ->
    tr [td [pcdata date]; td [pcdata voter]]
  ) records in
  let table = match records with
    | [] -> div [pcdata "Nobody voted!"]
    | _ ->
       div [
         table
           (tr [th [pcdata "Date/Time (UTC)"]; th [pcdata "Username"]]
            :: records);
       ]
  in
  let content = [
    div [
      pcdata "You can also access the ";
      a ~service:election_dir [pcdata "raw data"] (uuid, ESRecords);
      pcdata ".";
    ];
    table;
  ] in
  let%lwt login_box = login_box ~cont:(ContSiteElection uuid) () in
  base ~title ~login_box ~content ()

let tally_trustees election trustee_id token () =
  let params = election.e_params in
  let uuid = params.e_uuid in
  let title =
    params.e_name ^ " — Partial decryption #" ^ string_of_int trustee_id
  in
  let%lwt encrypted_private_key =
    match%lwt Web_persist.get_private_keys uuid with
    | None -> return_none
    | Some keys -> return (Some (List.nth keys (trustee_id-1)))
  in
  let content = [
    p [pcdata "It is now time to compute your partial decryption factors."];
    p [
      pcdata "The hash of the encrypted tally is ";
      b [span ~a:[a_id "hash"] []];
      pcdata "."
    ];
    (
      match encrypted_private_key with
      | None -> pcdata ""
      | Some epk ->
         div ~a:[a_style "display:none;"] [
             unsafe_textarea "encrypted_private_key" epk
           ];
    );
    hr ();
    div [
        b [pcdata "Instructions:"];
        ol [
            li [
                div ~a:[a_id "input_private_key"] [
                    div [
                        p [pcdata "Please enter your private key:"];
                        input
                          ~a:[a_id "private_key"; a_size 80]
                          ~input_type:`Text
                          string;
                      ];
                    div [
                        p [pcdata "Or load it from a file:"];
                        input
                          ~a:[a_id "private_key_file"]
                          ~input_type:`File
                          string;
                      ];
                  ];
                br ();
              ];
            li [
                div [
                    button_no_value
                      ~a:[a_id "compute"]
                      ~button_type:`Button
                      [pcdata "Compute decryption factors"];
                  ];
                br ();
              ];
            li [
                div ~a:[a_id "pd_done"] [
                    post_form
                      ~service:election_tally_trustees_post
                      (fun pd ->
                        [
                          div [
                              input ~input_type:`Submit ~value:"Submit" string;
                              pcdata " your contribution to decryption.";
                            ];
                          div [
                              pcdata "Data: ";
                              textarea
                                ~a:[a_rows 5; a_cols 40; a_id "pd"]
                                ~name:pd
                                ();
                            ];
                        ]
                      ) (uuid,  token);
                  ];
              ];
          ];
      ];
    div [
      script ~a:[a_src (static "sjcl.js")] (pcdata "");
      script ~a:[a_src (static "jsbn.js")] (pcdata "");
      script ~a:[a_src (static "jsbn2.js")] (pcdata "");
      script ~a:[a_src (static "random.js")] (pcdata "");
      script ~a:[a_src (static "tool_js_pd.js")] (pcdata "");
    ]
  ] in
  base ~title ~content ~uuid ()

let login_choose auth_systems service () =
  let auth_systems =
    auth_systems |>
    List.map (fun name ->
      a ~service:(service name) [pcdata name] ()
    ) |> List.join (pcdata ", ")
  in
  let content = [
    div [p (
      [pcdata "Please log in: ["] @ auth_systems @ [pcdata "]"]
    )]
  ] in
  base ~title:"Log in" ~content ()

let login_dummy () =
  let title, field_name, input_type =
    "Dummy login", "Username:", `Text
  in
  let form = post_form ~service:dummy_post
    (fun name ->
      [
        tablex [tbody [
          tr [
            th [label ~a:[a_label_for (Eliom_parameter.string_of_param_name name)] [pcdata field_name]];
            td [input ~a:[a_maxlength 50] ~input_type ~name string];
          ]]
        ];
        div [
          input ~input_type:`Submit ~value:"Login" string;
        ]
      ]) ()
  in
  let content = [
    form;
  ] in
  base ~title ~content ()

let login_password ~service ~allowsignups =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let signup =
    if allowsignups then
      div [
          br ();
          pcdata "You can also ";
          a ~service:signup_captcha [pcdata "create an account"] service;
          pcdata ", or ";
          a ~service:changepw_captcha [pcdata "change your password"] service;
          pcdata " (if you forgot it, for example).";
        ]
    else pcdata ""
  in
  let form = post_form ~service:password_post
    (fun (llogin, lpassword) ->
      [
        tablex [tbody [
          tr [
            th [label ~a:[a_label_for (Eliom_parameter.string_of_param_name llogin)] [pcdata L.username]];
            td [input ~a:[a_maxlength 50] ~input_type:`Text ~name:llogin string];
          ];
          tr [
            th [label ~a:[a_label_for (Eliom_parameter.string_of_param_name lpassword)] [pcdata L.password]];
            td [input ~a:[a_maxlength 50] ~input_type:`Password ~name:lpassword string];
          ];
        ]];
        div [
          input ~input_type:`Submit ~value:L.login string;
        ]
      ]) ()
  in
  let content = [
    form;
    signup;
  ] in
  base ~title:L.password_login ~content ()

let signup_captcha_img challenge =
  let src = make_uri ~service:signup_captcha_img challenge in
  img ~src ~alt:"CAPTCHA" ()

let format_captcha_error = function
  | None -> pcdata ""
  | Some x ->
     let msg = match x with
       | BadCaptcha -> "Bad security code!"
       | BadAddress -> "Bad e-mail address!"
     in
     div ~a:[a_style "color: red;"] [pcdata msg]

let signup_captcha ~service error challenge email =
  let form =
    post_form ~service:signup_captcha_post
      (fun (lchallenge, (lresponse, lemail)) ->
        [
          div [
              pcdata "E-mail address: ";
              input ~input_type:`Text ~name:lemail ~value:email string;
            ];
          div [
              input ~input_type:`Hidden ~name:lchallenge ~value:challenge string;
              pcdata "Please enter ";
              signup_captcha_img challenge;
              pcdata " in the following box: ";
              input ~input_type:`Text ~name:lresponse string;
            ];
          div [
              input ~input_type:`Submit ~value:"Submit" string;
            ];
        ]
      ) service
  in
  let error = format_captcha_error error in
  let content = [error; form] in
  base ~title:"Create an account" ~content ()

let signup_changepw ~service error challenge email username =
  let form =
    post_form ~service:changepw_captcha_post
      (fun (lchallenge, (lresponse, (lemail, lusername))) ->
        [
          div [
              pcdata "E-mail address: ";
              input ~input_type:`Text ~name:lemail ~value:email string;
              pcdata " or username: ";
              input ~input_type:`Text ~name:lusername ~value:username string;
              pcdata ".";
            ];
          div [
              input ~input_type:`Hidden ~name:lchallenge ~value:challenge string;
              pcdata "Please enter ";
              signup_captcha_img challenge;
              pcdata " in the following box: ";
              input ~input_type:`Text ~name:lresponse string;
            ];
          div [
              input ~input_type:`Submit ~value:"Submit" string;
            ];
        ]
      ) service
  in
  let error = format_captcha_error error in
  let content = [error; form] in
  base ~title:"Change password" ~content ()

let signup address error username =
  let error = match error with
    | None -> pcdata ""
    | Some e ->
       let msg = match e with
         | UsernameTaken -> "the username is already taken"
         | AddressTaken -> "there is already an account with this e-mail address"
         | BadUsername -> "the username is invalid"
         | BadPassword e -> Printf.sprintf "the password is too weak (%s)" e
         | PasswordMismatch -> "the two passwords are not the same"
       in
       div [
           pcdata "The account creation ";
           span ~a:[a_style "color: red;"] [pcdata "failed"];
           pcdata " because ";
           pcdata msg;
           pcdata ". Please try again with a different one.";
         ]
  in
  let form =
    post_form ~service:signup_post
      (fun (lusername, (lpassword, lpassword2)) ->
        [
          div [
              pcdata "Your e-mail address is: ";
              pcdata address;
              pcdata ".";
            ];
          div [
              pcdata "Please choose a username: ";
              input ~input_type:`Text ~name:lusername ~value:username string;
              pcdata " and a password: ";
              input ~input_type:`Password ~name:lpassword string;
              pcdata ".";
            ];
          div[
              pcdata "Type the password again: ";
              input ~input_type:`Password ~name:lpassword2 string;
              pcdata ".";
            ];
          div [
              input ~input_type:`Submit ~value:"Submit" string;
            ];
        ]
      ) ()
  in
  let content = [error; form] in
  base ~title:"Create an account" ~content ()

let changepw ~username ~address error =
  let error = match error with
    | None -> pcdata ""
    | Some e ->
       let reason = match e with
         | `PasswordMismatch -> "the two passwords are not the same"
         | `WeakPassword e -> Printf.sprintf "the new password is too weak (%s)" e
       in
       div [
           pcdata "The change ";
           span ~a:[a_style "color: red;"] [pcdata "failed"];
           pcdata " because ";
           pcdata reason;
           pcdata ". Please try again with a different one.";
         ]
  in
  let form =
    post_form ~service:changepw_post
      (fun (lpassword, lpassword2) ->
        [
          div [
              pcdata "Your username is: ";
              pcdata username;
              pcdata " and your e-mail address is: ";
              pcdata address;
              pcdata ".";
            ];
          div [
              pcdata "Please choose a password: ";
              input ~input_type:`Password ~name:lpassword string;
              pcdata ".";
            ];
          div [
              pcdata "Type the password again: ";
              input ~input_type:`Password ~name:lpassword2 string;
              pcdata ".";
            ];
          div [
              input ~input_type:`Submit ~value:"Submit" string;
            ];
        ]
      ) ()
  in
  let content = [error; form] in
  base ~title:"Change password" ~content ()

let booth () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let head = head (title (pcdata L.belenios_booth)) [
    link ~rel:[`Stylesheet] ~href:(static "booth.css") ();
    script ~a:[a_src (static "sjcl.js")] (pcdata "");
    script ~a:[a_src (static "jsbn.js")] (pcdata "");
    script ~a:[a_src (static "jsbn2.js")] (pcdata "");
    script ~a:[a_src (static "random.js")] (pcdata "");
    script ~a:[a_src (static "tool_js_booth.js")] (pcdata "");
  ] in
  let wait_div =
    div ~a:[a_id "wait_div"] [
        pcdata "Please wait... ";
        img ~src:(static "encrypting.gif") ~alt:"Loading..." ();
      ]
  in
  let election_loader =
    div ~a:[a_id "election_loader"; a_style "display:none;"] [
      h1 [pcdata L.belenios_booth];
      br ();
      pcdata "Load an election by giving its URL:";
      div [unsafe_textarea "url" ""];
      div [button_no_value ~button_type:`Button ~a:[a_id "load_url"] [pcdata "Load URL"]];
      br ();
      pcdata "Load an election by giving its parameters:";
      div [unsafe_textarea "election_params" ""];
      div [button_no_value ~button_type:`Button ~a:[a_id "load_params"] [pcdata "Load parameters"]];
    ]
  in
  let text_choices = unsafe_textarea "choices" "" in
  let ballot_form =
    post_form ~a:[a_id "ballot_form"] ~service:election_submit_ballot
      (fun encrypted_vote -> [
        div ~a:[a_id "div_ballot"; a_style "display:none;"] [
          pcdata "Encrypted ballot:";
          div [
            textarea
              ~a:[a_id "ballot"; a_rows 1; a_cols 80; a_readonly ()]
              ~name:encrypted_vote ();
          ];
        ];
        p [
          pcdata L.successfully_encrypted;
          b [pcdata L.not_cast_yet];
          pcdata L.qmark;
        ];
        p [
          pcdata L.your_tracker_is;
          span ~a:[a_id "ballot_tracker"] [];
        ];
        p [
          pcdata L.we_invite_you_to_save_it;
        ];
        br ();
        div ~a:[a_id "div_submit"] [
            input ~input_type:`Submit ~value:L.continue ~a:[a_style "font-size:30px;"] string;
          ];
        div ~a:[a_id "div_submit_manually"; a_style "display:none;"] [
            pcdata "You must submit your ballot manually.";
          ];
        br (); br ();
       ])
      ()
  in
  let main =
    div ~a:[a_id "main"] [
      div ~a:[a_style "text-align:center; margin-bottom:20px;"] [
        span ~a:[a_id "progress1"; a_style "font-weight:bold;"] [pcdata L.input_credential];
        pcdata " — ";
        span ~a:[a_id "progress2"] [pcdata L.answer_to_questions];
        pcdata " — ";
        span ~a:[a_id "progress3"] [pcdata L.review_and_encrypt];
        pcdata " — ";
        span ~a:[a_id "progress4"] [pcdata L.authenticate];
        pcdata " — ";
        span ~a:[a_id "progress5"] [pcdata L.confirm];
        pcdata " — ";
        span ~a:[a_id "progress6"] [pcdata L.done_];
        hr ();
      ];
      div ~a:[a_id "intro"; a_style "text-align:center;"] [
        div ~a:[a_class ["current_step"]] [
          pcdata L.booth_step1;
        ];
        br (); br ();
        p ~a:[a_id "input_code"; a_style "font-size:20px;"] [
          pcdata L.input_your_credential;
        ];
        br (); br ();
      ];
      div ~a:[a_id "question_div"; a_style "display:none;"] [
        div ~a:[a_class ["current_step"]] [
          pcdata L.booth_step2;
        ];
      ];
      div ~a:[a_id "plaintext_div"; a_style "display:none;"] [
        div ~a:[a_class ["current_step"]] [
          pcdata L.booth_step3;
        ];
        div ~a:[a_id "pretty_choices"] [];
        div ~a:[a_style "display:none;"] [
          pcdata "Plaintext raw ballot:";
          div [text_choices];
        ];
        div ~a:[a_style "text-align:center;"] [
          div ~a:[a_id "encrypting_div"] [
            p [pcdata L.wait_while_encrypted];
            img ~src:(static "encrypting.gif") ~alt:L.encrypting ();
          ];
          div ~a:[a_id "ballot_div"; a_style "display:none;"] [ballot_form];
          Unsafe.data ("<button onclick=\"location.reload();\">"^L.restart^"</button>");
          br (); br ();
        ];
      ];
    ]
  in
  let booth_div =
    div ~a:[a_id "booth_div"; a_style "display:none;"] [
      div ~a:[a_id "header"] [
        div ~a:[a_style "float: left; padding: 15px;"] [
          img ~alt:L.election_server ~a:[a_height 70]
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
            pcdata L.election_uuid;
            span ~a:[a_id "election_uuid"] [];
          ];
          div [
            pcdata L.election_fingerprint;
            span ~a:[a_id "election_fingerprint"] [];
          ];
        ];
      ];
      div ~a:[a_style "display:none;"] [
        span ~a:[a_id "str_here"] [pcdata L.here];
        span ~a:[a_id "question_header"] [pcdata L.question_header];
        span ~a:[a_id "at_least"] [pcdata L.at_least];
        span ~a:[a_id "at_most"] [pcdata L.at_most];
        span ~a:[a_id "str_previous"] [pcdata L.previous];
        span ~a:[a_id "str_next"] [pcdata L.next];
        span ~a:[a_id "str_nothing"] [pcdata L.nothing];
        span ~a:[a_id "enter_cred"] [pcdata L.enter_cred];
        span ~a:[a_id "invalid_cred"] [pcdata L.invalid_cred];
        span ~a:[a_id "str_blank_vote"] [pcdata L.blank_vote];
        span ~a:[a_id "no_other_blank"] [pcdata L.no_other_blank];
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
  return @@ html ~a:[a_dir `Ltr; a_xml_lang L.lang] head body

let contact_footer metadata please_contact =
  match metadata.e_contact with
  | None -> ""
  | Some x -> Printf.sprintf "\n\n%s\n  %s" please_contact x
