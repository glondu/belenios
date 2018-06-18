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
open Web_signatures
open Web_common
open Web_services
open Eliom_content.Html5.F

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

let make_login_box ~site auth links =
  let style = if site then admin_background else "" in
  let style = "float: right; text-align: right;" ^ style in
  let module S = (val auth : AUTH_SERVICES) in
  let module L = (val links : AUTH_LINKS) in
  let%lwt user = S.get_user () in
  let%lwt auth_systems = S.get_auth_systems () in
  let body =
    match user with
    | Some user ->
      [
        div [
          pcdata "Logged in as ";
          format_user ~site user;
          pcdata ".";
        ];
        div [
          a ~a:[a_id "logout"] ~service:L.logout [pcdata "Log out"] ();
          pcdata ".";
        ];
      ]
    | None ->
       if site then
      [
        div [
          pcdata "Not logged in.";
        ];
        let auth_systems =
          auth_systems |>
          List.map (fun name ->
            a ~a:[a_id ("login_" ^ name)]
              ~service:(L.login (Some name)) [pcdata name] ()
          ) |> list_join (pcdata ", ")
        in
        div (
          [pcdata "Log in: ["] @ auth_systems @ [pcdata "]"]
        );
      ]
        else []
  in
  match body with
  | [] -> return None
  | _::_ -> return (Some (div ~a:[a_style style] body))

module Site_links = struct
  let login x = Eliom_service.preapply site_login x
  let logout = Eliom_service.preapply logout ()
end

module Site_auth = struct
  let get_user () = Web_state.get_site_user ()
  let get_auth_systems () =
    let%lwt l = Web_persist.get_auth_config None in
    return (List.map fst l)
end

let site_links = (module Site_links : AUTH_LINKS)
let site_auth = (module Site_auth : AUTH_SERVICES)

let site_login_box () =
  make_login_box ~site:true site_auth site_links

let belenios_url = Eliom_service.Http.external_service
  ~prefix:"http://www.belenios.org"
  ~path:[]
  ~get_params:Eliom_parameter.unit
  ()

let base ~title ?login_box ~content ?(footer = div []) ?uuid () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let administer =
    match uuid with
    | None ->
       a ~service:admin [pcdata L.administer_elections] ()
    | Some uuid ->
       a ~service:election_admin [pcdata L.administer_this_election] (uuid, ())
  in
  let login_box = match login_box with
    | None ->
       div ~a:[a_style "float: right; padding: 10px;"] [
         img ~a:[a_height 70] ~alt:""
           ~src:(static "placeholder.png") ();
       ]
    | Some x -> x
  in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang L.lang]
    (head (Eliom_content.Html5.F.title (pcdata title)) [
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
      div ~a:[a_id "main"] content;
      div ~a:[a_id "footer"; a_style "text-align: center;" ] [
        div ~a:[a_id "bottom"] [
          footer;
          pcdata L.powered_by;
          a ~service:belenios_url [pcdata "Belenios"] ();
          Belenios_version.(
            Printf.ksprintf pcdata " %s (%s)." version build
          );
          a ~service:source_code [pcdata L.get_the_source_code] ();
          pcdata ". ";
          unsafe_a !gdpr_uri "Privacy policy";
          pcdata ". ";
          administer;
          pcdata ".";
        ]
      ]]
     ]))

let format_election election =
  let e = election.e_params in
  let service = election_admin in
  li [
    a ~service [pcdata e.e_name] (e.e_uuid, ());
  ]

let admin_gdpr () =
  let title = site_title ^ " — Personal data processing notice" in
  let content =
    [
      div [
          pcdata "To use this site, you must accept our ";
          unsafe_a !gdpr_uri "personal data policy";
          pcdata ".";
        ];
      post_form ~service:admin_gdpr_accept
        (fun () ->
          [
            div [
                string_input ~input_type:`Submit ~value:"Accept" ();
              ];
          ]
        ) ();
    ]
  in
  base ~title ~content ()

let admin ~elections () =
  let title = site_title ^ " — Administration" in
  match elections with
  | None ->
     let contact = match !contact_uri with
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
     let%lwt login_box = site_login_box () in
     base ~title ?login_box ~content ()
  | Some (elections, tallied, archived, draft_elections) ->
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
    let draft_elections =
      match draft_elections with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@
         List.map (fun (k, title) ->
           li [a ~service:election_draft [pcdata title] k]
         ) draft_elections
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
        draft_elections;
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
    let%lwt login_box = site_login_box () in
    base ~title ?login_box ~content ()

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
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

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
  let cred_info = Eliom_service.Http.external_service
    ~prefix:"http://www.belenios.org"
    ~path:["setup.php"]
    ~get_params:Eliom_parameter.unit
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
                string_radio ~checked:true ~name:credmgmt ~value:"auto" ();
                pcdata " Automatic (degraded mode - credentials will be handled by the server)";
              ];
              div [
                string_radio ~name:credmgmt ~value:"manual" ();
                pcdata " Manual (safe mode - a third party will handle the credentials)";
              ];
            ];
          fieldset
            ~legend:(legend [pcdata "Authentication"])
            [
              div [
                string_radio ~checked:true ~name:auth ~value:"password" ();
                pcdata " Password (passwords will be emailed to voters)";
              ];
              div [
                string_radio ~name:auth ~value:"cas" ();
                pcdata " CAS (external authentication server), server address: ";
                string_input ~input_type:`Text ~name:cas_server ();
                pcdata " (for example: https://cas.inria.fr/cas)";
              ];
            ];
          div [
            string_input ~input_type:`Submit ~value:"Proceed" ();
          ];
        ]
      ) ()
  in
  let content = [
    form
  ] in
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

let election_draft uuid se () =
  let title = "Preparation of election " ^ se.se_questions.t_name in
  let form_languages =
    post_form ~service:election_draft_languages
      (fun languages ->
        [
          div [
              pcdata "Languages: ";
              string_input ~name:languages ~input_type:`Text
                ~value:(string_of_languages se.se_metadata.e_languages) ();
              pcdata " (Available languages: ";
              pcdata (string_of_languages (Some available_languages));
              pcdata ")";
            ];
          div [
              pcdata "This is a space-separated list of languages that will be used in emails sent by the server.";
            ];
          div [
              string_input ~input_type:`Submit ~value:"Save changes" ();
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
            string_input ~name:name
              ~input_type:`Text ~value:se.se_questions.t_name ();
          ];
          div [
            div [pcdata "Description of the election: "];
            div [
              textarea ~name:description ~a:[a_cols 80]
                ~value:se.se_questions.t_description ();
            ];
          ];
          div [
            string_input ~input_type:`Submit ~value:"Save changes" ();
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
              string_input ~name:contact ~input_type:`Text ~value ();
            ];
          div [
              pcdata "This contact will be added to emails sent to the voters.";
            ];
          div [
              string_input ~input_type:`Submit ~value:"Save changes" ();
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
               [string_input ~input_type:`Submit ~value:"Generate and mail missing passwords" ()]
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
      ]
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
                [string_input ~input_type:`Submit ~value:"Generate on server" ()]
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
    let t = option_get se.se_creation_date default_creation_date in
    let t = datetime_add t (day 365) in
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
              string_input ~input_type:`Submit ~value:"Destroy election" ();
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
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

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
          string_input ~input_type:`Text ~name ();
          string_input ~input_type:`Submit ~value:"Add" ();
        ]
      ) uuid
  in
  let form_trustees_add_server =
    match List.filter (fun {st_id; _} -> st_id = "server") se.se_public_keys with
    | [] ->
       post_form
         ~service:election_draft_trustee_add_server
         (fun () ->
           [
             string_input ~input_type:`Submit ~value:"Add the server" ()
           ]
         ) uuid
    | _ -> pcdata ""
  in
  let mk_form_trustee_del value =
    post_form
      ~service:election_draft_trustee_del
      (fun name ->
        [
          int_input ~input_type:`Hidden ~name ~value ();
          string_input ~input_type:`Submit ~value:"Remove" ();
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
                   ~absolute:true ~service:election_draft_trustee t.st_token
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
                   a ~service:election_draft_trustee [pcdata "Link"] t.st_token;
                   ) else (
                     pcdata "(server)"
                   )
               ];
               td [
                 pcdata (if t.st_public_key = "" then "No" else "Yes");
               ];
               td [mk_form_trustee_del i];
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
          form_trustees_add_server;
        ]
    else pcdata ""
  in
  let div_content =
    div [
      div [
          pcdata "To set up the election key, you need to nominate trustees. Each trustee will create her own secret key.";
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
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

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
            string_input ~input_type:`Text ~name ();
            string_input ~input_type:`Submit ~value:"Add" ();
          ]
        ) uuid
    else pcdata ""
  in
  let mk_form_trustee_del value =
    post_form
      ~service:election_draft_threshold_trustee_del
      (fun name ->
        [
          int_input ~input_type:`Hidden ~name ~value ();
          string_input ~input_type:`Submit ~value:"Remove" ();
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
             th [pcdata "Step"];
           ] @ (if show_add_remove then [th [pcdata "Remove"]] else [])
         ) ::
           List.mapi (fun i t ->
             tr (
                 [
                   td [
                       pcdata t.stt_id;
                     ];
                   td [
                       let uri = rewrite_prefix @@
                                   Eliom_uri.make_string_uri
                                     ~absolute:true ~service:election_draft_threshold_trustee t.stt_token
                       in
                       let body = Printf.sprintf mail_trustee_generation uri in
                       let subject = "Link to generate the decryption key" in
                       a_mailto ~dest:t.stt_id ~subject ~body "Mail"
                     ];
                   td [
                       a ~service:election_draft_threshold_trustee [pcdata "Link"] t.stt_token;
                     ];
                   td [
                       pcdata (string_of_int (match t.stt_step with None -> 0 | Some x -> x));
                     ];
                 ] @ (if show_add_remove then [td [mk_form_trustee_del i]] else [])
               )
             ) ts
         );
       div [
           pcdata "Meaning of steps:";
           ul [
               li [pcdata "0: administrator needs to set threshold"];
               li [pcdata "1: action needed from trustee: generate private key"];
               li [pcdata "2, 4, 6: waiting for other trustees"];
               li [pcdata "3, 5: action needed from trustee: enter private key"];
               li [pcdata "7: the key establishment protocol is finished"];
             ];
         ];
       br ();
       ]
  in
  let form_threshold =
    div [
        let value =
          match se.se_threshold with
          | None -> 0
          | Some i -> i
        in
        post_form
          ~service:election_draft_threshold_set
          (fun name ->
            [
              pcdata "Threshold: ";
              int_input ~input_type:`Text ~name ~value ();
              string_input ~input_type:`Submit ~value:"Set" ();
            ]
          ) uuid
      ]
  in
  let threshold_warning =
    if show_add_remove then pcdata "" else
      div [
          b [pcdata "Warning:"];
          pcdata " any change will re-initialize the whole process.";
          pcdata " To edit trustees and restart the process, set to 0.";
        ]
  in
  let maybe_error =
    match se.se_threshold_error with
    | None -> pcdata ""
    | Some e -> div [b [pcdata "ERROR: "]; pcdata e; br (); br ()]
  in
  let div_content =
    div [
      div [pcdata "On this page, you can configure a group of trustees such that only a threshold of them is needed to perform the decryption."];
      br ();
      form_threshold;
      threshold_warning;
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
    ]
  in
  let back_link = div [
    a ~service:Web_services.election_draft
      [pcdata "Go back to election draft"] uuid;
  ] in
  let content = [
    div_content;
    back_link;
  ] in
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

let election_draft_credential_authority _ se () =
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
              se.se_public_creds
          ]
          se.se_public_creds;
      ];
    ];
    div [
      pcdata "Note that this authority will have to send each credential to each voter herself.";
    ];
  ] in
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

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
         div [string_input ~input_type:`Submit ~value:"Save changes" ()]])
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
  let content = [
    interactivity;
    form;
  ] in
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

let election_draft_voters uuid se maxvoters () =
  let title = "Voters for election " ^ se.se_questions.t_name in
  let form =
    post_form
      ~service:election_draft_voters_add
      (fun name ->
        [
          div [textarea ~a:[a_rows 20; a_cols 50] ~name ()];
          div [string_input ~input_type:`Submit ~value:"Add" ()]])
      uuid
  in
  let mk_remove_button id =
    post_form
      ~service:election_draft_voters_remove
      (fun name ->
        [
          string_input ~input_type:`Hidden ~name ~value:id ();
          string_input ~input_type:`Submit ~value:"Remove" ();
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
          string_input ~input_type:`Hidden ~name ~value ();
          string_input ~input_type:`Submit ~value:"Send again" ();
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
          [string_input ~input_type:`Submit ~value:"Generate and mail missing passwords" ()]
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
            pcdata "):"
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
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

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
           div [string_input ~input_type:`Submit ~value:"Submit public credentials" ()]]])
      token
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
           div [string_input ~input_type:`Submit ~value:"Submit" ()]]])
      token
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
    let service = Eliom_service.preapply election_draft_trustee_post token in
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
           div [string_input ~input_type:`Submit ~value:"Submit public key" ()];
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
       try return (List.find (fun x -> x.stt_token = token) ts)
       with Not_found -> fail_http 404
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
              div [a ~service:home ~a:[a_id "private_key"] [pcdata "Private key"] ()];
              b [pcdata "Instructions:"];
              ol [
                  li [pcdata "download your private key using the link above;"];
                  li [pcdata "submit public data using the button below."];
                ];
            ];
          div [
              div [
                  pcdata "Data: ";
                  textarea ~a:[a_id "data"] ~name:data ();
                ];
              div [string_input ~input_type:`Submit ~value:"Submit" ()];
            ];
        ]
      ) token
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
      div_link;
      inputs;
      interactivity;
      br ();
      form;
    ]
  in
  base ~title ~content ()

let election_draft_importer ~service ~title uuid (elections, tallied, archived) () =
  let format_election election =
    let name = election.e_params.e_name in
    let uuid_s = raw_string_of_uuid election.e_params.e_uuid in
    let form = post_form ~service
      (fun from ->
        [
          div [pcdata name; pcdata " ("; pcdata uuid_s; pcdata ")"];
          div [
            user_type_input raw_string_of_uuid
              ~input_type:`Hidden
              ~name:from
              ~value:election.e_params.e_uuid ();
            string_input ~input_type:`Submit ~value:"Import from this election" ();
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
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

let election_draft_import uuid se elections =
  let title = "Election " ^ se.se_questions.t_name ^ " — Import voters from another election" in
  let service = election_draft_import_post in
  election_draft_importer ~service ~title uuid elections

let election_draft_import_trustees uuid se elections =
  let title = "Election " ^ se.se_questions.t_name ^ " — Import trustees from another election" in
  let service = election_draft_import_trustees_post in
  election_draft_importer ~service ~title uuid elections

let election_draft_confirm uuid se () =
  let title = "Election " ^ se.se_questions.t_name ^ " — Validate creation" in
  let ready, questions =
    if se.se_questions.t_questions = default_questions then
      false, "Not edited"
    else
      true, "OK"
  in
  let ready, voters =
    ready && not (se.se_voters = []),
    Printf.sprintf "%d voter(s)" (List.length se.se_voters)
  in
  let ready, passwords =
    match se.se_metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] ->
       if List.for_all (fun v -> v.sv_password <> None) se.se_voters then ready, "OK"
       else false, "Missing"
    | _ -> ready, "Not applicable"
  in
  let ready, credentials =
    if se.se_public_creds_received then
      ready, if se.se_metadata.e_cred_authority = None then "Received" else "Sent"
    else false, "Missing"
  in
  let ready, trustees =
    match se.se_public_keys with
    | [] -> ready, "OK"
    | _ :: _ ->
       match se.se_threshold_trustees with
       | None -> if List.for_all (fun {st_public_key; _} ->
                        st_public_key <> ""
                      ) se.se_public_keys then ready, "OK" else false, "Missing"
       | Some _ ->
          if se.se_threshold_parameters <> None &&
               match se.se_threshold_trustees with
               | None -> false
               | Some ts ->
                  List.for_all (fun {stt_step; _} -> stt_step = Some 7) ts
          then ready, "OK"
          else false, "Missing"
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
      td [pcdata "Questions?"];
      td [pcdata questions];
    ];
    tr [
      td [pcdata "Voters?"];
      td [pcdata voters];
    ];
    tr [
      td [pcdata "Passwords?"];
      td [pcdata passwords];
    ];
    tr [
      td [pcdata "Credentials?"];
      td [pcdata credentials];
    ];
    tr [
      td [pcdata "Trustees?"];
      td [pcdata trustees];
    ];
    tr [
      td [pcdata "Contact?"];
      td [pcdata contact];
    ];
  ] in
  let checklist = div [
    h2 [pcdata "Checklist"];
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
               string_input ~input_type:`Submit ~value:"Create election" ();
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
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

let election_login_box uuid =
  let module A = struct
    let get_user () =
      Web_state.get_election_user uuid
    let get_auth_systems () =
      let%lwt l = Web_persist.get_auth_config (Some uuid) in
      return @@ List.map fst l
  end in
  let auth = (module A : AUTH_SERVICES) in
  let module L = struct
    let login x =
      Eliom_service.preapply election_login ((uuid, ()), x)
    let logout =
      Eliom_service.preapply logout ()
  end in
  let links = (module L : AUTH_LINKS) in
  fun () -> make_login_box ~site:false auth links

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
  let module L = (val Web_i18n.get_lang language) in
  let params = election.e_params in
  let uuid = params.e_uuid in
  let state_ =
    match state with
    | `Closed ->
      [
        pcdata " ";
        b [pcdata L.election_currently_closed];
      ]
    | `Open -> []
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
    | `Tallied _ ->
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
          ~service:(Eliom_service.preapply election_cast (uuid, ()))
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
        a ~service:set_language [pcdata lang] lang
       ) available_languages)
  in
  let%lwt scd = Eliom_reference.get Web_state.show_cookie_disclaimer in
  let cookie_disclaimer =
    if scd then
      div
        ~a:[a_style "border-style: solid; border-width: 1px;"]
        [
          pcdata L.by_using_you_accept;
          unsafe_a !gdpr_uri L.privacy_policy;
          pcdata ". ";
          a ~service:set_cookie_disclaimer [pcdata L.accept] ();
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
  let%lwt login_box = election_login_box uuid () in
  base ~title:params.e_name ?login_box ~content ~footer ~uuid ()

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
         pcdata msg;
         string_input ~input_type:`Submit ~value ();
         pcdata msg2;
       ]) (uuid, ())
  in
  let%lwt state_div =
    match state with
    | `Open ->
       return @@ div [
         state_form true;
       ]
    | `Closed ->
       return @@ div [
         state_form false;
         br ();
         post_form
           ~service:election_compute_encrypted_tally
           (fun () ->
             [string_input
                 ~input_type:`Submit
                 ~value:"Proceed to vote counting"
                 ();
              pcdata " Warning: this action is irreversible; the election will be definitively closed.";
             ]) (uuid, ());
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
             let x = (uuid, ((), token)) in
             let uri = rewrite_prefix @@ Eliom_uri.make_string_uri
               ~absolute:true ~service x
             in
             let link_content, dest = match name with
               | None -> uri, !server_mail
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
             [string_input
                 ~input_type:`Submit
                 ~value:"Compute the result"
                 ()
             ]) (uuid, ())
       in
       return @@ div [
         div [
           pcdata "The ";
           a
             ~service:election_dir
             [pcdata "encrypted tally"]
             (uuid, ESETally);
           pcdata " has been computed. Its hash is ";
           b [pcdata hash];
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
    | `Tallied _ ->
       return @@ div [
         pcdata "This election has been tallied.";
       ]
    | `Archived ->
       return @@ div [
         pcdata "This election is archived.";
       ]
  in
  let%lwt archive_date = match state with
    | `Tallied _ ->
       let%lwt t = Web_persist.get_election_date `Tally uuid in
       let t = datetime_add (option_get t default_tally_date) (day days_to_archive) in
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
          string_input ~input_type:`Submit ~value:"Archive election" ();
          pcdata " Warning: this action is irreversible. Archiving an election makes it read-only; in particular, the election will be definitively closed (no vote submission, no vote counting).";
        ]
      ) (uuid, ());
    ]
  in
  let%lwt deletion_date = match state with
    | `Open | `Closed | `EncryptedTally _ ->
       let%lwt t = Web_persist.get_election_date `Validation uuid in
       let dt = day days_to_delete in
       return @@ datetime_add (option_get t default_validation_date) dt
    | `Tallied _ ->
       let%lwt t = Web_persist.get_election_date `Tally uuid in
       let dt = day (days_to_archive + days_to_delete) in
       return @@ datetime_add (option_get t default_tally_date) dt
    | `Archived ->
       let%lwt t = Web_persist.get_election_date `Archive uuid in
       let dt = day days_to_delete in
       return @@ datetime_add (option_get t default_archive_date) dt
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
              string_input ~input_type:`Submit ~value:"Delete election" ();
              pcdata " Warning: this action is irreversible.";
            ]
          ) (uuid, ());
      ]
  in
  let update_credential =
    match metadata.e_cred_authority with
    | Some "server" ->
       pcdata ""
    | _ ->
       div [
         a ~service:election_update_credential [pcdata "Update a credential"] (uuid, ());
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
          a ~service:election_regenpwd [pcdata "Regenerate and mail a password"] (uuid, ());
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
    div [state_div];
    div_archive;
    div_delete;
  ] in
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

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
          string_input ~name:old ~input_type:`Text ~a:[a_size 64] ();
        ];
        p [
          pcdata "New credential: ";
          string_input ~name:new_ ~input_type:`Text ~a:[a_size 617] ();
        ];
        p [string_input ~input_type:`Submit ~value:"Submit" ()];
      ]
    ) (uuid, ())
  in
  let content = [
    form;
  ] in
  let%lwt login_box = site_login_box () in
  base ~title:params.e_name ?login_box ~content ~uuid ()

let regenpwd uuid () =
  let form = post_form ~service:election_regenpwd_post
    (fun user ->
      [
        div [
          pcdata "Username: ";
          string_input ~name:user ~input_type:`Text ();
        ];
        div [string_input ~input_type:`Submit ~value:"Submit" ()];
      ]
    ) (uuid, ())
  in
  let content = [ form ] in
  let title = "Regenerate and mail password" in
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ~uuid ()

let cast_raw election () =
  let params = election.e_params in
  let uuid = params.e_uuid in
  let form_rawballot = post_form ~service:election_cast_post
    (fun (name, _) ->
      [
        div [pcdata "Please paste your encrypted ballot in JSON format in the following box:"];
        div [textarea ~a:[a_rows 10; a_cols 40] ~name ()];
        div [string_input ~input_type:`Submit ~value:"Submit" ()];
      ]
    ) (uuid, ())
  in
  let form_upload = post_form ~service:election_cast_post
    (fun (_, name) ->
      [
        div [pcdata "Alternatively, you can also upload a file containing your ballot:"];
        div [
          pcdata "File: ";
          file_input ~name ();
        ];
        div [string_input ~input_type:`Submit ~value:"Submit" ()];
      ]
    ) (uuid, ())
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
  let%lwt login_box = election_login_box uuid () in
  let%lwt footer = audit_footer election in
  base ~title:params.e_name ?login_box ~content ~uuid ~footer ()

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
          string_input
            ~a:[a_style "font-size: 20px; cursor: pointer;"]
            ~input_type:`Submit ~value:L.i_cast_my_vote ();
          pcdata ".";
        ]
      ]) (uuid, ())
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
      b [pcdata hash];
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
    | `Valid hash ->
       [pcdata L.has_been_accepted;
        pcdata " ";
        pcdata L.your_tracker_is;
        b [pcdata hash];
        pcdata ". ";
        pcdata L.you_can_check_its_presence;
        a ~service:election_pretty_ballots [pcdata L.ballot_box] (uuid, ());
        pcdata L.anytime_during_the_election;
        pcdata L.confirmation_email;
       ], L.thank_you_for_voting
    | `Error e ->
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
  let%lwt login_box = election_login_box uuid () in
  base ~title ?login_box ~content ~uuid ()

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
  let%lwt login_box = site_login_box () in
  base ~title ?login_box ~content ()

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
    div ~a:[a_id "input_private_key"] [
        div [
            p [pcdata "Please enter your private key:"];
            input
              ~a:[a_id "private_key"; a_size 80]
              ~input_type:`Text
              ();
          ];
        div [
            p [pcdata "Or load it from a file:"];
            input
              ~a:[a_id "private_key_file"]
              ~input_type:`File
              ();
          ];
      ];
    hr ();
    div [
      button
        ~a:[a_id "compute"]
        ~button_type:`Button
        [pcdata "Compute decryption factors"];
    ];
    div ~a:[a_id "pd_done"] [
      post_form
        ~service:election_tally_trustees_post
        (fun pd ->
          [
            div [
              textarea
                ~a:[a_rows 5; a_cols 40; a_id "pd"]
                ~name:pd
                ();
            ];
            div [string_input ~input_type:`Submit ~value:"Submit" ()];
          ]
        ) (uuid, ((), token));
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

let already_logged_in () =
  let title = "Already logged in" in
  let content = [
    div [
      pcdata "You are already logged in as an administrator or on another election. You have to ";
      a ~service:logout [pcdata "log out"] ();
      pcdata " first."];
  ] in
  base ~title ~content ()

let login_choose auth_systems service () =
  let auth_systems =
    auth_systems |>
    List.map (fun name ->
      a ~service:(service name) [pcdata name] ()
    ) |> list_join (pcdata ", ")
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
            th [label ~a:[a_for name] [pcdata field_name]];
            td [string_input ~a:[a_maxlength 50] ~input_type ~name ()];
          ]]
        ];
        div [
          string_input ~input_type:`Submit ~value:"Login" ();
        ]
      ]) ()
  in
  let content = [
    form;
  ] in
  base ~title ~content ()

let login_password () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let form = post_form ~service:password_post
    (fun (llogin, lpassword) ->
      [
        tablex [tbody [
          tr [
            th [label ~a:[a_for llogin] [pcdata L.username]];
            td [string_input ~a:[a_maxlength 50] ~input_type:`Text ~name:llogin ()];
          ];
          tr [
            th [label ~a:[a_for lpassword] [pcdata L.password]];
            td [string_input ~a:[a_maxlength 50] ~input_type:`Password ~name:lpassword ()];
          ];
        ]];
        div [
          string_input ~input_type:`Submit ~value:L.login ();
        ]
      ]) ()
  in
  let content = [
    form;
  ] in
  base ~title:L.password_login ~content ()

let dummy_uuid = uuid_of_raw_string "00000000-0000-0000-0000-000000000000"

let booth () =
  let%lwt language = Eliom_reference.get Web_state.language in
  let module L = (val Web_i18n.get_lang language) in
  let head = head (title (pcdata L.belenios_booth)) [
    link ~rel:[`Stylesheet] ~href:(static "booth.css") ();
    script ~a:[a_src (static "sjcl.js")] (pcdata "");
    script ~a:[a_src (static "jsbn.js")] (pcdata "");
    script ~a:[a_src (static "jsbn2.js")] (pcdata "");
    script ~a:[a_src (static "random.js")] (pcdata "");
    script ~a:[a_src (static "booth.js")] (pcdata "");
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
      div [button ~button_type:`Button ~a:[a_id "load_url"] [pcdata "Load URL"]];
      br ();
      pcdata "Load an election by giving its parameters:";
      div [unsafe_textarea "election_params" ""];
      div [button ~button_type:`Button ~a:[a_id "load_params"] [pcdata "Load parameters"]];
    ]
  in
  let text_choices = unsafe_textarea "choices" "" in
  let ballot_form =
    post_form ~a:[a_id "ballot_form"] ~service:election_cast_post
      (fun (encrypted_vote, _) -> [
        div ~a:[a_id "div_ballot"; a_style "display:none;"] [
          pcdata "Encrypted ballot:";
          div [
            textarea
              ~a:[a_id "ballot"; a_rows 1; a_cols 80; a_readonly `ReadOnly]
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
            string_input ~input_type:`Submit ~value:L.continue ~a:[a_style "font-size:30px;"] ();
          ];
        div ~a:[a_id "div_submit_manually"; a_style "display:none;"] [
            pcdata "You must submit your ballot manually.";
          ];
        br (); br ();
       ])
      (dummy_uuid, ())
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
