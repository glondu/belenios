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
open Belenios_api.Serializable_t
open Web_serializable_j
open Web_common
open Eliom_content.Html.F
open Eliom_content.Html.F.Form

module Make
    (Web_state : Web_state_sig.S)
    (Web_i18n : Web_i18n_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S)
    (Mails_admin : Belenios_ui.Mails_admin_sig.S) =
struct
  open Web_services
  open Pages_common

  let admin_background = " background: #FF9999;"
  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "admin"

  let privacy_notice cont =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      s_ "Election server" ^ " — " ^ s_ "Personal data processing notice"
    in
    let content =
      [
        div
          [
            txt (s_ "To use this site, you must accept our ");
            direct_a !Web_config.gdpr_uri (s_ "personal data policy");
            txt ".";
          ];
        post_form ~service:privacy_notice_accept
          (fun ncont ->
            [
              div
                [
                  input ~input_type:`Hidden ~name:ncont ~value:cont
                    (user string_of_privacy_cont);
                  input ~input_type:`Submit ~value:(s_ "Accept") string;
                ];
            ])
          ();
      ]
    in
    base ~title ~content ()

  let checkpriv_link l uuid =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    let uri_base =
      let service = Eliom_service.static_dir () in
      Eliom_uri.make_string_uri ~absolute:true ~service
        [ "static"; "checkpriv.html" ]
      |> rewrite_prefix
    in
    let uri =
      uri_base ^ "#" ^ Uuid.unwrap uuid |> Eliom_content.Xml.uri_of_string
    in
    Eliom_content.Html.F.Raw.a
      ~a:[ a_href uri ]
      [ txt @@ s_ "Check private key ownership" ]

  let login_box ?cont () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let style = "float: right; text-align: right;" ^ admin_background in
    let* user = Eliom_reference.get Web_state.site_user in
    let auth_systems =
      List.map (fun x -> x.auth_instance) !Web_config.site_auth_config
    in
    let cont = match cont with None -> ContSiteHome | Some x -> x in
    let cont = default_admin cont in
    let login service =
      Eliom_service.preapply ~service:site_login (Some service, cont)
    in
    let logout () = Eliom_service.preapply ~service:logout cont in
    let body =
      match user with
      | Some (_, account, _) ->
          [
            div
              [
                txt (s_ "Logged in as");
                txt " ";
                em [ a ~service:Web_services.account [ txt account.name ] () ];
                txt ".";
              ];
            div
              [
                a
                  ~a:[ a_id "logout" ]
                  ~service:(logout ())
                  [ txt (s_ "Log out") ]
                  ();
                txt ".";
              ];
          ]
      | None ->
          [
            div [ txt (s_ "Not logged in.") ];
            (let auth_systems =
               List.map
                 (fun name ->
                   a
                     ~a:[ a_id ("login_" ^ name) ]
                     ~service:(login name)
                     [ txt name ]
                     ())
                 auth_systems
               |> List.join (txt ", ")
             in
             div ([ txt (s_ "Log in:"); txt " [" ] @ auth_systems @ [ txt "]" ]));
          ]
    in
    return (div ~a:[ a_style style ] body)

  let admin_login get_handler =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let contact =
      match !Web_config.contact_uri with
      | None -> txt ""
      | Some uri ->
          div
            [
              txt (s_ "If you do not have any account, you may ");
              direct_a ~target:"_blank" uri (s_ "contact us");
              txt ".";
            ]
    in
    let* auth_div =
      match !Web_config.site_auth_config with
      | [] -> return @@ txt ""
      | { auth_instance = service; _ } :: others ->
          let* default = get_handler service in
          let default =
            match default with
            | Web_auth_sig.Html x ->
                div ~a:[ a_class [ "embedded-login-form" ] ] [ x ]
            | Web_auth_sig.Redirection _ ->
                div
                  [
                    txt (s_ "Log in with");
                    txt " ";
                    a ~service:site_login
                      [ txt service ]
                      (Some service, default_admin ContSiteAdmin);
                    txt ".";
                  ]
          in
          let others =
            List.map
              (fun { auth_instance = service; _ } ->
                div
                  [
                    txt (s_ "You can also log in with");
                    txt " ";
                    a ~service:site_login
                      [ txt service ]
                      (Some service, default_admin ContSiteAdmin);
                    txt ".";
                  ])
              others
          in
          return @@ div (default :: others)
    in
    let* body =
      let default =
        div
          [ txt (s_ "To administer an election, you need to log in."); contact ]
      in
      read_snippet ~default ~lang !Web_config.admin_home
    in
    let content = [ body; auth_div ] in
    let title = "Belenios" ^ " — " ^ s_ "Verifiable online voting platform" in
    let* login_box = login_box ~cont:ContSiteAdmin () in
    base ~title ~login_box ~content ()

  let try_new_ui l uuid =
    let@ () =
     fun cont -> if !Web_config.restricted_mode then txt "" else cont ()
    in
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    div
    @@
    let str, xml =
      let s_ = Belenios_ui.I18n.s_xml s_ in
      s_
        "<div><b>New:</b> try the <a>experimental interface</a> (more \
         ergonomic)</div>"
    in
    match xml with
    | Element ("div", [], xs) ->
        List.map
          (function
            | Xml_light_types.PCData x -> txt x
            | Element ("b", [], [ PCData x ]) ->
                span ~a:[ a_class [ "markup-b" ] ] [ txt x ]
            | Element ("a", [], [ PCData x ]) ->
                Eliom_content.Html.F.Raw.a
                  ~a:[ a_href (make_admin_new_uri uuid) ]
                  [ txt x ]
            | _ -> txt str)
          xs
    | _ -> [ txt str ]

  let admin ~elections =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let format_election (uuid, name) =
      let name = if name = "" then s_ "(no title)" else name in
      li
        [
          a ~service:election_admin
            ~a:[ a_id ("election_admin_" ^ Uuid.unwrap uuid) ]
            [ txt name ]
            uuid;
        ]
    in
    let format_draft_election (uuid, name) =
      let name = if name = "" then s_ "(no title)" else name in
      li
        [
          a ~service:election_draft
            ~a:[ a_id ("election_draft_" ^ Uuid.unwrap uuid) ]
            [ txt name ]
            uuid;
        ]
    in
    let title = s_ "Election server" ^ " — " ^ s_ "Administration" in
    match elections with
    | draft, elections, tallied, archived ->
        let draft =
          match draft with
          | [] -> p [ txt (s_ "You own no such elections!") ]
          | _ -> ul @@ List.map format_draft_election draft
        in
        let elections =
          match elections with
          | [] -> p [ txt (s_ "You own no such elections!") ]
          | _ -> ul @@ List.map format_election elections
        in
        let tallied =
          match tallied with
          | [] -> p [ txt (s_ "You own no such elections!") ]
          | _ -> ul @@ List.map format_election tallied
        in
        let archived =
          match archived with
          | [] -> p [ txt (s_ "You own no such elections!") ]
          | _ -> ul @@ List.map format_election archived
        in
        let prepare_new_election =
          if !Web_config.deny_newelection then
            div [ txt (s_ "New elections are not allowed on this server.") ]
          else
            div
              [
                a
                  ~a:[ a_id "prepare_new_election" ]
                  ~service:election_draft_pre
                  [ txt (s_ "Prepare a new election") ]
                  ();
              ]
        in
        let content =
          [
            div
              [
                prepare_new_election;
                div [ br () ];
                try_new_ui l None;
                div [ br () ];
                h2 [ txt (s_ "Elections being prepared") ];
                draft;
                div [ br () ];
                h2 [ txt (s_ "Elections you can administer") ];
                elections;
                div [ br () ];
                h2 [ txt (s_ "Tallied elections") ];
                tallied;
                div [ br () ];
                h2 [ txt (s_ "Archived elections") ];
                archived;
              ];
          ]
        in
        let* login_box = login_box () in
        let* lang_box = lang_box ContSiteAdmin in
        base ~lang_box ~title ~login_box ~content ()

  let new_election_failure reason () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Create new election" in
    let reason =
      match reason with
      | `Exists -> txt (s_ "An election with the same UUID already exists.")
      | `Exception e -> txt @@ Printexc.to_string e
    in
    let content =
      [ div [ p [ txt (s_ "The creation failed.") ]; p [ reason ] ] ]
    in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_pre () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Prepare a new election" in
    let form =
      post_form ~service:election_draft_new
        (fun (credmgmt, (auth, cas_server)) ->
          let auth_systems =
            !Web_config.exported_auth_config
            |> List.mapi (fun i x ->
                   let checked = i = 0 in
                   match x with
                   | `BuiltinPassword ->
                       div
                         [
                           label
                             [
                               radio ~checked ~name:auth ~value:"password"
                                 string;
                               txt " ";
                               txt
                                 (s_
                                    "sent in advance by e-mail (useful for \
                                     multiple elections)");
                             ];
                         ]
                   | `BuiltinCAS ->
                       div
                         [
                           label
                             [
                               radio ~checked ~name:auth ~value:"cas" string;
                               txt " ";
                               txt
                                 (s_
                                    "CAS (external authentication server, \
                                     offers better security guarantees when \
                                     applicable)");
                             ];
                           div
                             ~a:[ a_style "margin-left: 5em;" ]
                             [
                               txt (s_ "Server address:");
                               txt " ";
                               input ~input_type:`Text ~name:cas_server string;
                               txt " ";
                               txt
                                 (s_ "(for example: https://cas.inria.fr/cas)");
                             ];
                         ]
                   | `Export a ->
                       let legend =
                         match a.auth_system with
                         | "email" ->
                             [
                               txt
                                 (s_
                                    "sent by e-mail when voting (a short \
                                     password, renewed for each vote)");
                             ]
                         | _ ->
                             [
                               txt a.auth_instance;
                               txt " ";
                               txt (s_ "(imported from server)");
                             ]
                       in
                       div
                         [
                           label
                             (radio ~checked ~name:auth
                                ~value:("%" ^ a.auth_instance) string
                             :: txt " " :: legend);
                         ])
          in
          let auto_credentials, checked =
            if !Web_config.restricted_mode then (txt "", true)
            else
              ( div
                  [
                    label
                      [
                        radio ~checked:true ~name:credmgmt ~value:"auto" string;
                        txt " ";
                        txt
                          (s_
                             "sent by our server (easier mode but offers less \
                              security)");
                      ];
                  ],
                false )
          in
          [
            div
              [
                txt
                  (s_
                     "For a better control of eligibility, voters will be \
                      authenticated by two factors: credentials and passwords");
                txt " (";
                direct_a Belenios_ui.Links.setup (s_ "more info");
                txt ").";
              ];
            ol
              [
                li
                  [
                    txt (s_ "Credentials:");
                    auto_credentials;
                    div
                      [
                        label
                          [
                            radio ~checked ~name:credmgmt ~value:"manual" string;
                            txt " ";
                            txt
                              (s_
                                 "sent by a third party chosen by you (safe \
                                  mode)");
                          ];
                      ];
                  ];
                li (txt (s_ "Passwords:") :: auth_systems);
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Proceed") string ];
          ])
        ()
    in
    let content = [ form ] in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let preview_booth l uuid metadata =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    let hash =
      Netencoding.Url.mk_url_encoded_parameters
        [ ("uuid", Uuid.unwrap uuid); ("lang", lang); ("draft", "1") ]
    in
    match get_booth_index metadata.e_booth_version with
    | Some i ->
        let (Booth election_vote) = fst booths.(i) in
        let service =
          Eliom_uri.make_string_uri ~service:(election_vote ()) ~absolute:true
            ()
          |> rewrite_prefix
        in
        span [ direct_a (service ^ "#" ^ hash) (s_ "Preview booth") ]
    | None -> span [ txt @@ s_ "Unsupported booth version" ]

  let election_draft uuid (Draft (_, se) as fse) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      Printf.sprintf (f_ "Preparation of election %s") se.se_questions.t_name
    in
    let available_languages = List.map fst Belenios_ui.Languages.available in
    let form_languages =
      post_form ~service:election_draft_languages
        (fun languages ->
          [
            div
              [
                txt (s_ "Languages:");
                txt " ";
                input ~name:languages ~input_type:`Text
                  ~value:(string_of_languages se.se_metadata.e_languages)
                  string;
                txt " (";
                txt (s_ "Available languages:");
                txt " ";
                txt (string_of_languages (Some available_languages));
                txt ")";
              ];
            div
              [
                txt
                  (s_
                     "This is a space-separated list of languages that will be \
                      used in e-mails sent by the server.");
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Save changes") string ];
          ])
        uuid
    in
    let div_languages = div [ h2 [ txt (s_ "Languages") ]; form_languages ] in
    let form_description =
      post_form ~service:election_draft_description
        ~a:[ a_id "name_and_description_form" ]
        (fun (name, description) ->
          [
            div
              [
                txt (s_ "Name of the election:");
                txt " ";
                input ~name ~input_type:`Text ~value:se.se_questions.t_name
                  ~a:[ a_placeholder (s_ "Name of the election") ]
                  string;
              ];
            div
              [
                div [ txt (s_ "Description of the election:"); txt " " ];
                div
                  [
                    textarea ~name:description
                      ~a:
                        [
                          a_cols 80;
                          a_placeholder (s_ "Description of the election.");
                        ]
                      ~value:se.se_questions.t_description ();
                  ];
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Save changes") string ];
          ])
        uuid
    in
    let div_description =
      div
        [
          h2 [ txt (s_ "Name and description of the election") ];
          form_description;
        ]
    in
    let form_admin_name =
      post_form ~service:election_draft_admin_name
        ~a:[ a_id "form_admin_name" ]
        (fun name ->
          [
            div
              [
                txt (s_ "Public name of the administrator:");
                txt " ";
                (let value =
                   match se.se_administrator with Some x -> x | None -> ""
                 in
                 input ~name ~input_type:`Text ~value string);
              ];
            div
              [
                txt
                  (s_ "This name will be published on the election result page.");
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Save changes") string ];
          ])
        uuid
    in
    let div_admin_name =
      div
        [ h2 [ txt (s_ "Public name of the administrator") ]; form_admin_name ]
    in
    let form_contact =
      post_form ~service:election_draft_contact
        ~a:[ a_id "form_contact" ]
        (fun contact ->
          [
            div
              [
                txt (s_ "Contact:");
                txt " ";
                (let value =
                   match se.se_metadata.e_contact with
                   | Some x -> x
                   | None -> Web_defaults.contact
                 in
                 input ~name:contact ~input_type:`Text ~value
                   ~a:[ a_placeholder (s_ "Name <user@example.org>") ]
                   string);
              ];
            div
              [
                txt
                  (s_
                     "This contact will be added to e-mails sent to the voters.");
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Save changes") string ];
          ])
        uuid
    in
    let div_contact = div [ h2 [ txt (s_ "Contact") ]; form_contact ] in
    let auth =
      match se.se_metadata.e_auth_config with
      | Some [ { auth_system = "password"; _ } ] -> `Password
      | Some [ { auth_system = "dummy"; _ } ] -> `Dummy
      | Some
          [ { auth_system = "cas"; auth_config = [ ("server", server) ]; _ } ]
        ->
          `CAS server
      | Some [ { auth_system = "import"; auth_instance = name; _ } ] ->
          `Import name
      | _ -> failwith "unknown authentication scheme in election_draft"
    in
    let div_auth =
      div
        [
          h2 [ txt (s_ "Authentication") ];
          (match auth with
          | `Password ->
              div
                [
                  txt (s_ "Authentication scheme: password");
                  (if List.for_all (fun v -> v.sv_password <> None) se.se_voters
                   then div [ txt (s_ "All passwords have been sent!") ]
                   else
                     post_form ~service:election_draft_auth_genpwd
                       (fun () ->
                         [
                           input ~input_type:`Submit
                             ~value:(s_ "Generate and mail missing passwords")
                             string;
                         ])
                       uuid);
                ]
          | `Dummy -> div [ txt (s_ "Authentication scheme: dummy") ]
          | `CAS server ->
              div
                [
                  txt (s_ "Authentication scheme: CAS with server "); txt server;
                ]
          | `Import name ->
              div
                [
                  txt
                    (Printf.sprintf
                       (f_ "Authentication scheme: %s (imported from server)")
                       name);
                ]);
        ]
    in
    let div_questions =
      div
        [
          h2
            [
              a
                ~a:[ a_id "edit_questions" ]
                ~service:election_draft_questions
                [ txt (s_ "Edit questions") ]
                uuid;
            ];
          preview_booth l uuid se.se_metadata;
        ]
    in
    let div_voters =
      div
        [
          h2
            [
              a
                ~a:[ a_id "edit_voters" ]
                ~service:election_draft_voters
                [ txt (s_ "Edit voters") ]
                uuid;
            ];
          div
            [
              txt @@ string_of_int @@ List.length se.se_voters;
              txt (s_ " voter(s) registered");
            ];
        ]
    in
    let div_trustees =
      div
        [
          h2 [ txt (s_ "Trustees") ];
          div
            [
              txt
                (s_
                   "By default, the election server manages the keys of the \
                    election (degraded privacy mode).");
              txt " ";
              txt
                (s_
                   "For real elections, the key must be shared among \
                    independent trustees.");
              txt " ";
              txt (s_ "Click ");
              a ~service:election_draft_trustees [ txt (s_ "here") ] uuid;
              txt (s_ " to set up the election key.");
            ];
        ]
    in
    let cred_auth_is_server = se.se_metadata.e_cred_authority = Some "server" in
    let div_credentials =
      div
        [
          h2 [ txt (s_ "Credentials") ];
          (match Web_persist.get_credentials_status uuid fse with
          | `Done ->
              let div_private_creds =
                if cred_auth_is_server then
                  div
                    [
                      a ~service:election_draft_credentials_get
                        [ txt (s_ "Download private credentials") ]
                        uuid;
                    ]
                else txt ""
              in
              let div_edit_credential_authority_name =
                if cred_auth_is_server then txt ""
                else
                  div
                    [
                      a ~service:election_draft_credential_authority
                        [ txt (s_ "Edit credential authority name") ]
                        uuid;
                    ]
              in
              div
                [
                  div [ txt (s_ "Credentials have already been generated!") ];
                  div_edit_credential_authority_name;
                  div_private_creds;
                ]
          | `None ->
              div
                [
                  txt (s_ "Warning: this will freeze the voter list!");
                  (if cred_auth_is_server then
                     post_form ~service:election_draft_credentials_server
                       (fun () ->
                         [
                           input ~input_type:`Submit
                             ~value:(s_ "Generate on server") string;
                         ])
                       uuid
                   else
                     div
                       [
                         a ~service:election_draft_credential_authority
                           [ txt (s_ "Credential management") ]
                           uuid;
                       ]);
                ]
          | `Pending n ->
              div
                [
                  txt (s_ "Credentials are being generated on the server.");
                  txt " ";
                  Printf.ksprintf txt (f_ "Number of credentials left: %d.") n;
                ]);
        ]
    in
    let link_confirm =
      div
        [
          h2 [ txt (s_ "Validate creation") ];
          a ~service:election_draft_confirm [ txt (s_ "Create election") ] uuid;
        ]
    in
    let form_destroy =
      let t =
        Option.value se.se_creation_date ~default:Web_defaults.creation_date
      in
      let t = Period.add t (Period.day Web_defaults.days_to_delete) in
      post_form ~service:election_draft_destroy
        (fun () ->
          [
            div
              [
                h2 [ txt (s_ "Destroy election") ];
                div
                  [
                    txt
                      (s_
                         "Note: this election will be automatically destroyed \
                          after ");
                    txt (Datetime.format t);
                    txt ".";
                  ];
                input ~input_type:`Submit ~value:(s_ "Destroy election") string;
              ];
          ])
        uuid
    in
    let content =
      [
        try_new_ui l (Some uuid);
        hr ();
        div_description;
        hr ();
        div_admin_name;
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
      ]
    in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_trustees ?token uuid (Draft (_, se)) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      Printf.sprintf (f_ "Trustees for election %s") se.se_questions.t_name
    in
    let form_trustees_add =
      post_form ~service:election_draft_trustee_add
        (fun (n_id, n_comment) ->
          [
            txt (s_ "Trustee's e-mail address:");
            txt " ";
            input ~input_type:`Text ~name:n_id string;
            txt ", ";
            txt (s_ "public name:");
            txt " ";
            input ~input_type:`Text ~name:n_comment string;
            input ~input_type:`Submit ~value:(s_ "Add") string;
          ])
        uuid
    in
    let mk_form_trustee_del value =
      post_form ~service:election_draft_trustee_del
        (fun name ->
          [
            input ~input_type:`Hidden ~name ~value string;
            input ~input_type:`Submit ~value:(s_ "Remove") string;
          ])
        uuid
    in
    let langs = get_languages se.se_metadata.e_languages in
    let* trustees =
      match se.se_trustees with
      | `Basic x when x.dbp_trustees <> [] ->
          let ts = x.dbp_trustees in
          let* ts =
            Lwt_list.map_s
              (fun t ->
                let this_line =
                  match token with
                  | Some x when x = t.st_token -> true
                  | _ -> false
                in
                let uri =
                  compute_hash_link ~uuid ~token:t.st_token
                    ~service:election_draft_trustee_static
                in
                let* mail_cell, link_cell =
                  if t.st_token <> "" then
                    if t.st_public_key = "" then
                      let* subject, body =
                        Mails_admin.mail_trustee_generation_basic langs uri
                      in
                      let mail_cell =
                        a_mailto ~dest:t.st_id ~subject ~body (s_ "E-mail")
                      in
                      let link_cell =
                        if this_line then
                          a ~service:election_draft_trustees
                            [ txt (s_ "Hide link") ]
                            uuid
                        else
                          Raw.a
                            ~a:[ a_href (Xml.uri_of_string uri) ]
                            [ txt (s_ "Link") ]
                      in
                      return (mail_cell, link_cell)
                    else
                      let cell = txt (s_ "(done)") in
                      return (cell, cell)
                  else
                    let cell = txt (s_ "(server)") in
                    return (cell, cell)
                in
                let first_line =
                  tr
                    [
                      td [ txt t.st_id ];
                      td
                        [
                          (match t.st_name with
                          | None -> txt (s_ "(not available)")
                          | Some x -> txt x);
                        ];
                      td [ mail_cell ];
                      td [ link_cell ];
                      td
                        [
                          txt
                            (if t.st_public_key = "" then s_ "No" else s_ "Yes");
                        ];
                      td
                        [
                          (if t.st_id = "server" then
                             txt (s_ "(cannot be removed)")
                           else mk_form_trustee_del t.st_id);
                        ];
                    ]
                in
                let second_line =
                  if this_line then
                    [
                      tr
                        [
                          td
                            ~a:[ a_colspan 6 ]
                            [
                              txt (s_ "The link that must be sent to trustee ");
                              txt t.st_id;
                              txt (s_ " is:");
                              br ();
                              txt uri;
                            ];
                        ];
                    ]
                  else []
                in
                return (first_line :: second_line))
              ts
          in
          return
          @@ table
               (tr
                  [
                    th [ txt (s_ "Trustee") ];
                    th [ txt (s_ "Public name") ];
                    th [ txt (s_ "E-mail") ];
                    th [ txt (s_ "Link") ];
                    th [ txt (s_ "Done?") ];
                    th [ txt (s_ "Remove") ];
                  ]
               :: List.flatten ts)
      | _ -> return (txt "")
    in
    let import_link =
      div
        [
          a ~service:Web_services.election_draft_import_trustees
            [ txt (s_ "Import trustees from another election") ]
            uuid;
        ]
    in
    let div_trustees =
      match se.se_trustees with
      | `Basic x ->
          div
            [
              trustees;
              (if x.dbp_trustees <> [] then
                 div
                   [
                     txt
                       (s_
                          "There is one link per trustee. Send each trustee \
                           the respective link.");
                     br ();
                     br ();
                   ]
               else txt "");
              form_trustees_add;
            ]
      | `Threshold _ -> txt ""
    in
    let div_content =
      div
        [
          div
            [
              txt
                (s_
                   "To set up the election key, you need to nominate trustees. \
                    Each trustee will create a secret key.");
              txt " ";
              txt
                (s_
                   "To set up the election so that only a subset of trustees \
                    is needed, go to the ");
              a ~service:election_draft_threshold_trustees
                [ txt (s_ "threshold mode") ]
                uuid;
              txt ".";
            ];
          br ();
          div_trustees;
        ]
    in
    let back_link =
      div
        [
          a ~service:Web_services.election_draft
            [ txt (s_ "Go back to election draft") ]
            uuid;
        ]
    in
    let content = [ div_content; import_link; back_link ] in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_threshold_trustees ?token uuid (Draft (_, se)) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      Printf.sprintf (f_ "Trustees for election %s") se.se_questions.t_name
    in
    let dtp =
      match se.se_trustees with
      | `Basic _ ->
          {
            dtp_threshold = None;
            dtp_trustees = [];
            dtp_parameters = None;
            dtp_error = None;
          }
      | `Threshold x -> x
    in
    let show_add_remove = dtp.dtp_threshold = None in
    let form_trustees_add =
      if show_add_remove then
        post_form ~service:election_draft_threshold_trustee_add
          (fun (n_id, n_comment) ->
            [
              txt (s_ "Trustee's e-mail address:");
              txt " ";
              input ~input_type:`Text ~name:n_id string;
              txt ", ";
              txt (s_ "public name:");
              txt " ";
              input ~input_type:`Text ~name:n_comment string;
              input ~input_type:`Submit ~value:(s_ "Add") string;
            ])
          uuid
      else txt ""
    in
    let mk_form_trustee_del value =
      post_form ~service:election_draft_threshold_trustee_del
        (fun name ->
          [
            input ~input_type:`Hidden ~name ~value string;
            input ~input_type:`Submit ~value:(s_ "Remove") string;
          ])
        uuid
    in
    let langs = get_languages se.se_metadata.e_languages in
    let* trustees =
      match dtp.dtp_trustees with
      | [] -> return (txt "")
      | ts ->
          let* ts =
            Lwt_list.map_s
              (fun t ->
                let this_line =
                  match token with
                  | Some x when x = t.stt_token -> true
                  | _ -> false
                in
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
                let uri =
                  compute_hash_link ~uuid ~token:t.stt_token
                    ~service:election_draft_threshold_trustee_static
                in
                let* mail_cell =
                  let* subject, body =
                    Mails_admin.mail_trustee_generation_threshold langs uri
                  in
                  return (a_mailto ~dest:t.stt_id ~subject ~body (s_ "E-mail"))
                in
                let first_line =
                  tr
                    ([
                       td [ txt t.stt_id ];
                       td
                         [
                           (match t.stt_name with
                           | None -> txt (s_ "(not available)")
                           | Some x -> txt x);
                         ];
                       td [ mail_cell ];
                       td
                         [
                           (if this_line then
                              a ~service:election_draft_threshold_trustees
                                [ txt (s_ "Hide link") ]
                                uuid
                            else
                              Raw.a
                                ~a:[ a_href (Xml.uri_of_string uri) ]
                                [ txt (s_ "Link") ]);
                         ];
                       td [ txt state ];
                     ]
                    @
                    if show_add_remove then
                      [ td [ mk_form_trustee_del t.stt_id ] ]
                    else [])
                in
                let second_line =
                  if this_line then
                    [
                      tr
                        [
                          td
                            ~a:[ a_colspan (if show_add_remove then 6 else 5) ]
                            [
                              txt (s_ "The link that must be sent to trustee ");
                              txt t.stt_id;
                              txt (s_ " is:");
                              br ();
                              txt uri;
                            ];
                        ];
                    ]
                  else []
                in
                return (first_line :: second_line))
              ts
          in
          return
          @@ div
               [
                 table
                   (tr
                      ([
                         th [ txt (s_ "Trustee") ];
                         th [ txt (s_ "Public name") ];
                         th [ txt (s_ "E-mail") ];
                         th [ txt (s_ "Link") ];
                         th [ txt (s_ "State") ];
                       ]
                      @
                      if show_add_remove then [ th [ txt (s_ "Remove") ] ]
                      else [])
                   :: List.flatten ts);
                 div
                   [
                     txt (s_ "Meaning of states:");
                     ul
                       [
                         li
                           [
                             txt
                               (s_ "init: administrator needs to set threshold");
                           ];
                         li
                           [
                             txt
                               (s_
                                  "1a: action needed from trustee: generate \
                                   private key");
                           ];
                         li
                           [
                             txt
                               (s_
                                  "2a, 3a: action needed from trustee: enter \
                                   private key");
                           ];
                         li
                           [ txt (s_ "1b, 2b, 3b: waiting for other trustees") ];
                         li
                           [
                             txt
                               (s_
                                  "done: the key establishment protocol is \
                                   finished");
                           ];
                       ];
                   ];
                 br ();
               ]
    in
    let form_threshold, form_reset =
      match dtp.dtp_trustees with
      | [] -> (txt "", txt "")
      | ts -> (
          match dtp.dtp_threshold with
          | None ->
              ( post_form ~service:election_draft_threshold_set
                  (fun name ->
                    [
                      txt (s_ "Threshold:");
                      txt " ";
                      input ~input_type:`Text ~name int;
                      input ~input_type:`Submit ~value:(s_ "Set") string;
                      txt " ";
                      txt
                        (s_
                           "(the threshold must be smaller than the number of \
                            trustees)");
                    ])
                  uuid,
                txt "" )
          | Some i ->
              ( div
                  [
                    txt (string_of_int i);
                    txt (s_ " out of ");
                    txt (string_of_int (List.length ts));
                    txt (s_ " trustees will be needed to decrypt the result.");
                  ],
                post_form ~service:election_draft_threshold_set
                  (fun name ->
                    [
                      input ~input_type:`Hidden ~name ~value:0 int;
                      input ~input_type:`Submit ~value:(s_ "Reset threshold")
                        string;
                    ])
                  uuid ))
    in
    let maybe_error =
      match dtp.dtp_error with
      | None -> txt ""
      | Some e -> div [ b [ txt "Error: " ]; txt e; br (); br () ]
    in
    let div_content =
      div
        [
          div
            [
              txt
                (s_
                   "On this page, you can configure a group of trustees so \
                    that only a subset of them is needed to perform the \
                    decryption.");
            ];
          br ();
          form_threshold;
          br ();
          trustees;
          (if dtp.dtp_trustees <> [] then
             div
               [
                 txt
                   (s_
                      "There is one link per trustee. Send a link to each \
                       trustee.");
                 br ();
                 br ();
                 maybe_error;
               ]
           else txt "");
          form_trustees_add;
          form_reset;
        ]
    in
    let back_link =
      div
        [
          a ~service:Web_services.election_draft
            [ txt (s_ "Go back to election draft") ]
            uuid;
        ]
    in
    let content = [ div_content; br (); back_link ] in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_credential_authority uuid (Draft (_, se)) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      Printf.sprintf (f_ "Credentials for election %s") se.se_questions.t_name
    in
    let public_name_form =
      post_form ~service:election_draft_set_credential_authority
        (fun name ->
          let value =
            match se.se_metadata.e_cred_authority with
            | Some x -> x
            | None -> ""
          in
          [
            txt (s_ "Public name of the credential authority:");
            txt " ";
            input ~input_type:`Text ~name ~value string;
            input ~input_type:`Submit ~value:(s_ "Set") string;
          ])
        uuid
    in
    let back =
      div
        [
          a ~service:Web_services.election_draft
            [ txt (s_ "Back to election preparation page") ]
            uuid;
        ]
    in
    let url =
      compute_hash_link ~uuid ~token:se.se_public_creds
        ~service:election_draft_credentials_static
    in
    let content =
      [
        back;
        public_name_form;
        div
          [
            (let subject, body = Mails_admin.mail_credential_authority l url in
             a_mailto ~subject ~body
               (s_ "Send instructions to the credential authority"));
          ];
        div
          [
            txt
              (s_
                 "Alternatively, you can send the credential authority the \
                  following link:");
          ];
        ul
          [
            li
              [
                Raw.a
                  ~a:
                    [
                      a_id "credential_authority_link";
                      a_href (Xml.uri_of_string url);
                    ]
                  [ txt url ];
              ];
          ];
        div
          [
            txt
              (s_
                 "Note that this authority will personally have to send each \
                  credential to its respective voter.");
          ];
      ]
    in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_credentials_done (Draft (_, se)) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      Printf.sprintf (f_ "Credentials for election %s") se.se_questions.t_name
    in
    let content =
      [
        div [ txt (s_ "Credentials have been received and checked!") ];
        div
          [
            div [ b [ txt (s_ "Instructions") ] ];
            div
              [
                txt (s_ "Once the election is open, check that:");
                ol
                  [
                    li
                      [
                        txt
                          (s_
                             "the number of voters is correct, and the \
                              fingerprint of the voter list matches what has \
                              been saved;");
                      ];
                    li
                      [
                        txt
                          (s_
                             "the fingerprint of public credentials matches \
                              what has been saved;");
                      ];
                    li
                      [
                        txt
                          (s_
                             "you can send the private credential back to its \
                              rightful owner in case it gets lost.");
                      ];
                  ];
              ];
            div
              [
                txt
                  (s_
                     "Once the election is over, the file creds.txt must be \
                      destroyed.");
              ];
          ];
      ]
    in
    base ~title ~content ()

  let script_with_lang ~lang file =
    let file = static file in
    let dir = Filename.dirname (string_of_uri file) in
    div
      [
        Printf.ksprintf Unsafe.data
          "<script>var belenios_lang = %S; var belenios_dir = %S;</script>" lang
          (dir ^ "/");
        script ~a:[ a_src file ] (txt "");
      ]

  let election_draft_questions uuid (Draft (v, se) as fse) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      Printf.sprintf (f_ "Questions for election %s") se.se_questions.t_name
    in
    let booth_version =
      match se.se_metadata.e_booth_version with None -> 1 | Some v -> v
    in
    let form =
      let open (val Election.get_serializers v) in
      let value = string_of_template write_question se.se_questions in
      post_form ~service:election_draft_questions_post
        (fun (nquestions, nbooth) ->
          [
            div [ txt (s_ "Questions:") ];
            div
              [
                textarea
                  ~a:[ a_id "questions"; a_rows 5; a_cols 80 ]
                  ~name:nquestions ~value ();
              ];
            div
              [
                input ~input_type:`Text
                  ~a:[ a_id "booth_version" ]
                  ~name:nbooth ~value:booth_version int;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Save changes") string ];
          ])
        uuid
    in
    let allow_nh =
      match
        Belenios.Election.has_nh_questions (Template (v, se.se_questions))
      with
      | true -> true
      | false -> not (Web_persist.is_group_fixed uuid fse)
    in
    let hybrid_box =
      let@ () =
       fun cont -> if !Web_config.restricted_mode then txt "" else cont ()
      in
      div
        ~a:[ a_class [ "hybrid_box" ] ]
        [
          div
            [
              txt
                (s_ "Alternative voting methods (warning, still experimental):");
            ];
          div
            [
              txt
                (s_
                   "You may wish voters to rank candidates or give each \
                    candidate a score.");
              txt " ";
              txt
                (s_
                   "This allows deciding the winner according to your favorite \
                    counting method.");
              txt " ";
              txt
                (s_
                   "Our platform currently supports Condorcet, STV and \
                    majority judgment, but you may also apply your own method \
                    on the raw result (shuffled list of ballots).");
            ];
          div [ txt (s_ "Note that:") ];
          ol
            [
              li
                [
                  txt
                    (s_ "the after-the-vote procedure will require more steps;");
                ];
              li
                [
                  txt
                    (s_
                       "the voting interface will depend on the selected \
                        counting method. In some cases, you should explain to \
                        voters (e.g. in the question field) how they are \
                        supposed to express their vote.");
                  txt " ";
                  direct_a Belenios_ui.Links.mixnet (s_ "More information.");
                ];
            ];
          div
            [
              label
                [
                  input ~a:[ a_id "hybrid_mode" ] ~input_type:`Checkbox string;
                  txt (s_ "Tick the box to activate this mode.");
                ];
            ];
        ]
    in
    let interactivity =
      div
        ~a:[ a_id "interactivity" ]
        [
          script (Printf.ksprintf txt "var allow_nh = %b;" allow_nh);
          script_with_lang ~lang "tool_js_questions.js";
          hybrid_box;
        ]
    in
    let back =
      div
        [
          a ~service:Web_services.election_draft
            [ txt (s_ "Go back to election draft") ]
            uuid;
        ]
    in
    let content = [ back; interactivity; form ] in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_voters uuid (Draft (_, se) as fse) maxvoters () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      Printf.sprintf (f_ "Voters for election %s") se.se_questions.t_name
    in
    let tooltip =
      div
        ~a:
          [
            a_class [ "tooltip" ];
            a_style "position:relative;display:inline-block;";
          ]
        [
          div
            ~a:
              [
                a_style
                  "display:inline-block;width:3ex;line-height:3ex;text-align:center;border-radius:1.5ex;background-color:black;color:white;";
              ]
            [ txt "?" ];
          div
            ~a:
              [
                a_class [ "tooltiptext" ];
                a_style
                  "position:absolute;left:3ex;line-height:2.5ex;border:1px \
                   solid \
                   black;font-size:90%;display:inline-block;width:400px;padding:3px;background-color:#eee;";
              ]
            [
              txt
                (s_
                   "An identity is either \"address\", or \
                    \"address,username\", or \"address,username,weight\", or \
                    \"address,,weight\" where \"address\" is an e-mail \
                    address, \"username\" the associated user name for \
                    authentication, and \"weight\" is the number of votes of \
                    the voter (in case voters don't have all the same number \
                    of votes).");
            ];
        ]
    in
    let form =
      let placeholder =
        "bart.simpson@example.com              # " ^ s_ "typical use"
        ^ "\nalbert.einstein@example.com,albert_e  # "
        ^ s_ "when a login is needed, e.g. CAS"
        ^ "\nasterix.legaulois@example.com,,2      # "
        ^ s_ "when some voters have several votes"
      in
      post_form ~service:election_draft_voters_add
        (fun name ->
          [
            div
              [
                textarea
                  ~a:
                    [
                      a_style "vertical-align:top";
                      a_placeholder placeholder;
                      a_rows 20;
                      a_cols 80;
                    ]
                  ~name ();
                tooltip;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Add") string ];
          ])
        uuid
    in
    let mk_remove_button voter =
      let _, value, _ = Voter.get voter in
      post_form ~service:election_draft_voters_remove
        (fun name ->
          [
            input ~input_type:`Hidden ~name ~value string;
            input ~input_type:`Submit ~value:(s_ "Remove") string;
          ])
        uuid
    in
    let remove_all_button =
      match Web_persist.get_credentials_status uuid fse with
      | `Done | `Pending _ -> div []
      | `None ->
          post_form ~service:election_draft_voters_remove_all
            (fun () ->
              [ input ~input_type:`Submit ~value:(s_ "Remove all") string ])
            uuid
    in
    let has_passwords =
      match se.se_metadata.e_auth_config with
      | Some [ { auth_system = "password"; _ } ] -> true
      | _ -> false
    in
    let mk_regen_passwd voter =
      let _, value, _ = Voter.get voter in
      post_form ~service:election_draft_voters_passwd
        ~a:[ a_style "display: inline;" ]
        (fun name ->
          [
            input ~input_type:`Hidden ~name ~value string;
            input ~input_type:`Submit ~value:(s_ "Send again") string;
          ])
        uuid
    in
    let format_password_cell x =
      match x.sv_password with
      | Some _ -> [ txt (s_ "Yes"); txt " "; mk_regen_passwd x.sv_id ]
      | None -> [ txt (s_ "No") ]
    in
    let voters =
      List.map
        (fun v ->
          tr
            ([ td [ txt @@ Voter.to_string v.sv_id ] ]
            @ (if has_passwords then [ td (format_password_cell v) ] else [])
            @
            match Web_persist.get_credentials_status uuid fse with
            | `Done | `Pending _ -> []
            | `None -> [ td [ mk_remove_button v.sv_id ] ]))
        se.se_voters
    in
    let form_passwords =
      if has_passwords then
        post_form ~service:election_draft_auth_genpwd
          (fun () ->
            [
              input ~input_type:`Submit
                ~value:(s_ "Generate and mail missing passwords")
                string;
            ])
          uuid
      else txt ""
    in
    let voters =
      match voters with
      | [] -> div [ txt (s_ "No voters") ]
      | _ :: _ ->
          div
            [
              form_passwords;
              br ();
              table
                (tr
                   ([ th [ txt (s_ "Identity") ] ]
                   @ (if has_passwords then [ th [ txt (s_ "Password sent?") ] ]
                      else [])
                   @
                   match Web_persist.get_credentials_status uuid fse with
                   | `Done | `Pending _ -> []
                   | `None -> [ th [ txt (s_ "Remove") ] ])
                :: voters);
              remove_all_button;
            ]
    in
    let back =
      div
        [
          a ~service:Web_services.election_draft
            [ txt (s_ "Go back to election draft") ]
            uuid;
        ]
    in
    let div_add =
      match Web_persist.get_credentials_status uuid fse with
      | `Done | `Pending _ -> txt ""
      | `None ->
          div
            [
              div
                [
                  txt
                    (s_
                       "Please enter the identities of voters to add, one per \
                        line");
                  txt " (max ";
                  txt (string_of_int maxvoters);
                  txt ").";
                ];
              form;
            ]
    in
    let warning =
      div
        [
          div
            ~a:[ a_style "text-align:center;font-size:120%;" ]
            [
              b [ txt (s_ "Warning:") ];
              txt " ";
              txt
                (s_ "you have to make sure that the e-mail addresses are valid.");
            ];
          div
            ~a:
              [
                a_style
                  "text-align:center;font-style:italic;width:80%;margin-left:auto;margin-right:auto;";
              ]
            [
              txt
                (s_
                   "You won't be able to change the e-mail addresses once the \
                    credentials are created. Voters with invalid e-mail \
                    addresses won't be able to vote.");
            ];
        ]
    in
    let div_import =
      div
        [
          a ~service:election_draft_import
            [ txt (s_ "Import voters from another election") ]
            uuid;
        ]
    in
    let content = [ back; div_import; br (); warning; voters; div_add ] in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_credentials_already_generated () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Credential generation" in
    let content =
      [ div [ txt (s_ "Credentials have already been generated!") ] ]
    in
    base ~title ~content ()

  let election_draft_credentials_static () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Credential generation" in
    let div_link =
      div
        [
          txt (s_ "The link to the election will be:");
          ul [ li [ span ~a:[ a_id "election_url" ] [] ] ];
        ]
    in
    let uuid = Uuid.wrap "XXXXXXXXXXXXXX" and token = "XXXXXXXXXXXXXX" in
    let form_textarea =
      post_form
        ~a:[ a_id "submit_form"; a_style "display:none;" ]
        ~service:election_draft_credentials_post
        (fun name ->
          [
            div
              [
                div [ txt (s_ "Public credentials:") ];
                div [ textarea ~a:[ a_id "pks"; a_rows 5; a_cols 40 ] ~name () ];
                div
                  [
                    txt (s_ "Fingerprint of public credentials:");
                    txt " ";
                    span ~a:[ a_id "public_creds_fp" ] [];
                  ];
                div
                  [
                    b [ txt (s_ "Instructions:") ];
                    ol
                      [
                        li
                          [
                            txt (s_ "Download ");
                            raw_a ~service:home
                              ~a:[ a_id "creds" ]
                              [ txt (s_ "private credentials") ]
                              ();
                            txt (s_ " and save the file to a secure location.");
                            br ();
                            txt
                              (s_
                                 "You will use it to send credentials to \
                                  voters.");
                          ];
                        li
                          [
                            txt (s_ "Download ");
                            raw_a ~service:home
                              ~a:[ a_id "voters_txt" ]
                              [ txt (s_ "voter list") ]
                              ();
                            txt ".";
                            br ();
                            txt
                              (s_
                                 "This list must be the one approved by the \
                                  election commission.");
                          ];
                        li
                          [
                            txt (s_ "Save the two fingerprints above.");
                            br ();
                            txt
                              (s_
                                 "Once the election is open, you must check \
                                  that they match with what is published by \
                                  the server.");
                          ];
                        li
                          [
                            txt
                              (s_
                                 "Submit public credentials using the button \
                                  below.");
                          ];
                      ];
                  ];
                div
                  [
                    input ~input_type:`Submit
                      ~value:(s_ "Submit public credentials")
                      string;
                  ];
              ];
          ])
        (uuid, token)
    in
    let disclaimer =
      p
        [
          b [ txt (s_ "Note:") ];
          txt " ";
          txt
            (s_
               "submitting a large number of credentials using the above form \
                may fail; in this case, you have to use the command-line tool \
                and the form below.");
        ]
    in
    let form_file =
      post_form
        ~a:[ a_id "submit_form_file" ]
        ~service:election_draft_credentials_post_file
        (fun name ->
          [
            div
              [
                h2 [ txt (s_ "Submit by file") ];
                div
                  [
                    txt
                      (s_
                         "Use this form to upload public credentials generated \
                          with the command-line tool.");
                  ];
                div [ file_input ~name () ];
                div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
              ];
          ])
        (uuid, token)
    in
    let voters =
      let hash = span ~a:[ a_id "voters_hash" ] [] in
      div
        [
          div [ txt (s_ "Voter list:") ];
          div [ raw_textarea ~rows:5 ~cols:40 "voters" "" ];
          div [ txt (s_ "Fingerprint of voters:"); txt " "; hash ];
        ]
    in
    let interactivity =
      div
        ~a:[ a_id "interactivity" ]
        [ script_with_lang ~lang "tool_js_credgen.js" ]
    in
    let div_textarea =
      div [ voters; interactivity; form_textarea; disclaimer ]
    in
    let content =
      div
        ~a:[ a_id "initially_hidden_content"; a_style "display: none;" ]
        [ div_link; div_textarea; form_file ]
    in
    base ~title ~content:[ content ] ~static:true ()

  let election_draft_trustee_static () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Trustee key generation" in
    let div_link =
      div
        [
          txt (s_ "The link to the election will be:");
          ul [ li [ span ~a:[ a_id "election_url" ] [] ] ];
        ]
    in
    let form =
      let uuid = Uuid.wrap "XXXXXXXXXXXXXX" and token = "XXXXXXXXXXXXXX" in
      let service =
        Eliom_service.preapply ~service:election_draft_trustee_post (uuid, token)
      in
      post_form
        ~a:[ a_id "data_form" ]
        ~service
        (fun name ->
          [
            div
              ~a:[ a_id "submit_form"; a_style "display:none;" ]
              [
                div [ txt (s_ "Public key:") ];
                div [ textarea ~a:[ a_rows 5; a_cols 40; a_id "pk" ] ~name () ];
                div
                  [
                    txt (s_ "Fingerprint of the verification key:");
                    txt " ";
                    span ~a:[ a_id "public_key_fp" ] [];
                  ];
                div
                  [
                    b [ txt (s_ "Instructions:") ];
                    ol
                      [
                        li
                          [
                            txt (s_ "Download your ");
                            raw_a ~service:home
                              ~a:[ a_id "private_key" ]
                              [ txt (s_ "private key") ]
                              ();
                            txt (s_ " and save it to a secure location.");
                            br ();
                            txt
                              (s_ "You will use it to decrypt the final result.");
                          ];
                        li
                          [
                            txt (s_ "Save the fingerprint above.");
                            br ();
                            txt
                              (s_
                                 "Once the election is open, you must check \
                                  that it is present in the set of public keys \
                                  published by the server.");
                          ];
                        li
                          [
                            txt
                              (s_
                                 "Submit your public key using the button \
                                  below.");
                          ];
                      ];
                  ];
                div
                  [
                    input ~input_type:`Submit ~value:(s_ "Submit public key")
                      string;
                  ];
              ];
          ])
        ()
    in
    let interactivity =
      div
        ~a:[ a_id "interactivity" ]
        [ script_with_lang ~lang "tool_js_tkeygen.js" ]
    in
    let content =
      div
        ~a:[ a_id "initially_hidden_content"; a_style "display: none;" ]
        [ div_link; interactivity; form ]
    in
    base ~title ~content:[ content ] ~static:true ()

  let election_draft_threshold_trustee_static () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Trustee key generation" in
    let header =
      div
        ~a:[ a_style "text-align:center;" ]
        [
          h2 [ txt (s_ "Collaborative key generation") ];
          div ~a:[ a_id "current_step" ] [ txt (s_ "Step 0/3") ];
        ]
    in
    let div_link =
      div
        [
          txt (s_ "The link to the election will be:");
          ul [ li [ span ~a:[ a_id "election_url" ] [] ] ];
        ]
    in
    let uuid = Uuid.wrap "XXXXXXXXXXXXXX" and token = "XXXXXXXXXXXXXX" in
    let form =
      post_form ~service:election_draft_threshold_trustee_post
        ~a:[ a_id "data_form" ]
        (fun data ->
          [
            div
              ~a:[ a_id "key_helper"; a_style "display:none;" ]
              [
                b [ txt (s_ "Instructions:") ];
                ol
                  [
                    li
                      [
                        txt (s_ "Download your ");
                        raw_a ~service:home
                          ~a:[ a_id "private_key" ]
                          [ txt (s_ "private key") ]
                          ();
                        txt (s_ " and save it to a secure location.");
                        br ();
                        txt
                          (s_
                             "You will use it in the next steps and to decrypt \
                              the final result.");
                      ];
                    li
                      [
                        txt (s_ "The fingerprint of your public key is ");
                        span ~a:[ a_id "pki_fp" ] [];
                        txt
                          (s_
                             ". Save it so that you can check that it appears \
                              on the election home later.");
                      ];
                    li
                      [
                        txt (s_ "Submit data using the following button:");
                        txt " ";
                        input ~input_type:`Submit ~value:(s_ "Submit") string;
                        txt ".";
                        div
                          [
                            txt (s_ "Data:");
                            txt " ";
                            textarea
                              ~a:[ a_id "data"; a_rows 5; a_cols 40 ]
                              ~name:data ();
                          ];
                      ];
                  ];
              ];
          ])
        (uuid, token)
    in
    let form_compute =
      div
        ~a:[ a_id "compute_form"; a_style "display: none;" ]
        [
          b [ txt (s_ "Instructions:") ];
          ol
            [
              li
                [
                  txt (s_ "Enter your private key:");
                  txt " ";
                  input ~input_type:`Text
                    ~a:[ a_id "compute_private_key" ]
                    string;
                  txt " ";
                  button_no_value
                    ~a:[ a_id "compute_button" ]
                    ~button_type:`Button
                    [ txt (s_ "Proceed") ];
                ];
              li
                [
                  txt (s_ "Submit data using the following button:");
                  post_form
                    ~a:[ a_id "data_form_compute" ]
                    ~service:election_draft_threshold_trustee_post
                    (fun data ->
                      [
                        input ~input_type:`Submit ~value:(s_ "Submit") string;
                        div
                          [
                            txt (s_ "Data:");
                            txt " ";
                            textarea
                              ~a:[ a_id "compute_data"; a_rows 5; a_cols 40 ]
                              ~name:data ();
                          ];
                      ])
                    (uuid, token);
                ];
            ];
        ]
    in
    let interactivity =
      div
        ~a:[ a_id "interactivity" ]
        [ script_with_lang ~lang "tool_js_ttkeygen.js" ]
    in
    let div_instructions =
      div
        ~a:[ a_id "div_instructions"; a_style "display: none;" ]
        [
          b [ txt (s_ "Instructions") ];
          ol
            [
              li [ txt (s_ "Save the fingerprint above.") ];
              li
                [
                  txt
                    (s_
                       "Once the election is open, you must check that it is \
                        present in the set of verification keys published by \
                        the server.");
                ];
              li
                [
                  txt
                    (s_
                       "Remember that you must also check the presence of your \
                        public key.");
                ];
              li
                [
                  txt
                    (s_
                       "Remember to store you private key in a secure location.");
                ];
            ];
        ]
    in
    let content =
      div
        ~a:[ a_id "initially_hidden_content"; a_style "display: none;" ]
        [
          header;
          div_link;
          br ();
          div ~a:[ a_id "explain" ] [];
          interactivity;
          form;
          form_compute;
          div_instructions;
        ]
    in
    base ~title ~content:[ content ] ~static:true ()

  let election_draft_importer l ~service ~title ~note uuid
      (elections, tallied, archived) =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    let format_election (from_uuid, name) =
      let from_uuid = Uuid.unwrap from_uuid in
      let form =
        post_form ~service
          (fun from ->
            [
              div [ txt name; txt " ("; txt from_uuid; txt ")" ];
              div
                [
                  input ~input_type:`Hidden ~name:from ~value:from_uuid string;
                  input ~input_type:`Submit
                    ~value:(s_ "Import from this election")
                    string;
                ];
            ])
          uuid
      in
      li [ form ]
    in
    let itemize xs =
      match xs with
      | [] -> p [ txt (s_ "You own no such elections!") ]
      | _ -> ul @@ List.map format_election xs
    in
    let content =
      [
        b [ txt (s_ "Note:") ];
        txt " ";
        txt note;
        h2 [ txt (s_ "Elections you can administer") ];
        itemize elections;
        h2 [ txt (s_ "Tallied elections") ];
        itemize tallied;
        h2 [ txt (s_ "Archived elections") ];
        itemize archived;
      ]
    in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_draft_import uuid (Draft (_, se)) elections () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      s_ "Election " ^ se.se_questions.t_name ^ " — "
      ^ s_ "Import voters from another election"
    in
    let note =
      s_
        "Imported voters will have the same password as in the original \
         election, and no new e-mail will be sent."
    in
    let service = election_draft_import_post in
    election_draft_importer l ~service ~title ~note uuid elections

  let election_draft_import_trustees uuid (Draft (_, se)) elections () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title =
      s_ "Election " ^ se.se_questions.t_name ^ " — "
      ^ s_ "Import trustees from another election"
    in
    let note =
      s_
        "Imported trustees will have the same keys as in the original election."
    in
    let service = election_draft_import_trustees_post in
    election_draft_importer l ~service ~title ~note uuid elections

  let election_draft_confirm s uuid (Draft (v, se) as fse) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let notok x = span ~a:[ a_style "color: red;" ] [ txt x ] in
    let ok x = txt x in
    let title =
      s_ "Election " ^ se.se_questions.t_name ^ " — " ^ s_ "Validate creation"
    in
    let* s = Api_drafts.get_draft_status s uuid fse in
    let ready = true in
    let ready, name =
      if se.se_questions.t_name = Web_defaults.name then
        (false, notok (s_ "Not edited"))
      else (ready, ok "OK")
    in
    let ready, description =
      if se.se_questions.t_description = Web_defaults.description then
        (false, notok (s_ "Not edited"))
      else (ready, ok "OK")
    in
    let ready, admin_name =
      if se.se_administrator = None then (false, notok (s_ "Missing"))
      else (ready, ok "OK")
    in
    let ready, questions =
      if
        Belenios.Election.get_questions (Template (v, se.se_questions))
        = Web_defaults.questions
      then (false, notok (s_ "Not edited"))
      else (ready, ok "OK")
    in
    let ready, voters =
      match s.num_voters with
      | 0 -> (false, notok "Missing")
      | n -> (ready, ok (Printf.sprintf (f_ "%d voter(s)") n))
    in
    let ready, passwords =
      match s.passwords_ready with
      | Some true -> (ready, ok "OK")
      | Some false -> (false, notok (s_ "Missing"))
      | None -> (ready, ok (s_ "Not applicable"))
    in
    let ready, credential_authority =
      match se.se_metadata.e_cred_authority with
      | None | Some "" -> (false, notok (s_ "Missing"))
      | Some _ -> (ready, ok "OK")
    in
    let ready, credentials =
      match s.credentials_ready with
      | true -> (ready, ok "OK")
      | false -> (false, notok (s_ "Missing"))
    in
    let private_creds_base =
      match s.credentials_ready with
      | true ->
          [
            txt " ";
            txt (s_ "Please ");
            a ~service:election_draft_credentials_get
              [ txt (s_ "download") ]
              uuid;
            txt (s_ " and save them in a secure location.");
          ]
      | false -> []
    in
    let private_creds =
      match s.private_credentials_downloaded with
      | Some true -> ok "OK"
      | Some false -> span (notok (s_ "Not downloaded.") :: private_creds_base)
      | None -> ok (s_ "Not applicable")
    in
    let ready, trustees =
      match s.trustees_ready with
      | true -> (ready, ok "OK")
      | false -> (false, notok (s_ "Missing"))
    in
    let ready, nh_and_weights =
      match s.nh_and_weights_compatible with
      | true -> (ready, [])
      | false ->
          ( false,
            [
              tr
                [
                  td [ txt (s_ "Compatibility of weights with questions?") ];
                  td
                    [
                      notok
                        (s_
                           "Alternative questions cannot be combined with \
                            weights.");
                    ];
                ];
            ] )
    in
    let ready, restricted_mode =
      match s.restricted_mode_error with
      | None -> (ready, [])
      | Some e ->
          let msg =
            match e with
            | `AutoCredentials -> s_ "Credentials cannot be handled by server."
            | `VoterAuthentication -> s_ "Bad authentication for voters."
            | `ForbiddenQuestions -> s_ "This kind of questions is forbidden."
            | `NotEnoughTrustees -> s_ "There are not enough trustees."
            | `NoThreshold -> s_ "The threshold mode must be used."
            | `HasWeights -> s_ "Weights are forbidden."
            | `BadGroup -> s_ "The chosen group is not allowed."
          in
          ( false,
            [ tr [ td [ txt (s_ "Restricted mode?") ]; td [ notok msg ] ] ] )
    in
    let div_trustee_warning =
      match se.se_trustees with
      | `Basic x when x.dbp_trustees = [] ->
          div
            [
              b [ txt (s_ "Warning:") ];
              txt " ";
              txt
                (s_
                   "No trustees were set. This means the server will manage \
                    the election key by itself.");
            ]
      | _ -> txt ""
    in
    let contact, div_contact_warning =
      match se.se_metadata.e_contact with
      | None ->
          ( s_ "No",
            div
              [
                b [ txt (s_ "Warning:") ];
                txt " ";
                txt (s_ "No contact was set!");
              ] )
      | Some _ -> (s_ "Yes", txt "")
    in
    let table_checklist =
      [
        [
          tr [ td [ txt (s_ "Name?") ]; td [ name ] ];
          tr [ td [ txt (s_ "Description?") ]; td [ description ] ];
          tr
            [
              td [ txt (s_ "Public name of the administrator?") ];
              td [ admin_name ];
            ];
          tr
            [
              td [ txt (s_ "Questions?") ];
              td [ questions; txt " "; preview_booth l uuid se.se_metadata ];
            ];
          tr [ td [ txt (s_ "Voters?") ]; td [ voters ] ];
          tr [ td [ txt (s_ "Passwords?") ]; td [ passwords ] ];
          tr
            [
              td [ txt (s_ "Credential authority?") ];
              td [ credential_authority ];
            ];
          tr [ td [ txt (s_ "Credentials?") ]; td [ credentials ] ];
          tr [ td [ txt (s_ "Private credentials?") ]; td [ private_creds ] ];
          tr
            [
              td [ txt (s_ "Trustees?") ];
              td [ trustees; txt " ("; checkpriv_link l uuid; txt ")" ];
            ];
          tr [ td [ txt (s_ "Contact?") ]; td [ txt contact ] ];
        ];
        nh_and_weights;
        restricted_mode;
      ]
      |> List.flatten |> table
    in
    let status =
      if ready then
        span ~a:[ a_style "color: green;" ] [ txt (s_ "election ready") ]
      else span ~a:[ a_style "color: red;" ] [ txt (s_ "election not ready") ]
    in
    let checklist =
      div
        [
          h2 [ txt (s_ "Checklist:"); txt " "; status ];
          table_checklist;
          div_trustee_warning;
          div_contact_warning;
        ]
    in
    let form_create =
      if ready then
        post_form ~service:election_draft_create
          (fun () ->
            [
              div
                [
                  h2 [ txt (s_ "Validate creation") ];
                  input ~input_type:`Submit ~value:(s_ "Create election") string;
                  txt " ";
                  txt (s_ "(Warning: This action is irreversible.)");
                ];
            ])
          uuid
      else div []
    in
    let back =
      div
        [
          a ~service:Web_services.election_draft
            [ txt (s_ "Go back to election draft") ]
            uuid;
        ]
    in
    let content = [ back; checklist; form_create ] in
    let* login_box = login_box () in
    base ~title ~login_box ~content ()

  let election_admin ?shuffle_token ?tally_token s election metadata status () =
    let langs = get_languages metadata.e_languages in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let title = template.t_name ^ " — " ^ s_ "Administration" in
    let auto_form () =
      let* d = Web_persist.get_election_automatic_dates s uuid in
      let format = function
        | None -> ""
        | Some x -> Datetime.format @@ Datetime.from_unixfloat x
      in
      return
      @@ post_form ~service:election_auto_post
           (fun (lopen, lclose) ->
             [
               div [ txt (s_ "Alternatively, you may set up automatic dates.") ];
               div
                 [
                   b [ txt (s_ "Note:") ];
                   txt " ";
                   txt (s_ "times are in UTC. Now is ");
                   txt (Datetime.format @@ Datetime.now ());
                   txt ".";
                 ];
               div
                 ~a:[ a_style "margin-left: 3em;" ]
                 [
                   div
                     [
                       txt (s_ "Automatically open the election at:");
                       txt " ";
                       input ~name:lopen ~input_type:`Text
                         ~value:(format d.auto_date_open) string;
                     ];
                   div
                     [
                       txt (s_ "Automatically close the election at:");
                       txt " ";
                       input ~name:lclose ~input_type:`Text
                         ~value:(format d.auto_date_close) string;
                     ];
                   div
                     [
                       txt
                         (s_
                            "Enter dates in UTC format, as per YYYY-MM-DD \
                             HH:MM:SS, leave empty for no date.");
                     ];
                 ];
               div
                 [
                   input ~input_type:`Submit
                     ~value:(s_ "Change automatic dates")
                     string;
                 ];
             ])
           uuid
    in
    let state_form checked =
      let service, value, msg, msg2 =
        if checked then
          ( election_close,
            s_ "Close election",
            s_ "The election is open. Voters can vote.",
            s_ " You may re-open the election when it is closed." )
        else
          ( election_open,
            s_ "Open election",
            s_ "The election is closed. No one can vote.",
            "" )
      in
      post_form ~service
        (fun () ->
          [
            div ~a:[ a_style "text-align: center;" ] [ txt msg; txt " " ];
            br ();
            input ~input_type:`Submit ~value string;
            txt msg2;
          ])
        uuid
    in
    let* state_div =
      match status.status_state with
      | `Open ->
          let* auto_form = auto_form () in
          return @@ div [ state_form true; br (); auto_form ]
      | `Closed ->
          let* auto_form = auto_form () in
          return
          @@ div
               [
                 state_form false;
                 br ();
                 auto_form;
                 br ();
                 hr ();
                 post_form ~service:election_compute_encrypted_tally
                   (fun () ->
                     [
                       input ~input_type:`Submit
                         ~value:(s_ "Proceed to vote counting")
                         string;
                       txt " ";
                       txt
                         (s_
                            "Warning: This action is irreversible; the \
                             election will be definitively closed.");
                     ])
                   uuid;
               ]
      | `Shuffling ->
          let* shuffles = Api_elections.get_shuffles s uuid metadata in
          let shufflers = shuffles.shuffles_shufflers in
          let select_disabled =
            List.exists (fun x -> x.shuffler_token <> None) shufflers
          in
          let* table_contents =
            Lwt_list.map_s
              (fun x ->
                let skip, hash, done_ =
                  let mk_skip disabled =
                    post_form ~service:election_shuffler_skip_confirm
                      (fun (nuuid, ntrustee) ->
                        let a = if disabled then [ a_disabled () ] else [] in
                        [
                          input ~input_type:`Hidden ~name:nuuid ~value:uuid
                            (user Uuid.unwrap);
                          input ~input_type:`Hidden ~name:ntrustee
                            ~value:x.shuffler_address string;
                          input ~a ~input_type:`Submit ~value:(s_ "Skip") string;
                        ])
                      ()
                  in
                  match x.shuffler_fingerprint with
                  | None -> (mk_skip false, txt "", false)
                  | Some h ->
                      ( mk_skip true,
                        txt (if h = "" then s_ "(skipped)" else h),
                        true )
                in
                let this_line =
                  match shuffle_token with
                  | Some y when x.shuffler_token = Some y -> true
                  | _ -> false
                in
                let* cell =
                  match x.shuffler_token with
                  | Some token ->
                      let uri =
                        compute_hash_link ~uuid ~token
                          ~service:election_shuffle_link_static
                      in
                      let* subject, body = Mails_admin.mail_shuffle langs uri in
                      return
                      @@ div
                           [
                             a_mailto ~dest:x.shuffler_address ~subject ~body
                               (s_ "E-mail");
                             txt " | ";
                             (if this_line then
                                a ~service:election_admin
                                  [ txt (s_ "Hide link") ]
                                  uuid
                              else
                                Raw.a
                                  ~a:
                                    [
                                      a_href (Xml.uri_of_string uri);
                                      a_id "shuffle-link";
                                    ]
                                  [ txt (s_ "Link") ]);
                           ]
                  | None ->
                      return
                      @@ post_form ~service:election_shuffler_select
                           (fun (nuuid, ntrustee) ->
                             let a =
                               if select_disabled || done_ then
                                 [ a_disabled () ]
                               else []
                             in
                             [
                               input ~input_type:`Hidden ~name:nuuid ~value:uuid
                                 (user Uuid.unwrap);
                               input ~input_type:`Hidden ~name:ntrustee
                                 ~value:x.shuffler_address string;
                               input ~a ~input_type:`Submit
                                 ~value:(s_ "Select this trustee") string;
                             ])
                           ()
                in
                let first_line =
                  tr
                    [
                      td [ txt x.shuffler_address ];
                      td [ cell ];
                      td [ (if done_ then txt (s_ "Yes") else txt (s_ "No")) ];
                      td [ skip ];
                      td [ hash ];
                    ]
                in
                let second_line =
                  match (this_line, x.shuffler_token) with
                  | true, Some token ->
                      let uri =
                        compute_hash_link ~uuid ~token
                          ~service:election_shuffle_link_static
                      in
                      [
                        tr
                          [
                            td
                              ~a:[ a_colspan 5 ]
                              [
                                txt
                                  (s_ "The link that must be sent to trustee ");
                                txt x.shuffler_address;
                                txt (s_ " is:");
                                br ();
                                txt uri;
                              ];
                          ];
                      ]
                  | _, _ -> []
                in
                return (first_line :: second_line))
              shufflers
          in
          let proceed =
            if List.for_all (fun x -> x.shuffler_fingerprint <> None) shufflers
            then
              post_form ~service:election_decrypt
                (fun () ->
                  [
                    input ~input_type:`Submit
                      ~value:(s_ "Proceed to decryption")
                      string;
                  ])
                uuid
            else txt ""
          in
          return
            (div
               [
                 div
                   [
                     div
                       ~a:[ a_style "text-align: center;" ]
                       [ txt (s_ "Shuffling of ballots") ];
                     table
                       (tr
                          [
                            th [ txt (s_ "Trustee") ];
                            th [];
                            th [ txt (s_ "Done?") ];
                            th [];
                            th [ txt (s_ "Fingerprint") ];
                          ]
                       :: List.flatten table_contents);
                   ];
                 proceed;
               ])
      | `EncryptedTally ->
          let* p = Api_elections.get_partial_decryptions s uuid metadata in
          let threshold_or_not =
            match p.partial_decryptions_threshold with
            | None -> txt ""
            | Some x ->
                txt
                  (" "
                  ^ Printf.sprintf (f_ "At least %d trustee(s) must act.") x)
          in
          let* trustees =
            p.partial_decryptions_trustees
            |> Lwt_list.map_s (fun t ->
                   let this_line =
                     match tally_token with
                     | Some x when x = t.trustee_pd_token -> true
                     | _ -> false
                   in
                   let uri =
                     compute_hash_link ~uuid ~token:t.trustee_pd_token
                       ~service:election_tally_trustees_static
                   in
                   let* mail, link =
                     if t.trustee_pd_address = "server" then
                       return (txt (s_ "(server)"), txt (s_ "(server)"))
                     else
                       let* subject, body =
                         Mails_admin.mail_trustee_tally langs uri
                       in
                       let mail =
                         a_mailto ~dest:t.trustee_pd_address ~subject ~body
                           (s_ "E-mail")
                       in
                       let link =
                         if this_line then
                           a ~service:election_admin
                             [ txt (s_ "Hide link") ]
                             uuid
                         else
                           Raw.a
                             ~a:[ a_href (Xml.uri_of_string uri) ]
                             [ txt (s_ "Link") ]
                       in
                       return (mail, link)
                   in
                   let first_line =
                     tr
                       [
                         td [ txt t.trustee_pd_address ];
                         td [ mail ];
                         td [ link ];
                         td
                           [
                             txt
                               (if t.trustee_pd_done then s_ "Yes" else s_ "No");
                           ];
                       ]
                   in
                   let second_line =
                     if this_line then
                       [
                         tr
                           [
                             td
                               ~a:[ a_colspan 4 ]
                               [
                                 txt
                                   (s_ "The link that must be sent to trustee ");
                                 txt t.trustee_pd_address;
                                 txt (s_ " is:");
                                 br ();
                                 txt uri;
                               ];
                           ];
                       ]
                     else []
                   in
                   return (first_line :: second_line))
          in
          let* release_form =
            let* hidden = Web_persist.get_election_result_hidden s uuid in
            match hidden with
            | Some t ->
                let scheduled =
                  div
                    [
                      Printf.sprintf
                        (f_ "The result is scheduled to be published after %s.")
                        (Datetime.unwrap t)
                      |> txt;
                    ]
                in
                post_form ~service:election_show_result
                  (fun () ->
                    [
                      scheduled;
                      input ~input_type:`Submit
                        ~value:(s_ "Publish the result as soon as possible")
                        string;
                    ])
                  uuid
                |> return
            | None ->
                let postpone_form =
                  post_form ~service:election_hide_result
                    (fun date ->
                      [
                        div
                          [
                            Printf.ksprintf txt
                              (f_
                                 "You may postpone the publication of the \
                                  election result up to %d days in the future.")
                              Web_defaults.days_to_publish_result;
                          ];
                        div
                          [
                            input ~input_type:`Submit
                              ~value:(s_ "Postpone publication until")
                              string;
                            txt " ";
                            input ~name:date ~input_type:`Text string;
                          ];
                        div
                          [
                            txt
                              (s_
                                 "Enter the date in UTC fornat, as per \
                                  YYYY-MM-DD HH:MM:SS. For example, today is ");
                            txt
                              (String.sub
                                 (string_of_datetime (Datetime.now ()))
                                 1 19);
                            txt ".";
                          ];
                      ])
                    uuid
                in
                let release_form =
                  post_form ~service:election_tally_release
                    (fun () ->
                      [
                        div
                          [
                            txt
                            @@ s_
                                 "You may force the computation of the result \
                                  now, if the required number of trustees have \
                                  done their job, by clicking on the following \
                                  button.";
                            txt " ";
                            txt
                            @@ s_
                                 "Note: no more partial decryptions will be \
                                  allowed.";
                          ];
                        div
                          [
                            input ~input_type:`Submit
                              ~value:(s_ "Compute the result") string;
                          ];
                      ])
                    uuid
                in
                div [ postpone_form; hr (); release_form ] |> return
          in
          return
          @@ div
               [
                 div
                   [
                     txt (s_ "The ");
                     a ~service:election_dir
                       [ txt (s_ "encrypted tally") ]
                       (uuid, ESETally);
                     txt (s_ " has been computed.");
                   ];
                 div
                   [
                     div [ txt (s_ "Awaiting trustees…"); threshold_or_not ];
                     table
                       (tr
                          [
                            th [ txt (s_ "Trustee") ];
                            th [ txt (s_ "E-mail") ];
                            th [ txt (s_ "Link") ];
                            th [ txt (s_ "Done?") ];
                          ]
                       :: List.flatten trustees);
                   ];
                 release_form;
               ]
      | `Tallied ->
          return
          @@ div
               [
                 div
                   [
                     txt (s_ "This election has been tallied.");
                     txt " ";
                     a ~service:election_download_archive
                       [ txt (s_ "Download archive.") ]
                       (uuid, ());
                   ];
               ]
      | `Archived ->
          return
          @@ div
               [
                 txt (s_ "This election is archived.");
                 txt " ";
                 a ~service:election_download_archive
                   [ txt (s_ "Download archive.") ]
                   (uuid, ());
               ]
    in
    let archive_date =
      match status.status_auto_archive_date with
      | None -> txt ""
      | Some t ->
          div
            [
              txt (s_ "This election will be automatically archived after ");
              txt (Datetime.format @@ Datetime.from_unixfloat t);
              txt ".";
            ]
    in
    let div_archive =
      match status.status_state with
      | `Archived -> txt ""
      | _ -> div [ br (); hr (); archive_date ]
    in
    let delete_date =
      let t = status.status_auto_delete_date in
      div
        [
          txt (s_ "This election will be automatically deleted after ");
          txt (Datetime.format @@ Datetime.from_unixfloat t);
          txt ".";
        ]
    in
    let div_delete =
      div
        [
          br ();
          hr ();
          delete_date;
          post_form ~service:election_delete
            (fun () ->
              [
                input ~input_type:`Submit ~value:(s_ "Delete the election")
                  string;
                txt " ";
                txt (s_ "Warning: This action is irreversible.");
              ])
            uuid;
        ]
    in
    let password =
      match metadata.e_auth_config with
      | Some [ { auth_system = "password"; _ } ] -> true
      | _ -> false
    in
    let div_regenpwd =
      if
        password
        && match status.status_state with `Open | `Closed -> true | _ -> false
      then
        div
          [
            a
              ~a:[ a_id "election_regenpwd" ]
              ~service:election_regenpwd
              [ txt (s_ "Regenerate and e-mail a password") ]
              uuid;
          ]
      else txt ""
    in
    let content =
      [
        try_new_ui l (Some uuid);
        div
          [
            a ~service:Web_services.election_home
              [ txt (s_ "Election home") ]
              (uuid, ());
          ];
        div
          [ a ~service:election_dir [ txt (s_ "Voter list") ] (uuid, ESVoters) ];
        div
          [
            a ~service:election_pretty_records
              [ txt (s_ "Voting records") ]
              (uuid, ());
          ];
        div
          [
            a ~service:election_missing_voters
              [ txt (s_ "Missing voters") ]
              (uuid, ());
          ];
        div [ checkpriv_link l uuid ];
        div_regenpwd;
        hr ();
        div [ state_div ];
        div_archive;
        div_delete;
      ]
    in
    let* login_box = login_box ~cont:(ContSiteElection uuid) () in
    base ~title ~login_box ~content ()

  let regenpwd uuid () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:election_regenpwd_post
        (fun user ->
          [
            div
              [
                txt (s_ "Username:");
                txt " ";
                input ~name:user ~input_type:`Text string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        uuid
    in
    let content = [ form ] in
    let title = s_ "Regenerate and e-mail a password" in
    let* login_box = login_box ~cont:(ContSiteElection uuid) () in
    base ~title ~login_box ~content ~uuid ()

  let pretty_records s election records () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let title = template.t_name ^ " — " ^ s_ "Records" in
    let nrecords = List.length records in
    let records =
      List.map
        (fun { vr_date; vr_username } ->
          tr
            [
              td [ txt @@ Datetime.format @@ Datetime.from_unixfloat vr_date ];
              td [ txt vr_username ];
            ])
        records
    in
    let* voters = Web_persist.get_all_voters s uuid in
    let nvoters = List.length voters in
    let summary =
      div
        [ Printf.ksprintf txt (f_ "Number of records: %d/%d") nrecords nvoters ]
    in
    let table =
      match records with
      | [] -> div [ txt (s_ "Nobody voted!") ]
      | _ ->
          div
            [
              table
                (tr
                   [
                     th [ txt (s_ "Date/Time (UTC)") ];
                     th [ txt (s_ "Username") ];
                   ]
                :: records);
            ]
    in
    let content =
      [
        div
          [
            txt (s_ "You can also access the ");
            a ~service:election_dir [ txt (s_ "raw data") ] (uuid, ESRecords);
            txt ".";
          ];
        summary;
        table;
      ]
    in
    let* login_box = login_box ~cont:(ContSiteElection uuid) () in
    base ~title ~login_box ~content ()

  let election_shuffler_skip_confirm uuid trustee =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = Printf.sprintf (f_ "Skipping trustee %s") trustee in
    let content =
      [
        post_form ~service:election_shuffler_skip
          (fun (nuuid, ntrustee) ->
            [
              div
                [
                  txt
                    (s_
                       "You may skip a trustee if they do not answer. Be aware \
                        that this reduces the security.");
                ];
              div
                [
                  input ~input_type:`Hidden ~name:nuuid ~value:uuid
                    (user Uuid.unwrap);
                  input ~input_type:`Hidden ~name:ntrustee ~value:trustee string;
                  input ~input_type:`Submit ~value:(s_ "Confirm") string;
                  txt " ";
                  a ~service:Web_services.election_admin
                    [ txt (s_ "Cancel") ]
                    uuid;
                ];
            ])
          ();
      ]
    in
    base ~title ~content ()

  let shuffle_static () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let uuid = Uuid.wrap "XXXXXXXXXXXXXX" and token = "XXXXXXXXXXXXXX" in
    let title = s_ "Shuffle" in
    let content =
      div
        ~a:[ a_id "initially_hidden_content"; a_style "display: none;" ]
        [
          div
            [
              txt
                (s_
                   "As a trustee, your first role is to shuffle the encrypted \
                    ballots.");
            ];
          div
            [
              txt (s_ "Current list of ballots:");
              txt " ";
              raw_textarea ~rows:5 ~cols:40 "current_ballots" "";
              txt " ";
              (let service =
                 Eliom_service.preapply ~service:election_nh_ciphertexts uuid
               in
               raw_a
                 ~a:[ a_id "nh_ciphertexts_link" ]
                 ~service
                 [ txt (s_ "Download as a file") ]
                 ());
            ];
          div
            ~a:[ a_id "estimation" ]
            [ txt (s_ "Estimating computation time…") ];
          div
            ~a:[ a_id "wait_div" ]
            [
              txt (s_ "Please wait… ");
              img ~src:(static "encrypting.gif") ~alt:(s_ "Loading…") ();
            ];
          div
            ~a:[ a_id "controls_div"; a_style "display: none;" ]
            [
              button_no_value ~button_type:`Button
                ~a:[ a_id "compute_shuffle" ]
                [ txt (s_ "Compute shuffle") ];
            ];
          post_form ~service:election_shuffle_post
            ~a:[ a_id "submit_form" ]
            (fun nshuffle ->
              [
                div
                  [
                    txt (s_ "Shuffled list of ballots:");
                    txt " ";
                    textarea
                      ~a:[ a_rows 5; a_cols 40; a_id "shuffle" ]
                      ~name:nshuffle ();
                  ];
                div
                  ~a:[ a_id "hash_div"; a_style "display:none;" ]
                  [
                    div
                      [
                        txt (s_ "The fingerprint of your shuffle is:");
                        txt " ";
                        b ~a:[ a_id "hash" ] [];
                        txt ".";
                      ];
                    div
                      [
                        txt
                          (s_
                             "You must record this fingerprint and check that \
                              it appears on the election result page.");
                      ];
                  ];
                div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
              ])
            (uuid, token);
          script_with_lang ~lang "tool_js_shuffle.js";
        ]
    in
    base ~title ~content:[ content ] ~static:true ()

  let tally_trustees_static () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Partial decryption" in
    let content =
      div
        ~a:[ a_id "initially_hidden_content"; a_style "display: none;" ]
        [
          p
            [
              txt
                (s_ "It is now time to compute your partial decryption factors.");
            ];
          p
            [
              txt (s_ "The fingerprint of the encrypted tally is ");
              b [ span ~a:[ a_id "hash" ] [] ];
              txt ".";
            ];
          hr ();
          div
            [
              b [ txt (s_ "Instructions:") ];
              ol
                [
                  li
                    [
                      div
                        ~a:[ a_id "input_private_key" ]
                        [
                          div
                            [
                              p [ txt (s_ "Please enter your private key:") ];
                              input
                                ~a:[ a_id "private_key"; a_size 80 ]
                                ~input_type:`Text string;
                            ];
                          div
                            [
                              p [ txt (s_ "Or load it from a file:") ];
                              input
                                ~a:[ a_id "private_key_file" ]
                                ~input_type:`File string;
                            ];
                        ];
                      br ();
                    ];
                  li
                    [
                      div
                        [
                          button_no_value
                            ~a:[ a_id "compute" ]
                            ~button_type:`Button
                            [
                              txt
                                (s_ "Generate your contribution to decryption");
                            ];
                        ];
                      br ();
                    ];
                  li
                    [
                      div
                        ~a:[ a_id "pd_done" ]
                        [
                          (let uuid = Uuid.wrap "XXXXXXXXXXXXXX"
                           and token = "XXXXXXXXXXXXXX" in
                           post_form
                             ~a:[ a_id "pd_form" ]
                             ~service:election_tally_trustees_post
                             (fun pd ->
                               [
                                 div
                                   [
                                     input ~input_type:`Submit
                                       ~value:(s_ "Submit") string;
                                     txt
                                       (s_ " your contribution to decryption.");
                                   ];
                                 div
                                   [
                                     txt "Data: ";
                                     textarea
                                       ~a:[ a_rows 5; a_cols 40; a_id "pd" ]
                                       ~name:pd ();
                                   ];
                               ])
                             (uuid, token));
                        ];
                    ];
                ];
            ];
          script_with_lang ~lang "tool_js_pd.js";
        ]
    in
    base ~title ~content:[ content ] ~static:true ()

  let signup_captcha ~service error challenge email =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:signup_captcha_post
        (fun (lchallenge, (lresponse, lemail)) ->
          [
            div
              [
                txt (s_ "E-mail address:");
                txt " ";
                input ~input_type:`Text ~name:lemail ~value:email string;
              ];
            div
              [
                input ~input_type:`Hidden ~name:lchallenge ~value:challenge
                  string;
                txt (s_ "Please enter ");
                Pages_common.signup_captcha_img challenge;
                txt (s_ " in the following box: ");
                input ~input_type:`Text ~name:lresponse string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        service
    in
    let error = format_captcha_error l error in
    let content = [ error; form ] in
    base ~title:(s_ "Create an account") ~content ()

  let signup_changepw ~service error challenge email username =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:changepw_captcha_post
        (fun (lchallenge, (lresponse, (lemail, lusername))) ->
          [
            div
              [
                txt (s_ "E-mail address:");
                txt " ";
                input ~input_type:`Text ~name:lemail ~value:email string;
                txt (s_ " or username: ");
                input ~input_type:`Text ~name:lusername ~value:username string;
                txt ".";
              ];
            div
              [
                input ~input_type:`Hidden ~name:lchallenge ~value:challenge
                  string;
                txt (s_ "Please enter ");
                Pages_common.signup_captcha_img challenge;
                txt (s_ " in the following box: ");
                input ~input_type:`Text ~name:lresponse string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        service
    in
    let error = format_captcha_error l error in
    let content = [ error; form ] in
    base ~title:(s_ "Change password") ~content ()

  let signup_login () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:signup_login_post
        (fun lcode ->
          [
            div
              [
                txt
                  (s_ "Please enter the verification code received by e-mail:");
                txt " ";
                input ~input_type:`Text ~name:lcode string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        ()
    in
    let content = [ form ] in
    base ~title:(s_ "Account management") ~content ()

  let signup address error username =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let error =
      match error with
      | None -> txt ""
      | Some e ->
          let msg =
            match e with
            | UsernameTaken -> s_ "the username is already taken"
            | AddressTaken ->
                s_ "there is already an account with this e-mail address"
            | BadUsername -> s_ "the username is invalid"
            | BadPassword e ->
                Printf.sprintf (f_ "the password is too weak (%s)") e
            | PasswordMismatch -> s_ "the two passwords are not the same"
            | BadSpaceInPassword ->
                s_ "the password starts or ends with a space"
            | DatabaseError -> s_ "there is an error in the database"
          in
          div
            [
              txt (s_ "The account creation ");
              span ~a:[ a_style "color: red;" ] [ txt (s_ "failed") ];
              txt (s_ " because ");
              txt msg;
              txt (s_ ". Please try again with a different one.");
            ]
    in
    let form =
      post_form ~service:signup_post
        (fun (lusername, (lpassword, lpassword2)) ->
          [
            div [ txt (s_ "Your e-mail address is: "); txt address; txt "." ];
            div
              [
                txt (s_ "Please choose a username: ");
                input ~input_type:`Text ~name:lusername ~value:username string;
                txt (s_ " and a password: ");
                input ~input_type:`Password ~name:lpassword string;
                txt ".";
              ];
            div
              [
                txt (s_ "Type the password again: ");
                input ~input_type:`Password ~name:lpassword2 string;
                txt ".";
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        ()
    in
    let content = [ error; form ] in
    base ~title:(s_ "Create an account") ~content ()

  let changepw ~username ~address error =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let error =
      match error with
      | None -> txt ""
      | Some e ->
          let reason =
            match e with
            | PasswordMismatch -> s_ "the two passwords are not the same"
            | BadPassword e ->
                Printf.sprintf (f_ "the new password is too weak (%s)") e
            | BadSpaceInPassword ->
                s_ "the new password starts or ends with a space"
            | _ -> s_ "of an unknown reason"
          in
          div
            [
              txt (s_ "The change ");
              span ~a:[ a_style "color: red;" ] [ txt (s_ "failed") ];
              txt (s_ " because ");
              txt reason;
              txt (s_ ". Please try again with a different one.");
            ]
    in
    let form =
      post_form ~service:changepw_post
        (fun (lpassword, lpassword2) ->
          [
            div
              [
                txt (s_ "Your username is: ");
                txt username;
                txt (s_ " and your e-mail address is: ");
                txt address;
                txt ".";
              ];
            div
              [
                txt (s_ "Please choose a password: ");
                input ~input_type:`Password ~name:lpassword string;
                txt ".";
              ];
            div
              [
                txt (s_ "Type the password again: ");
                input ~input_type:`Password ~name:lpassword2 string;
                txt ".";
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        ()
    in
    let content = [ error; form ] in
    base ~title:(s_ "Change password") ~content ()

  let compute_fingerprint () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let interactivity =
      div
        ~a:[ a_id "interactivity" ]
        [ script_with_lang ~lang "tool_js_fingerprint.js" ]
    in
    let content = [ interactivity ] in
    base ~title:(s_ "Compute fingerprint") ~content ()

  let set_email () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:set_email_post
        (fun name ->
          [
            div
              [
                txt (s_ "There is no e-mail address attached to your account.");
                txt " ";
                txt (s_ "Please provide one:");
                txt " ";
                input ~input_type:`Text ~name string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Proceed") string ];
          ])
        ()
    in
    let content = [ form ] in
    let title = s_ "Your e-mail address" in
    base ~title ~content ()

  let set_email_confirm ~address =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:set_email_confirm
        (fun name ->
          [
            div
              [
                txt
                  (Printf.sprintf
                     (f_ "An e-mail with a code has been sent to %s.")
                     address);
                txt " ";
                txt (s_ "Please enter the code here:");
                txt " ";
                input ~input_type:`Text ~name string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Proceed") string ];
          ])
        ()
    in
    let content = [ form ] in
    let title = s_ "Your e-mail address" in
    base ~title ~content ()

  let sudo () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:sudo_post
        (fun (ndomain, nuser) ->
          [
            div
              [
                txt "Domain: ";
                input ~input_type:`Text ~name:ndomain string;
                txt ", user: ";
                input ~input_type:`Text ~name:nuser string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Proceed") string ];
          ])
        ()
    in
    let content = [ form ] in
    let title = s_ "Impersonate a user" in
    base ~title ~content ()

  let account account =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Account settings" in
    let content =
      [
        post_form ~service:account_post
          (fun name ->
            [
              div
                [
                  txt (s_ "Name:");
                  txt " ";
                  input ~input_type:`Text ~name ~value:account.name string;
                ];
              div
                [
                  txt (s_ "E-mail address:");
                  txt " ";
                  txt @@ Option.value ~default:"(none)" account.email;
                ];
              div
                [
                  txt (s_ "Authentication methods:");
                  txt " ";
                  ul
                    (List.map
                       (fun u ->
                         li
                           [
                             Printf.ksprintf txt "%s:%s" u.user_domain
                               u.user_name;
                           ])
                       account.authentications);
                ];
              div
                [
                  txt (s_ "Consent date:");
                  txt " ";
                  txt
                    (match account.consent with
                    | None -> s_ "(none)"
                    | Some t -> Datetime.format t);
                ];
              div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
            ])
          ();
      ]
    in
    base ~title ~content ()
end

let mail_confirmation_link l address code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Your e-mail address has been used to create an account on our Belenios \
        server.");
  add_sentence b (s_ "To confirm this creation, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b (s_ "Belenios Server");
  let body = contents b in
  let subject = s_ "Belenios account creation" in
  (subject, body)

let mail_changepw_link l address code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "There has been a request to change the password of your account on our \
        Belenios server.");
  add_sentence b (s_ "To confirm this, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b (s_ "Belenios Server");
  let body = contents b in
  let subject = s_ "Belenios password change" in
  (subject, body)

let mail_set_email l address code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Someone is trying to associate your e-mail address to an account on \
        our Belenios server.");
  add_sentence b (s_ "To confirm this, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b (s_ "Belenios Server");
  let body = contents b in
  let subject = s_ "Belenios account e-mail address change" in
  (subject, body)
