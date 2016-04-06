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

open Lwt
open Serializable_j
open Signatures
open Common
open Web_serializable_j
open Web_signatures
open Web_common
open Web_services
open Eliom_content.Html5.F

(* TODO: these pages should be redesigned *)

let site_title = "Election Server"
let welcome_message = "Welcome to the Belenios system!"
let admin_background = " background: #FF9999;"

let format_user ~site u =
  em [pcdata (if site then string_of_user u else u.user_name)]

let make_login_box ~site auth links =
  let style = if site then admin_background else "" in
  let style = "float: right; text-align: right;" ^ style in
  let module S = (val auth : AUTH_SERVICES) in
  let module L = (val links : AUTH_LINKS) in
  lwt user = S.get_user () in
  lwt auth_systems = S.get_auth_systems () in
  return @@ div ~a:[a_style style] (
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
  )

module Site_links = struct
  let login x = Eliom_service.preapply site_login x
  let logout = Eliom_service.preapply logout ()
end

module Site_auth = struct
  let get_user () = Web_state.get_site_user ()
  let get_auth_systems () =
    lwt l = Web_persist.get_auth_config "" in
    return (List.map fst l)
end

let site_links = (module Site_links : AUTH_LINKS)
let site_auth = (module Site_auth : AUTH_SERVICES)

let site_login_box () =
  make_login_box ~site:true site_auth site_links

let belenios_url = Eliom_service.Http.external_service
  ~prefix:"http://belenios.gforge.inria.fr"
  ~path:[]
  ~get_params:Eliom_parameter.unit
  ()

let base ~title ~login_box ~content ?(footer = div []) ?uuid () =
  let administer =
    match uuid with
    | None ->
       a ~service:admin [pcdata "Administer elections"] ()
    | Some uuid ->
       a ~service:election_admin [pcdata "Administer this election"] (uuid, ())
  in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang "en"]
    (head (Eliom_content.Html5.F.title (pcdata title)) [
      script (pcdata "window.onbeforeunload = function () {};");
      link ~rel:[`Stylesheet] ~href:(uri_of_string (fun () -> "/static/site.css")) ();
    ])
    (body [
      div ~a:[a_id "wrapper"] [
      div ~a:[a_id "header"] [
        div [
          div ~a:[a_style "float: left;"] [
            a ~service:home [pcdata site_title] ();
          ];
          login_box;
          div ~a:[a_style "clear: both;"] [];
        ];
        h1 ~a:[a_style "text-align: center;"] [pcdata title];
      ];
      div ~a:[a_id "main"] content;
      div ~a:[a_id "footer"; a_style "text-align: center;" ] [
        div ~a:[a_id "bottom"] [
          footer;
          pcdata "Powered by ";
          a ~service:belenios_url [pcdata "Belenios"] ();
          pcdata ". ";
          a ~service:source_code [pcdata "Get the source code"] ();
          pcdata ". ";
          administer;
          pcdata ".";
        ]
      ]]
     ]))

let format_election kind election =
  let module W = (val election : ELECTION_DATA) in
  let e = W.election.e_params in
  let service =
    match kind with
    | `Home -> election_home
    | `Admin -> election_admin
  in
  li [
    h3 [
      a ~service [pcdata e.e_name] (e.e_uuid, ());
    ];
    p [pcdata e.e_description];
  ]

let home () =
  let loria = Eliom_service.Http.external_service
    ~prefix:"http://www.loria.fr"
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()
  in
  let content = [
    div [
      h2 ~a:[a_style "text-align:center;"] [pcdata welcome_message];
      h3 [a ~service:admin [pcdata "Administer elections"] ()];
      div [br ()];
      div [
        pcdata "Belenios is an electronic voting system developed at ";
        a ~service:loria [pcdata "LORIA"] ();
        pcdata ". It provides both confidentiality of the votes and ";
        pcdata "end-to-end verifiability of the result. Verifiability ";
        pcdata "relies in particular on the fact that the ballots are ";
        pcdata "stored on a public ballot box (on a webpage), so that ";
        pcdata "voters can check the presence of their ballots. Similarly, ";
        pcdata "anyone can check that the published result corresponds to ";
        pcdata "the contents of the ballot box. More information and ";
        pcdata "discussion can be found on the ";
        a ~service:belenios_url [pcdata "Belenios web page"] ();
        pcdata ".";
      ];
    ];
  ] in
  let login_box = pcdata "" in
  base ~title:site_title ~login_box ~content ()

let admin ~elections () =
  let title = site_title ^ " — Administration" in
  match elections with
  | None ->
     let content = [
       div [
         pcdata "To administer an election, you need to ";
         a ~service:site_login [pcdata "log in"] None;
         pcdata ". If you do not have an account, ";
         pcdata "please send an email to contact@belenios.org.";
       ]
     ] in
     lwt login_box = site_login_box () in
     base ~title ~login_box ~content ()
  | Some (elections, tallied, archived, setup_elections) ->
    let elections =
      match elections with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@ List.map (format_election `Admin) elections
    in
    let tallied =
      match tallied with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@ List.map (format_election `Admin) tallied
    in
    let archived =
      match archived with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@ List.map (format_election `Admin) archived
    in
    let setup_elections =
      match setup_elections with
      | [] -> p [pcdata "You own no such elections!"]
      | _ -> ul @@
         List.map (fun (k, title) ->
           li [a ~service:election_setup [pcdata title] k]
         ) setup_elections
    in
    let content = [
      div [
        div [
          a ~service:election_setup_pre [
            pcdata "Prepare a new election";
          ] ();
        ];
        div [br ()];
        h2 [pcdata "Elections being prepared"];
        setup_elections;
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
    lwt login_box = site_login_box () in
    base ~title ~login_box ~content ()

let make_button ~service contents =
  let uri = Eliom_uri.make_string_uri ~service () in
  Printf.ksprintf Unsafe.data (* FIXME: unsafe *)
    "<button onclick=\"location.href='%s';\" style=\"font-size:35px;\">%s</button>"
    uri
    contents

let a_mailto ~dest ~body contents =
  let uri = Printf.sprintf "mailto:%s?body=%s" dest
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
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let generic_page ~title ~service message () =
  let content = [
    p [pcdata message];
    div [
      a ~service [pcdata "Proceed"] ();
    ];
  ] in
  let login_box = pcdata "" in
  base ~title ~login_box ~content ()

let election_setup_pre () =
  let title = "Prepare a new election" in
  let cred_info = Eliom_service.Http.external_service
    ~prefix:"http://belenios.gforge.inria.fr"
    ~path:["howitworks.php"]
    ~get_params:Eliom_parameter.unit
    ()
  in
  let form =
    post_form ~service:election_setup_new
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
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_setup uuid se () =
  let title = "Preparation of election " ^ se.se_questions.t_name in
  let form_description =
    post_form ~service:election_setup_description
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
  let has_credentials = match se.se_metadata.e_cred_authority with
    | None -> false
    | Some _ -> true
  in
  let auth = match se.se_metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] -> `Password
    | Some [{auth_system = "dummy"; _}] -> `Dummy
    | Some [{auth_system = "cas"; auth_config = ["server", server]; _}] -> `CAS server
    | _ -> failwith "unknown authentication scheme in election_setup"
  in
  let div_auth =
    div [
      h2 [pcdata "Authentication"];
      match auth with
      | `Password ->
         div [
           pcdata "Authentication scheme: password ";
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
        a ~a:[a_id "edit_questions"] ~service:election_setup_questions
          [pcdata "Edit questions"]
          uuid;
      ]
    ]
  in
  let div_voters =
    div [
      h2 [
        a ~a:[a_id "edit_voters"] ~service:election_setup_voters
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
        pcdata "By default, the election server manages the keys of the ";
        pcdata "election. If you do not wish the server to store any keys, ";
        pcdata "click ";
        a ~service:election_setup_trustees [pcdata "here"] uuid;
        pcdata "."];
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
            post_form ~service:election_setup_credentials_server
              (fun () ->
                [string_input ~input_type:`Submit ~value:"Generate on server" ()]
              ) uuid
          ) else (
            div [
              a ~service:election_setup_credential_authority [pcdata "Credential management"] uuid;
            ]
          );
        ]
      )
    ]
  in
  let form_create =
    post_form
      ~service:election_setup_create
      (fun () ->
       [div
          [h2 [pcdata "Finalize creation"];
           string_input ~input_type:`Submit ~value:"Create election" ();
           pcdata " (Warning: this action is irreversible.)";
          ]]
      ) uuid
  in
  let content = [
    div_description;
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
    form_create;
  ] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_setup_trustees uuid se () =
  let title = "Trustees for election " ^ se.se_questions.t_name in
  let form_trustees_add =
    post_form
      ~service:election_setup_trustee_add
      (fun name ->
        [
          pcdata "Trustee's e-mail address: ";
          string_input ~input_type:`Text ~name ();
          string_input ~input_type:`Submit ~value:"Add" ();
        ]
      ) uuid
  in
  let mk_form_trustee_del value =
    post_form
      ~service:election_setup_trustee_del
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
           th [pcdata "Trustee link"];
           th [pcdata "Done?"];
           th [pcdata "Remove"];
         ] ::
           List.mapi (fun i t ->
             tr [
               td [
                 let body = rewrite_prefix @@ Eliom_uri.make_string_uri
                   ~absolute:true ~service:election_setup_trustee t.st_token
                 in a_mailto ~dest:t.st_id ~body t.st_id
               ];
               td [
                 pcdata (if t.st_public_key = "" then "No" else "Yes");
               ];
               td [mk_form_trustee_del i];
             ]
           ) ts
       )
  in
  let div_content =
    div [
      div [pcdata "If you do not wish the server to store any keys, you may nominate trustees. In that case, each trustee will create her own secret key. Be careful, once the election is over, you will need the contribution of each trustee to compute the result!"];
      br ();
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
  in
  let back_link = div [
    a ~service:Web_services.election_setup
      [pcdata "Go back to election setup"] uuid;
  ] in
  let content = [
    div_content;
    back_link;
  ] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_setup_credential_authority _ se () =
  let title = "Credentials for election " ^ se.se_questions.t_name in
  let content = [
    div [
      pcdata "Please send the credential authority the following link:";
    ];
    ul [
      li [
        a
          ~service:election_setup_credentials
          [
            pcdata @@ rewrite_prefix @@ Eliom_uri.make_string_uri
              ~absolute:true
              ~service:election_setup_credentials
              se.se_public_creds
          ]
          se.se_public_creds;
      ];
    ];
    div [
      pcdata "Note that this authority will have to send each credential to each voter herself.";
    ];
  ] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_setup_questions uuid se () =
  let title = "Questions for election " ^ se.se_questions.t_name in
  let form =
    let value = string_of_template se.se_questions in
    post_form
      ~service:election_setup_questions_post
      (fun name ->
       [
         div [pcdata "Questions:"];
         div [textarea ~a:[a_id "questions"; a_rows 5; a_cols 80] ~name ~value ()];
         div [string_input ~input_type:`Submit ~value:"Save changes" ()]])
      uuid
  in
  let link =
    let service = Web_services.election_setup in
    div [a ~service [pcdata "Go back to election preparation"] uuid]
  in
  let interactivity =
    div
      ~a:[a_id "interactivity"]
      [
        script ~a:[a_src (uri_of_string (fun () -> "../static/sjcl.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/jsbn.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/jsbn2.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/random.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/tool_js_questions.js"))] (pcdata "");
      ]
  in
  let content = [
    interactivity;
    form;
    link;
  ] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_setup_voters uuid se () =
  let title = "Voters for election " ^ se.se_questions.t_name in
  let form =
    post_form
      ~service:election_setup_voters_add
      (fun name ->
        [
          div [textarea ~a:[a_rows 20; a_cols 50] ~name ()];
          div [string_input ~input_type:`Submit ~value:"Add" ()]])
      uuid
  in
  let mk_remove_button id =
    post_form
      ~service:election_setup_voters_remove
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
    post_form ~service:election_setup_voters_passwd
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
      post_form ~service:election_setup_auth_genpwd
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
         table
           (tr (
             [th [pcdata "Identity"]] @
               (if has_passwords then [th [pcdata "Password sent?"]] else []) @
               (if se.se_public_creds_received then [] else [th [pcdata "Remove"]])
            ) :: voters)
       ]
  in
  let back = div [
    a ~service:Web_services.election_setup [pcdata "Return to setup page"] uuid;
  ] in
  let div_add =
    if se.se_public_creds_received then
      pcdata ""
    else
      div [
        div [pcdata "Please enter the identities of voters to add, one per line:"];
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
    a ~service:election_setup_import
      [pcdata "Import voters from another election"]
      uuid
  ] in
  let content = [
    back;
    div_import;
    voters;
    div_add;
  ] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_setup_credentials token uuid se () =
  let title = "Credentials for election " ^ se.se_questions.t_name in
  let form_textarea =
    post_form
      ~service:election_setup_credentials_post
      (fun name ->
       [div
          [div [pcdata "Public credentials:"];
           div [textarea ~a:[a_id "pks"; a_rows 5; a_cols 40] ~name ()];
           div [string_input ~input_type:`Submit ~value:"Submit" ()]]])
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
      ~service:election_setup_credentials_post_file
      (fun name ->
       [div
          [h2 [pcdata "Submit by file"];
           div [pcdata "Use this form to upload public credentials generated with the command-line tool."];
           div [file_input ~name ()];
           div [string_input ~input_type:`Submit ~value:"Submit" ()]]])
      token
  in
  let div_download =
    p [a ~service:election_setup_credentials_download
           [pcdata "Download current file"]
           token]
  in
  let group =
    let name : 'a Eliom_parameter.param_name = Obj.magic "group" in
    let value = se.se_group in
    div
      ~a:[a_style "display:none;"]
      [
        div [pcdata "UUID:"];
        div [textarea ~a:[a_id "uuid"; a_rows 1; a_cols 40; a_readonly `ReadOnly] ~name ~value:uuid ()];
        div [pcdata "Group parameters:"];
        div [textarea ~a:[a_id "group"; a_rows 5; a_cols 40; a_readonly `ReadOnly] ~name ~value ()];
      ]
  in
  let voters =
    let name : 'a Eliom_parameter.param_name = Obj.magic "voters" in
    let value = String.concat "\n" (List.map (fun x -> x.sv_id) se.se_voters) in
    div [
      div [pcdata "List of voters:"];
      div [textarea ~a:[a_id "voters"; a_rows 5; a_cols 40; a_readonly `ReadOnly] ~name ~value ()];
    ]
  in
  let interactivity =
    div
      ~a:[a_id "interactivity"]
      [
        script ~a:[a_src (uri_of_string (fun () -> "../static/sjcl.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/jsbn.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/jsbn2.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/random.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/tool_js_credgen.js"))] (pcdata "");
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
        div_download;
        div_textarea;
        form_file;
      ]
    ) in
  let login_box = pcdata "" in
  base ~title ~login_box ~content ()

let election_setup_trustee token se () =
  let title = "Trustee for election " ^ se.se_questions.t_name in
  let form =
    let trustee = List.find (fun x -> x.st_token = token) se.se_public_keys in
    let value = trustee.st_public_key in
    let service = Eliom_service.preapply election_setup_trustee_post token in
    post_form
      ~service
      (fun name ->
       [
         div [
           div [pcdata "Public key:"];
           div [textarea ~a:[a_rows 5; a_cols 40; a_id "pk"] ~name ~value ()];
           div [string_input ~input_type:`Submit ~value:"Submit" ()];
         ]
       ]
      ) ()
  in
  let group =
    let name : 'a Eliom_parameter.param_name = Obj.magic "group" in
    let value = se.se_group in
    div
      ~a:[a_style "display:none;"]
      [
        div [pcdata "Group parameters:"];
        div [textarea ~a:[a_id "group"; a_rows 5; a_cols 40; a_readonly `ReadOnly] ~name ~value ()];
      ]
  in
  let interactivity =
    div
      ~a:[a_id "interactivity"]
      [
        script ~a:[a_src (uri_of_string (fun () -> "../static/sjcl.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/jsbn.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/jsbn2.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/random.js"))] (pcdata "");
        script ~a:[a_src (uri_of_string (fun () -> "../static/tool_js_tkeygen.js"))] (pcdata "");
      ]
  in
  let content = [
    group;
    interactivity;
    form;
  ] in
  let login_box = pcdata "" in
  base ~title ~login_box ~content ()

let election_setup_import uuid se (elections, tallied, archived) () =
  let title = "Election " ^ se.se_questions.t_name ^ " — Import voters from another election" in
  let format_election election =
    let module W = (val election : ELECTION_DATA) in
    let name = W.election.e_params.e_name in
    let uuid_s = Uuidm.to_string W.election.e_params.e_uuid in
    let form = post_form
      ~service:election_setup_import_post
      (fun from ->
        [
          div [pcdata name; pcdata " ("; pcdata uuid_s; pcdata ")"];
          div [
            user_type_input Uuidm.to_string
              ~input_type:`Hidden
              ~name:from
              ~value:W.election.e_params.e_uuid ();
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
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_login_box w =
  let module W = (val w : ELECTION_DATA) in
  let module A = struct
    let get_user () =
      Web_state.get_election_user W.election.e_params.e_uuid
    let get_auth_systems () =
      let uuid_s = Uuidm.to_string W.election.e_params.e_uuid in
      lwt l = Web_persist.get_auth_config uuid_s in
      return @@ List.map fst l
  end in
  let auth = (module A : AUTH_SERVICES) in
  let module L = struct
    let login x =
      Eliom_service.preapply
        election_login
        ((W.election.e_params.e_uuid, ()), x)
    let logout =
      Eliom_service.preapply logout ()
  end in
  let links = (module L : AUTH_LINKS) in
  fun () -> make_login_box ~site:false auth links

let file w x =
  let module W = (val w : ELECTION_DATA) in
  Eliom_service.preapply
    election_dir
    (W.election.e_params.e_uuid, x)

let audit_footer w =
  let module W = (val w : ELECTION_DATA) in
  div ~a:[a_style "line-height:1.5em;"] [
    div [
      div [
        pcdata "Election fingerprint: ";
        code [ pcdata W.election.e_fingerprint ];
      ];
      div [
        pcdata "Audit data: ";
        a ~service:(file w ESRaw) [
          pcdata "parameters"
        ] ();
        pcdata ", ";
        a ~service:(file w ESCreds) [
          pcdata "public credentials"
        ] ();
        pcdata ", ";
        a ~service:(file w ESKeys) [
          pcdata "trustee public keys"
        ] ();
        pcdata ", ";
        a ~service:(file w ESBallots) [
          pcdata "ballots";
        ] ();
        pcdata ".";
      ];
    ]
  ]

let election_home w state () =
  let module W = (val w : ELECTION_DATA) in
  let params = W.election.e_params in
  let state_ =
    match state with
    | `Closed ->
      [
        pcdata " ";
        b [pcdata "This election is currently closed."];
      ]
    | `Open -> []
    | `EncryptedTally (_, _, hash) ->
       [
         pcdata " ";
         b [pcdata "The election is closed and being tallied."];
         pcdata " The ";
         a
           ~service:election_dir
           [pcdata "encrypted tally"]
           (W.election.e_params.e_uuid, ESETally);
         pcdata " hash is ";
         b [pcdata hash];
         pcdata ".";
       ]
    | `Tallied _ ->
       [
         pcdata " ";
         b [pcdata "This election has been tallied."];
       ]
    | `Archived ->
       [
         pcdata " ";
         b [pcdata "This election is archived."];
       ]
  in
  let ballots_link =
    p ~a:[a_style "text-align:center;"] [
        a
          ~a:[a_style "font-size:25px;"]
          ~service:election_pretty_ballots [
            pcdata "See accepted ballots"
          ] (params.e_uuid, ())
      ]
  in
  let footer = audit_footer w in
  let go_to_the_booth =
    div ~a:[a_style "text-align:center;"] [
      div [
        make_button
          ~service:(Eliom_service.preapply election_vote (params.e_uuid, ()))
          "Start";
        ];
      div [
        pcdata "or ";
        a
          ~service:(Eliom_service.preapply election_cast (params.e_uuid, ()))
          [pcdata "submit a raw ballot"] ();
      ];
    ]
  in
  lwt middle =
    let uuid = Uuidm.to_string params.e_uuid in
    lwt result = Web_persist.get_election_result uuid in
    match result with
    | Some r ->
       let result = r.result in
       let questions = Array.to_list W.election.e_params.e_questions in
       return @@ div [
         ul (List.mapi (fun i x ->
           let answers = Array.to_list x.q_answers in
           let answers = List.mapi (fun j x ->
             tr [td [pcdata x]; td [pcdata @@ string_of_int result.(i).(j)]]
           ) answers in
           let answers =
             match answers with
             | [] -> pcdata ""
             | x :: xs -> table (x :: xs)
           in
           li [
             pcdata x.q_question;
             answers;
           ]
         ) questions);
         div [
           pcdata "Number of accepted ballots: ";
           pcdata (string_of_int r.num_tallied);
         ];
         div [
           pcdata "You can also download the ";
           a ~service:election_dir
             [pcdata "result with cryptographic proofs"]
             (W.election.e_params.e_uuid, ESResult);
           pcdata ".";
         ];
       ]
    | None -> return go_to_the_booth
  in
  let content = [
    p state_;
    br ();
    middle;
    br ();
    ballots_link;
  ] in
  lwt login_box = election_login_box w () in
  let uuid = params.e_uuid in
  base ~title:params.e_name ~login_box ~content ~footer ~uuid ()

let election_admin w metadata state () =
  let module W = (val w : ELECTION_DATA) in
  let title = W.election.e_params.e_name ^ " — Administration" in
  let uuid_s = Uuidm.to_string W.election.e_params.e_uuid in
  let state_form checked =
    let service, value, msg =
      if checked then
        election_close, "Close election",
        "The election is open. Voters can vote. "
      else
        election_open, "Open election",
        "The election is closed. No one can vote. "
    in
    post_form
      ~service
      (fun () ->
       [
         pcdata msg;
         string_input ~input_type:`Submit ~value ();
       ]) (W.election.e_params.e_uuid, ())
  in
  lwt state_div =
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
                 ~value:"Tally election"
                 ();
              pcdata " Warning: this action is irreversible; the election will be definitively closed.";
             ]) (W.election.e_params.e_uuid, ());
       ]
    | `EncryptedTally (npks, _, hash) ->
       lwt pds = Web_persist.get_partial_decryptions uuid_s in
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
       let trustees =
         List.map
           (fun (name, trustee_id) ->
             let service = election_tally_trustees in
             let x = (W.election.e_params.e_uuid, ((), trustee_id)) in
             let uri = rewrite_prefix @@ Eliom_uri.make_string_uri
               ~absolute:true ~service x
             in
             let link_content, dest = match name with
               | None -> uri, "toto@example.org"
               | Some name -> name, name
             in
             tr [
               td [
                 a_mailto ~dest ~body:uri link_content
               ];
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
             ]) (W.election.e_params.e_uuid, ())
       in
       return @@ div [
         div [
           pcdata "The ";
           a
             ~service:election_dir
             [pcdata "encrypted tally"]
             (W.election.e_params.e_uuid, ESETally);
           pcdata " has been computed. Its hash is ";
           b [pcdata hash];
           pcdata ".";
         ];
         div [
           div [pcdata "We are now waiting for trustees..."];
           table
             (tr [
               th [pcdata "Trustee link"];
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
  let div_archive = match state with
    | `Archived -> pcdata ""
    | _ -> div [
      br ();
      hr ();
      post_form ~service:election_archive (fun () ->
        [
          string_input ~input_type:`Submit ~value:"Archive election" ();
          pcdata " Warning: this action is irreversible. Archiving an election makes it read-only; in particular, the election will be definitively closed (no vote submission, no tally).";
        ]
      ) (W.election.e_params.e_uuid, ());
    ]
  in
  let uuid = W.election.e_params.e_uuid in
  let update_credential =
    match metadata.e_cred_authority with
    | Some "server" ->
       pcdata ""
    | _ ->
       div [
         a ~service:election_update_credential [pcdata "Update a credential"] (uuid, ());
       ];
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
    div [
      a ~service:election_regenpwd [pcdata "Regenerate and mail a password"] (uuid, ());
    ];
    div [state_div];
    div_archive;
  ] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let update_credential w () =
  let module W = (val w : ELECTION_DATA) in
  let params = W.election.e_params in
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
    ) (params.e_uuid, ())
  in
  let content = [
    form;
  ] in
  lwt login_box = site_login_box () in
  let uuid = W.election.e_params.e_uuid in
  base ~title:params.e_name ~login_box ~content ~uuid ()

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
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ~uuid ()

let cast_raw w () =
  let module W = (val w : ELECTION_DATA) in
  let params = W.election.e_params in
  let form_rawballot = post_form ~service:election_cast_post
    (fun (name, _) ->
      [
        div [pcdata "Please paste your raw ballot in JSON format in the following box:"];
        div [textarea ~a:[a_rows 10; a_cols 40] ~name ()];
        div [string_input ~input_type:`Submit ~value:"Submit" ()];
      ]
    ) (params.e_uuid, ())
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
    ) (params.e_uuid, ())
  in
  let intro = div [
    div [
      pcdata "You can create a raw ballot by using the command line tool ";
      pcdata "(available in the ";
      a ~service:source_code [pcdata "sources"] ();
      pcdata "), or its ";
      a ~service:(Eliom_service.static_dir ()) [
        pcdata "web interface";
      ] ["static"; "belenios-tool.html"];
      pcdata ". A specification of raw ballots is also available in the ";
      pcdata "sources.";
    ];
    div [
      a ~service:Web_services.election_home
        [pcdata "Back to election home"] (params.e_uuid, ());
    ];
  ] in
  let content = [
    intro;
    h3 [ pcdata "Submit by copy/paste" ];
    form_rawballot;
    h3 [ pcdata "Submit by file" ];
    form_upload;
  ] in
  lwt login_box = election_login_box w () in
  let uuid = W.election.e_params.e_uuid in
  let footer = audit_footer w in
  base ~title:params.e_name ~login_box ~content ~uuid ~footer ()

let cast_confirmation w hash () =
  let module W = (val w : ELECTION_DATA) in
  lwt user = Web_state.get_election_user W.election.e_params.e_uuid in
  let params = W.election.e_params in
  let name = params.e_name in
  let user_div = match user with
    | Some u ->
      post_form ~service:election_cast_confirm (fun () -> [
        p ~a:[a_style "text-align: center; padding: 10px;"] [
          pcdata "I am ";
          format_user ~site:false u;
          pcdata " and ";
          string_input
            ~a:[a_style "font-size: 20px; cursor: pointer;"]
            ~input_type:`Submit ~value:"I cast my vote" ();
          pcdata ".";
        ]
      ]) (params.e_uuid, ())
    | None ->
      div [
        pcdata "Please log in to confirm your vote.";
      ]
  in
  let progress = div ~a:[a_style "text-align:center;margin-bottom:20px;"] [
    pcdata "Input credential — Answer to questions — Review and encrypt";
    pcdata " — Authenticate — ";
    b [pcdata "Confirm"];
    pcdata " — Done";
    hr ();
  ] in
  let content = [
    progress;
    div ~a:[a_class ["current_step"]] [
        pcdata "Step 5/6: Confirm";
    ];
    p [
      pcdata "Your ballot for ";
      em [pcdata name];
      pcdata " has been received, but not recorded yet. ";
      pcdata "Your smart ballot tracker is ";
      b [pcdata hash];
      pcdata ".";
      br ();
    ];
    br ();
    p [pcdata "Note: your ballot is encrypted and nobody can see its contents."];
    user_div;
    p [
      (let service =
        Eliom_service.preapply
          Web_services.election_home (W.election.e_params.e_uuid, ())
      in
      a ~service [
        pcdata "Go back to election"
      ] ());
      pcdata ".";
    ];
  ] in
  let login_box = pcdata "" in
  let uuid = params.e_uuid in
  base ~title:name ~login_box ~content ~uuid ()

let cast_confirmed w ~result () =
  let module W = (val w : ELECTION_DATA) in
  let params = W.election.e_params in
  let name = params.e_name in
  let progress = div ~a:[a_style "text-align:center;margin-bottom:20px;"] [
    pcdata "Input credential — Answer to questions — Review and encrypt";
    pcdata " — Authenticate — Confirm — ";
    b [pcdata "Done"];
    hr ();
  ] in
  let result, step_title =
    match result with
    | `Valid hash ->
       [pcdata " has been accepted, your smart ballot tracker is ";
        b [pcdata hash];
        pcdata ". You can check its presence in the ";
        a ~service:election_pretty_ballots [pcdata "ballot box"] (params.e_uuid, ());
        pcdata " anytime during the election.";
        pcdata " A confirmation e-mail has been sent to you.";
       ], "Thank you for voting!"
    | `Error e ->
       [pcdata " is rejected, because ";
        pcdata (Web_common.explain_error e);
        pcdata ".";
       ], "FAIL!"
  in
  let content = [
    progress;
    div ~a:[a_class ["current_step"]] [
        pcdata "Step 6/6: ";
        pcdata step_title;
    ];
    p ([
      pcdata "Your ballot for ";
      em [pcdata name];
      ] @ result);
    p [
      (let service = Eliom_service.preapply logout () in
      a ~a:[a_id "logout"] ~service [
        pcdata "Log out and come back to election page"
      ] ());
      pcdata ".";
    ];
  ] in
  let login_box = pcdata "" in
  let uuid = params.e_uuid in
  base ~title:name ~login_box ~content ~uuid ()

let pretty_ballots w hashes result () =
  let module W = (val w : ELECTION_DATA) in
  let params = W.election.e_params in
  let title = params.e_name ^ " — Accepted ballots" in
  let nballots = ref 0 in
  let hashes =
    List.sort (fun a b -> String.(compare (uppercase a) (uppercase b))) hashes
  in
  let ballots =
    List.map
      (fun h ->
       incr nballots;
       li
         [a
            ~service:election_pretty_ballot
            [pcdata h]
            ((params.e_uuid, ()), h)]
      ) hashes
  in
  let links =
    p
      [a
         ~service:Web_services.election_home
         [pcdata "Back to election"]
         (params.e_uuid, ())]
  in
  let number = match !nballots, result with
    | n, None ->
       div [
         pcdata (string_of_int n);
         pcdata " ballot(s) have been accepted so far."
       ]
    | n, Some r when n = r.num_tallied ->
       div [
         pcdata (string_of_int n);
         pcdata " ballot(s) have been accepted."
       ]
    | n, Some r -> (* should not happen *)
       div [
         pcdata (string_of_int n);
         pcdata " ballot(s) have been accepted, and ";
         pcdata (string_of_int r.num_tallied);
         pcdata " have been tallied.";
       ]
  in
  let content = [
    number;
    ul ballots;
    links;
  ] in
  lwt login_box = election_login_box w () in
  let uuid = params.e_uuid in
  base ~title ~login_box ~content ~uuid ()

let pretty_records w records () =
  let module W = (val w : ELECTION_DATA) in
  let uuid = W.election.e_params.e_uuid in
  let title = W.election.e_params.e_name ^ " — Records" in
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
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let tally_trustees w trustee_id () =
  let module W = (val w : ELECTION_DATA) in
  let params = W.election.e_params in
  let title =
    params.e_name ^ " — Partial decryption #" ^ string_of_int trustee_id
  in
  let content = [
    p [pcdata "It is now time to compute your partial decryption factors."];
    p [
      pcdata "The hash of the encrypted tally is ";
      b [span ~a:[a_id "hash"] []];
      pcdata "."
    ];
    div ~a:[a_id "input_private_key"] [
      p [pcdata "Please enter your private key:"];
      input
        ~a:[a_id "private_key"; a_size 80]
        ~input_type:`Text
        ();
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
        ) (params.e_uuid, ((), trustee_id));
    ];
    div [
      script ~a:[a_src (uri_of_string (fun () -> "../../../static/sjcl.js"))] (pcdata "");
      script ~a:[a_src (uri_of_string (fun () -> "../../../static/jsbn.js"))] (pcdata "");
      script ~a:[a_src (uri_of_string (fun () -> "../../../static/jsbn2.js"))] (pcdata "");
      script ~a:[a_src (uri_of_string (fun () -> "../../../static/random.js"))] (pcdata "");
      script ~a:[a_src (uri_of_string (fun () -> "../../../static/tool_js_pd.js"))] (pcdata "");
    ]
  ] in
  let login_box = pcdata "" in
  let uuid = params.e_uuid in
  base ~title ~login_box ~content ~uuid ()

let already_logged_in () =
  let title = "Already logged in" in
  let content = [
    div [
      pcdata "You are already logged in as an administrator or on another election. You have to ";
      a ~service:logout [pcdata "log out"] ();
      pcdata " first."];
  ] in
  let login_box = pcdata "" in
  base ~title ~login_box ~content ()

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
  let login_box = pcdata "" in
  base ~title:"Log in" ~login_box ~content ()

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
  let login_box = pcdata "" in
  base ~title ~login_box ~content ()

let login_password () =
  let form = post_form ~service:password_post
    (fun (llogin, lpassword) ->
      [
        tablex [tbody [
          tr [
            th [label ~a:[a_for llogin] [pcdata "Username:"]];
            td [string_input ~a:[a_maxlength 50] ~input_type:`Text ~name:llogin ()];
          ];
          tr [
            th [label ~a:[a_for lpassword] [pcdata "Password:"]];
            td [string_input ~a:[a_maxlength 50] ~input_type:`Password ~name:lpassword ()];
          ];
        ]];
        div [
          string_input ~input_type:`Submit ~value:"Login" ();
        ]
      ]) ()
  in
  let content = [
    form;
  ] in
  let login_box = pcdata "" in
  base ~title:"Password login" ~login_box ~content ()
