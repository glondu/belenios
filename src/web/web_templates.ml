(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

let format_user u =
  em [pcdata (string_of_user u)]

let make_login_box ~show_login style auth links =
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
          format_user user;
          pcdata ".";
        ];
        div [
          a ~service:L.logout [pcdata "Log out"] ();
          pcdata ".";
        ];
      ]
    | None ->
       if show_login then
      [
        div [
          pcdata "Not logged in.";
        ];
        let auth_systems =
          auth_systems |>
          List.map (fun name ->
            a ~service:(L.login (Some name)) [pcdata name] ()
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
  let get_user () = Web_auth_state.get_site_user ()
  let get_auth_systems () =
    lwt l = Web_auth_state.get_config None in
    return (List.map fst l)
end

let site_links = (module Site_links : AUTH_LINKS)
let site_auth = (module Site_auth : AUTH_SERVICES)

let site_login_box () =
  make_login_box ~show_login:true admin_background site_auth site_links

let belenios_url = Eliom_service.Http.external_service
  ~prefix:"http://belenios.gforge.inria.fr"
  ~path:[]
  ~get_params:Eliom_parameter.unit
  ()

let base ~title ~login_box ~content ?(footer = div []) () =
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
          a ~service:admin [pcdata "Administer elections"] ();
          pcdata ".";
        ]
      ]]
     ]))

let format_election kind election =
  let module W = (val election : WEB_ELECTION) in
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
  | Some (elections, tallied, setup_elections) ->
    let setup_form = post_form ~service:election_setup_new
      (fun () ->
       [
         div [string_input ~a:[a_style "font-size:24px;"] ~input_type:`Submit ~value:"Prepare a new election" ()]
       ]
      ) ()
    in
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
        div [setup_form];
        div [br ()];
        h2 [pcdata "Elections being prepared"];
        setup_elections;
        div [br ()];
        h2 [pcdata "Elections you can administer"];
        elections;
        div [br ()];
        h2 [pcdata "Tallied elections"];
        tallied;
      ];
    ] in
    lwt login_box = site_login_box () in
    base ~title ~login_box ~content ()

let format_date = Platform.format_datetime "%a, %d %b %Y %T %z"

let make_button ~service contents =
  let uri = Eliom_uri.make_string_uri ~service () in
  Printf.ksprintf Unsafe.data (* FIXME: unsafe *)
    "<button onclick=\"location.href='%s';\" style=\"font-size:35px;\">%s</button>"
    uri
    contents

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

let generic_page ~title message () =
  let content = [
    p [pcdata message];
  ] in
  let login_box = pcdata "" in
  base ~title ~login_box ~content ()

let election_setup uuid se () =
  let title = "Preparation of election " ^ se.se_questions.t_name in
  let make_form ?a service value title =
    post_form ?a ~service
      (fun name ->
       [
         div [
           h2 [pcdata title];
           div [textarea ~a:[a_rows 5; a_cols 80] ~name ~value ()];
           div [string_input ~input_type:`Submit ~value:"Submit" ()];
         ]
       ]
      ) ()
  in
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
            string_input ~input_type:`Submit ~value:"Submit" ();
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
  let form_group =
    make_form
      ~a:[a_style "display: none;"]
      (Eliom_service.preapply election_setup_group uuid)
      se.se_group "Group parameters"
  in
  let form_metadata =
    let value = string_of_metadata se.se_metadata in
    make_form
      ~a:[a_style "display: none;"]
      (Eliom_service.preapply election_setup_metadata uuid)
      value "Election metadata"
  in
  let form_auth =
    let checked_dummy, checked_password, checked_cas =
      match se.se_metadata.e_auth_config with
      | Some [x] ->
         (match x.auth_system with
         | "dummy" -> true, false, false
         | "password" -> false, true, false
         | "cas" -> false, false, true
         | _ -> false, false, false)
      | _ -> false, false, false
    in
    post_form ~service:election_setup_auth
      (fun name ->
        [
          div [
            string_radio ~checked:checked_password ~name ~value:"password" ();
            pcdata "Password (passwords will be emailed to voters)";
          ];
          div [
            string_radio ~checked:checked_dummy ~name ~value:"dummy" ();
            pcdata "Dummy (typically for a test election)";
          ];
          div [
            string_radio ~checked:checked_cas ~name ~value:"cas" ();
            pcdata "CAS (external authentication server)";
          ];
          div [
            string_input ~input_type:`Submit ~value:"Change authentication mode" ();
          ];
        ])
      uuid
  in
  let form_cas =
    match se.se_metadata.e_auth_config with
    | Some [x] ->
       (match x.auth_system with
       | "cas" ->
          let value =
            match x.auth_config with
            | ["server", x] -> x
            | _ -> ""
          in
          post_form ~service:election_setup_auth_cas
            (fun name ->
              [
                div [
                  pcdata "CAS server address: ";
                  string_input ~name ~input_type:`Text ~a:[a_size 40] ~value ();
                  string_input ~input_type:`Submit ~value:"Submit" ();
                ]
              ]) uuid
       | _ -> pcdata "")
    | _ -> pcdata ""
  in
  let form_password =
    match se.se_metadata.e_auth_config with
    | Some [x] ->
       (match x.auth_system with
       | "password" ->
          post_form ~service:election_setup_auth_genpwd
            (fun () ->
              [div [
                string_input ~input_type:`Submit ~value:"Generate passwords" ()
              ]]
            ) uuid
       | _ -> pcdata "")
    | _ -> pcdata ""
  in
  let div_questions =
    div [
      h2 [
        a
          ~service:election_setup_questions
          [pcdata "Edit questions"]
          uuid;
      ]
    ]
  in
  let div_voters =
    div [
      h2 [
        a ~service:election_setup_voters
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
      div [
        pcdata "The server may generate and email the credentials to the voters. If you prefer to delegate this task to another authority, click ";
        a ~service:election_setup_credential_authority [pcdata "here"] uuid;
        pcdata ".";
      ];
      post_form ~service:election_setup_credentials_server
        (fun () ->
          [string_input ~input_type:`Submit ~value:"Generate on server" ()]
        ) uuid;
    ]
  in
  let form_create =
    post_form
      ~service:election_setup_create
      (fun () ->
       [div
          [h2 [pcdata "Finalize creation"];
           string_input ~input_type:`Submit ~value:"Create election" ()]]
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
    form_group;
    form_metadata;
    div [
      h2 [pcdata "Authentication"];
      form_auth;
      form_cas;
      form_password;
    ];
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
      (fun () ->
        [string_input ~input_type:`Submit ~value:"Add" ()]) uuid
  in
  let form_trustees_del =
    post_form
      ~service:election_setup_trustee_del
      (fun () ->
        [string_input ~input_type:`Submit ~value:"Delete" ()]) uuid
  in
  let div_content =
    div [
      div [pcdata "If you do not wish the server to store any keys, you may nominate trustees. In that case, each trustee will create her own secret key. Be careful, once the election is over, you will need the contribution of each trustee to compute the result!"];
      br ();
      ol
        (List.rev_map
           (fun (token, pk) ->
             li [
               a ~service:election_setup_trustee [
                 pcdata @@ rewrite_prefix @@ Eliom_uri.make_string_uri
                   ~absolute:true
                   ~service:election_setup_trustee
                   token
               ] token
             ];
           ) se.se_public_keys
        );
      (if se.se_public_keys <> [] then
          div [
            pcdata "There is one link per trustee. Send each trustee her link.";
            br ();
            br ();
          ]
       else pcdata "");
      form_trustees_add;
      form_trustees_del;
    ]
  in
  let content = [div_content] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let election_setup_credential_authority uuid se () =
  let title = "Credentials for election " ^ se.se_questions.t_name in
  let content = [
    div [
      pcdata "If you wish the credentials to be generated and managed by ";
      pcdata "an external authority, please send her the following link:";
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
         div [string_input ~input_type:`Submit ~value:"Submit" ()]])
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
      ~service:election_setup_voters_post
      (fun name ->
        let value = String.concat "\n" se.se_voters in
        [
          div [textarea ~a:[a_rows 20; a_cols 50] ~name ~value ()];
          div [string_input ~input_type:`Submit ~value:"Submit" ()]])
      uuid
  in
  let content = [
    div [pcdata "Please enter the email addresses of the voters, one per line."];
    form;
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
    let value = String.concat "\n" se.se_voters in
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
  let content = [
    div_download;
    div_textarea;
    form_file;
  ] in
  let login_box = pcdata "" in
  base ~title ~login_box ~content ()

let election_setup_trustee token uuid se () =
  let title = "Trustee for election " ^ se.se_questions.t_name in
  let form =
    let value = !(List.assoc token se.se_public_keys) in
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


let election_login_box w =
  let module W = (val w : WEB_ELECTION) in
  let module A = struct
    let get_user () =
      Web_auth_state.get_election_user W.election.e_params.e_uuid
    let get_auth_systems () =
      lwt l = Web_auth_state.get_config (Some W.election.e_params.e_uuid) in
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
  fun () -> make_login_box ~show_login:false "" auth links

let file w x =
  let module W = (val w : WEB_ELECTION) in
  Eliom_service.preapply
    election_dir
    (W.election.e_params.e_uuid, x)

let election_home w state () =
  let module W = (val w : WEB_ELECTION) in
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
         pcdata " The result with ";
         a
           ~service:election_dir
           [pcdata "cryptographic proofs"]
           (W.election.e_params.e_uuid, ESResult);
         pcdata " is available."
       ]
  in
  let ballots_link =
    p ~a:[a_style "text-align:center;"] [
        a
          ~a:[a_style "font-size:25px;"]
          ~service:election_pretty_ballots [
            pcdata "See accepted ballots"
          ] ((params.e_uuid, ()), 1)
      ]
  in
  let footer = div ~a:[a_style "line-height:1.5em;"] [
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
  ] in
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
  let middle =
    match state with
    | `Tallied result ->
       let questions = Array.to_list W.election.e_params.e_questions in
       ul (List.mapi (fun i x ->
         let answers = Array.to_list x.q_answers in
         let answers = List.mapi (fun j x ->
           tr [td [pcdata x]; td [pcdata @@ string_of_int result.(i).(j)]]
         ) answers in
         let answers =
           match answers with
           | [] -> pcdata ""
           | x :: xs -> table x xs
         in
         li [
           pcdata x.q_question;
           answers;
         ]
       ) questions)
    | _ -> go_to_the_booth
  in
  let content = [
    p state_;
    br ();
    middle;
    br ();
    ballots_link;
  ] in
  lwt login_box = election_login_box w () in
  base ~title:params.e_name ~login_box ~content ~footer ()

let election_admin w state () =
  let module W = (val w : WEB_ELECTION) in
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
         post_form
           ~service:election_compute_encrypted_tally
           (fun () ->
             [string_input
                 ~input_type:`Submit
                 ~value:"Tally the election"
                 ();
              pcdata " (Warning, this action is irreversible.)";
             ]) (W.election.e_params.e_uuid, ());
       ]
    | `EncryptedTally (npks, _, hash) ->
       let rec seq a b =
         if a <= b then a :: (seq (a+1) b) else []
       in
       lwt pds = Web_persist.get_partial_decryptions uuid_s in
       let trustees =
         List.map
           (fun trustee_id ->
             tr [
               td [
                 a
                   ~service:election_tally_trustees
                   [
                     pcdata @@ rewrite_prefix @@ Eliom_uri.make_string_uri
                       ~absolute:true
                       ~service:election_tally_trustees
                       (W.election.e_params.e_uuid, ((), trustee_id))
                   ]
                   (W.election.e_params.e_uuid, ((), trustee_id))
               ];
               td [
                 pcdata (if List.mem_assoc trustee_id pds then "Yes" else "No")
               ];
             ]
           ) (seq 1 npks)
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
               td [pcdata "Trustee link"];
               td [pcdata "Done?"];
             ])
             trustees
         ];
         release_form;
       ]
    | `Tallied _ ->
       return @@ div [
         pcdata "This election has been tallied.";
       ]
  in
  let uuid = W.election.e_params.e_uuid in
  let update_credential =
    match W.metadata.e_cred_authority with
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
      a ~service:election_dir [pcdata "Voting records"] (uuid, ESRecords);
    ];
    div [
      a ~service:election_missing_voters [pcdata "Missing voters"] (uuid, ());
    ];
    div [state_div];
  ] in
  lwt login_box = site_login_box () in
  base ~title ~login_box ~content ()

let update_credential w () =
  let module W = (val w : WEB_ELECTION) in
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
  base ~title:params.e_name ~login_box ~content ()

let cast_raw w () =
  let module W = (val w : WEB_ELECTION) in
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
  let content = [
    h3 [ pcdata "Submit by copy/paste" ];
    form_rawballot;
    h3 [ pcdata "Submit by file" ];
    form_upload;
  ] in
  lwt login_box = election_login_box w () in
  base ~title:params.e_name ~login_box ~content ()

let cast_confirmation w hash () =
  let module W = (val w : WEB_ELECTION) in
  lwt user = Web_auth_state.get_election_user W.election.e_params.e_uuid in
  let params = W.election.e_params in
  let name = params.e_name in
  let user_div = match user with
    | Some u ->
      post_form ~service:election_cast_confirm (fun () -> [
        p ~a:[a_style "text-align: center; padding: 10px;"] [
          pcdata "I am ";
          format_user u;
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
  base ~title:name ~login_box ~content ()

let cast_confirmed w ~result () =
  let module W = (val w : WEB_ELECTION) in
  let params = W.election.e_params in
  let name = params.e_name in
  let progress = div ~a:[a_style "text-align:center;margin-bottom:20px;"] [
    pcdata "Input credential — Answer to questions — Review and encrypt";
    pcdata " — Authenticate — Confirm — ";
    b [pcdata "Done"];
    hr ();
  ] in
  let result =
    match result with
    | `Valid hash ->
       [pcdata " has been accepted, your smart ballot tracker is ";
        b [pcdata hash];
        pcdata ". We invite you to save it and check its presence in the ";
        a ~service:election_pretty_ballots [pcdata "ballot box"] ((params.e_uuid, ()), 1);
        pcdata ".";
       ]
    | `Error e ->
       [pcdata " is rejected, because ";
        pcdata (Web_common.explain_error e);
        pcdata ".";
       ]
  in
  let content = [
    progress;
    p ([
      pcdata "Your ballot for ";
      em [pcdata name];
      ] @ result);
    p [
      (let service = Eliom_service.preapply logout () in
      a ~service [
        pcdata "Log out and come back to election page"
      ] ());
      pcdata ".";
    ];
  ] in
  let login_box = pcdata "" in
  base ~title:name ~login_box ~content ()

let pretty_ballots w hashes () =
  let module W = (val w : WEB_ELECTION) in
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
  let content = [
    p [pcdata "This is the list of ballots accepted so far."];
    ul ballots;
    links;
  ] in
  lwt login_box = election_login_box w () in
  base ~title ~login_box ~content ()

let tally_trustees w trustee_id () =
  let module W = (val w : WEB_ELECTION) in
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
  base ~title ~login_box ~content ()

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
  let form = post_form ~service:Web_auth_state.dummy_post
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
  let form = post_form ~service:Web_auth_state.password_post
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
