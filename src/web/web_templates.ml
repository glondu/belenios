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
let welcome_message = "Welcome!"
let admin_background = " background: #FF9999;"

let format_user u =
  em [pcdata (Web_auth.(string_of_user u))]

let make_login_box style auth links =
  let style = "float: right; text-align: right;" ^ style in
  let module S = (val auth : AUTH_SERVICES) in
  let module L = (val links : AUTH_LINKS) in
  lwt user = S.get_user () in
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
      [
        div [
          pcdata "Not logged in.";
        ];
        let auth_systems =
          S.get_auth_systems () |>
          List.map (fun name ->
            a ~service:(L.login (Some name)) [pcdata name] ()
          ) |> list_join (pcdata ", ")
        in
        div (
          [pcdata "Log in: ["] @ auth_systems @ [pcdata "]"]
        );
      ]
  )


  let site_login_box =
    let auth = (module Web_site_auth : AUTH_SERVICES) in
    let module L = struct
      let login x = Eliom_service.preapply site_login x
      let logout = Eliom_service.preapply site_logout ()
    end in
    let links = (module L : AUTH_LINKS) in
    fun () -> make_login_box admin_background auth links

  let base ~title ~login_box ~content =
    Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang "en"]
      (head (Eliom_content.Html5.F.title (pcdata title)) [
        script (pcdata "window.onbeforeunload = function () {};");
      ])
      (body [
        div ~a:[a_id "header"] [
          div [
            div ~a:[a_style "float: left;"] [
              a ~service:home [pcdata site_title] ();
            ];
            login_box;
            div ~a:[a_style "clear: both;"] [];
          ];
        ];
        div ~a:[a_id "content"] content;
        hr ();
        div ~a:[a_id "footer"; a_style "text-align: center;" ] [
          pcdata "Powered by ";
          a ~service:source_code [pcdata "Belenios"] ();
          pcdata ". ";
          a ~service:admin [pcdata "Administer elections"] ();
          pcdata ".";
        ]
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

  let home ~featured () =
    let featured_box = match featured with
      | _::_ ->
        div [
          h2 [pcdata "Current featured elections"];
          ul (List.map (format_election `Home) featured);
        ]
      | [] ->
        div [
          pcdata "No featured elections at the moment.";
        ]
    in
    let content = [
      h1 [pcdata site_title];
      div [
        pcdata welcome_message;
        featured_box;
      ];
    ] in
    let login_box = pcdata "" in
    base ~title:site_title ~login_box ~content

  let admin ~elections () =
    let title = site_title ^ " — Administration" in
    let elections =
      match elections with
      | [] -> p [pcdata "You cannot administer any elections!"]
      | _ -> ul @@ List.map (format_election `Admin) elections
    in
    let content = [
      h1 [pcdata title];
      div [
        div [a ~service:new_election [pcdata "Create a new election"] ()];
        div [a ~service:election_setup_index [pcdata "Elections being prepared"] ()];
        h2 [pcdata "Elections you can administer"];
        elections;
      ];
    ] in
    lwt login_box = site_login_box () in
    base ~title ~login_box ~content

  module Login (S : AUTH_SERVICES) (L : AUTH_LINKS) : LOGIN_TEMPLATES = struct

    let login_box =
      let auth = (module S : AUTH_SERVICES) in
      let links = (module L : AUTH_LINKS) in
      let style =
        if S.auth_realm = "site" then admin_background else ""
      in
      fun () -> make_login_box style auth links

    let dummy ~service () =
      let title, field_name, input_type =
        "Dummy login", "Username:", `Text
      in
      let form = post_form ~service
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
        h1 [pcdata title];
        form;
      ] in
      lwt login_box = login_box () in
      base ~title ~login_box ~content

    let password ~service () =
      let form = post_form ~service
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
        h1 [pcdata "Password login"];
        form;
      ] in
      lwt login_box = login_box () in
      base ~title:"Password login" ~login_box ~content

    let upload_password_db ~service () =
      let title = "Upload password database" in
      let form = post_form ~service
        (fun password_db ->
          [
            div [
              pcdata "Password database (CSV format): ";
              file_input ~name:password_db ();
            ];
            div [string_input ~input_type:`Submit ~value:"Submit" ()];
          ]
        ) ()
      in
      let content = [
        h1 [pcdata title];
        div [form];
      ] in
      lwt login_box = site_login_box () in
      base ~title ~login_box ~content

    let choose () =
      let auth_systems =
        S.get_auth_systems () |>
        List.map (fun name ->
          a ~service:(L.login (Some name)) [pcdata name] ()
        ) |> list_join (pcdata ", ")
      in
      let content = [
        h1 [pcdata "Log in"];
        div [p (
          [pcdata "Please log in: ["] @ auth_systems @ [pcdata "]"]
        )]
      ] in
      lwt login_box = login_box () in
      base ~title:"Log in" ~login_box ~content

  end

  let format_date = Platform.format_datetime "%a, %d %b %Y %T %z"

  let make_button ~service contents =
    let uri = Eliom_uri.make_string_uri ~service () in
    Printf.ksprintf Unsafe.data (* FIXME: unsafe *)
      "<button onclick=\"location.href='%s';\">%s</button>"
      uri
      contents

  let new_election () =
    let title = "Create new election" in
    lwt body =
      let form = post_form ~service:new_election_post
        (fun (election, (metadata, (public_keys, public_creds))) ->
          [
            h2 [pcdata "Import prepared election"];
            p [
              pcdata "This section assumes you have already prepared election files offline using either the command-line tool or its ";
              a ~service:tool [pcdata "web version"] ();
              pcdata ".";
            ];
            div [
              pcdata "Public election parameters: ";
              file_input ~name:election ();
            ];
            div [
              pcdata "Optional metadata: ";
              file_input ~name:metadata ()
            ];
            div [
              pcdata "Trustee public keys: ";
              file_input ~name:public_keys ()
            ];
            div [
              pcdata "Public credentials: ";
              file_input ~name:public_creds ()
            ];
            div [string_input ~input_type:`Submit ~value:"Submit" ()];
          ]
        ) ()
      in
      let setup_form = post_form ~service:election_setup_new
        (fun () ->
         [
           h2 [pcdata "Prepare a new election"];
           div [string_input ~input_type:`Submit ~value:"Prepare a new election" ()]
         ]
        ) ()
      in
      return [form; setup_form]
    in
    let content = [
      h1 [pcdata title];
      div body;
    ] in
    lwt login_box = site_login_box () in
    base ~title ~login_box ~content

  let new_election_failure reason () =
    let title = "Create new election" in
    let reason =
      match reason with
      | `Exists -> pcdata "An election with the same UUID already exists."
      | `Exception e -> pcdata @@ Printexc.to_string e
    in
    let content = [
      h1 [pcdata title];
      div [
        p [pcdata "The creation failed."];
        p [reason];
      ]
    ] in
    lwt login_box = site_login_box () in
    base ~title ~login_box ~content

  let election_setup_index uuids () =
    let service = election_setup in
    let title = "Elections being prepared" in
    let uuids =
      List.map (fun k ->
        li [a ~service [pcdata (Uuidm.to_string k)] k]
      ) uuids
    in
    let list =
      match uuids with
      | [] -> div [pcdata "You own no such elections."]
      | us -> ul us
    in
    let content = [
      h1 [pcdata title];
      div [list];
    ] in
    lwt login_box = site_login_box () in
    base ~title ~login_box ~content

  let generic_error_page message () =
    let title = "Error" in
    let content = [
      h1 [pcdata title];
      p [pcdata message];
    ] in
    let login_box = pcdata "" in
    base ~title ~login_box ~content

  let election_setup uuid se () =
    let title = "Preparation of election " ^ Uuidm.to_string uuid in
    let make_form service value title =
      post_form ~service
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
    let form_group =
      make_form
        (Eliom_service.preapply election_setup_group uuid)
        se.se_group "Group parameters"
    in
    let form_metadata =
      let value = string_of_metadata se.se_metadata in
      make_form
        (Eliom_service.preapply election_setup_metadata uuid)
        value "Election metadata"
    in
    let form_questions =
      let value = string_of_template se.se_questions in
      make_form
        (Eliom_service.preapply election_setup_questions uuid)
        value "Questions"
    in
    let form_trustees =
      post_form
        ~service:election_setup_trustee_add
        (fun () ->
         [div
            [h2 [pcdata "Trustees"];
             ol
               (List.rev_map
                  (fun (token, pk) ->
                   li
                     [a ~service:election_setup_trustee [pcdata token] token]
                  ) se.se_public_keys
               );
             string_input ~input_type:`Submit ~value:"Add" ()]]) uuid
    in
    let div_credentials =
      div
        [h2 [pcdata "Credentials"];
         a
           ~service:election_setup_credentials
           [pcdata "Manage credentials"]
           se.se_public_creds]
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
      h1 [pcdata title];
      form_trustees;
      div_credentials;
      form_group;
      form_metadata;
      form_questions;
      form_create;
    ] in
    lwt login_box = site_login_box () in
    base ~title ~login_box ~content

  let election_setup_credentials token uuid se () =
    let title = "Credentials for election " ^ uuid in
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
    let form_file =
      post_form
        ~service:election_setup_credentials_post_file
        (fun name ->
         [div
            [h2 [pcdata "Submit by file"];
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
    let div_textarea = div [group; interactivity; form_textarea] in
    let content = [
      h1 [pcdata title];
      div_download;
      div_textarea;
      form_file;
    ] in
    let login_box = pcdata "" in
    base ~title ~login_box ~content

  let election_setup_trustee token uuid se () =
    let title = "Trustee for election " ^ uuid in
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
      h1 [pcdata title];
      group;
      interactivity;
      form;
    ] in
    let login_box = pcdata "" in
    base ~title ~login_box ~content


    let election_login_box w =
      let module W = (val w : WEB_ELECTION_) in
      let auth = (module W.S : AUTH_SERVICES) in
      let module L = struct
        let login x =
          Eliom_service.preapply
            election_login
            ((W.election.e_params.e_uuid, ()), x)
        let logout =
          Eliom_service.preapply
            election_logout
            (W.election.e_params.e_uuid, ())
      end in
      let links = (module L : AUTH_LINKS) in
      fun () -> make_login_box "" auth links

    let file w x =
      let module W = (val w : WEB_ELECTION_) in
      Eliom_service.preapply
        election_dir
        (W.election.e_params.e_uuid, x)

    let election_home w () =
      let module W = (val w : WEB_ELECTION_) in
      lwt user = W.S.get_user () in
      let params = W.election.e_params and m = W.metadata in
      lwt permissions =
        match user with
        | None ->
          (match m.e_voters with
          | Some `Any ->
            return [
              pcdata "Anybody can vote in this election.";
            ]
          | Some _ ->
            return [
              pcdata "Log in to check if you can vote. ";
              pcdata "Alternatively, you can try to vote and ";
              pcdata "log in at the last moment.";
            ]
          | None ->
            return [
              pcdata "Currently, nobody can vote in this election.";
            ]
          )
        | Some u ->
          let can = if check_acl m.e_voters u then "can" else "cannot" in
          Lwt.return [
            pcdata "You ";
            pcdata can;
            pcdata " vote in this election.";
          ]
      in
      let voting_period =
        match m.e_voting_starts_at, m.e_voting_ends_at with
        | None, None ->
          [
            pcdata "This election starts and ends at the administrator's discretion."
          ]
        | Some s, None ->
          [
            pcdata "This election starts on ";
            em [pcdata (format_date s)];
            pcdata " and ends at the administrator's discretion.";
          ]
        | None, Some s ->
          [
            pcdata "This election starts at the administrator's discretion and ends on ";
            em [pcdata (format_date s)];
            pcdata ".";
          ]
        | Some s, Some e ->
          [
            pcdata "This election starts on ";
            em [pcdata (format_date s)];
            pcdata " and ends on ";
            em [pcdata (format_date e)];
            pcdata ".";
          ]
      in
      let ballots_link =
        p [
            a ~service:election_pretty_ballots [
                pcdata "List of accepted ballots"
              ] ((params.e_uuid, ()), 1)
          ]
      in
      let audit_info = div [
        h3 [pcdata "Audit Info"];
        div [
          div [
            pcdata "Election fingerprint: ";
            code [ pcdata W.election.e_fingerprint ];
          ];
          div [
            pcdata "Election data: ";
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
      let content = [
        h1 [ pcdata params.e_name ];
        p ~a:[a_style "margin: 1em; padding: 2pt; font-style: italic; border: 1pt solid;"] [
          pcdata params.e_description
        ];
        p voting_period;
        p permissions;
        div [
          div [
            make_button
              ~service:(Eliom_service.preapply election_vote (params.e_uuid, ()))
              "Go to the booth";
            pcdata " or ";
            make_button
              ~service:(Eliom_service.preapply election_cast (params.e_uuid, ()))
              "Submit a raw ballot";
          ];
        ];
        ballots_link;
        br ();
        audit_info;
      ] in
      lwt login_box = election_login_box w () in
      base ~title:params.e_name ~login_box ~content

    let election_admin w ~is_featured () =
      let module W = (val w : WEB_ELECTION_) in
      let title = W.election.e_params.e_name ^ " — Administration" in
      let feature_form = post_form ~service:election_set_featured
        (fun featured -> [
          bool_checkbox ~name:featured ~checked:is_featured ();
          pcdata "Feature this election ";
          string_input ~input_type:`Submit ~value:"Apply" ();
        ]) (W.election.e_params.e_uuid, ())
      in
      let uuid = W.election.e_params.e_uuid in
      let content = [
        h1 [pcdata title];
        div [
          a ~service:Web_services.election_home [pcdata "Election home"] (uuid, ());
        ];
        div [
          a ~service:election_update_credential [pcdata "Update a credential"] (uuid, ());
        ];
        div [
          a ~service:election_dir [pcdata "Voting records"] (uuid, ESRecords);
        ];
        div [feature_form];
      ] in
      lwt login_box = site_login_box () in
      base ~title ~login_box ~content

    let update_credential w () =
      let module W = (val w : WEB_ELECTION_) in
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
        h1 [ pcdata params.e_name ];
        form;
      ] in
      lwt login_box = site_login_box () in
      base ~title:params.e_name ~login_box ~content

    let cast_raw w () =
      let module W = (val w : WEB_ELECTION_) in
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
        h1 [ pcdata params.e_name ];
        h3 [ pcdata "Submit by copy/paste" ];
        form_rawballot;
        h3 [ pcdata "Submit by file" ];
        form_upload;
      ] in
      lwt login_box = election_login_box w () in
      base ~title:params.e_name ~login_box ~content

    let cast_confirmation w ~can_vote () =
      let module W = (val w : WEB_ELECTION_) in
      lwt user = W.S.get_user () in
      let params = W.election.e_params in
      let name = params.e_name in
      let user_div = match user with
        | Some u when can_vote ->
          post_form ~service:election_cast_confirm (fun () -> [
            div [
              pcdata "I am ";
              format_user u;
              pcdata " and ";
              string_input ~input_type:`Submit ~value:"I confirm my vote" ();
              pcdata ".";
            ]
          ]) (params.e_uuid, ())
        | Some _ ->
          div [
            pcdata "You cannot vote in this election!";
          ]
        | None ->
          div [
            pcdata "Please log in to confirm your vote.";
          ]
      in
      let content = [
        h1 [ pcdata name ];
        p [
          pcdata "Your ballot for ";
          em [pcdata name];
          pcdata " has been received, but not recorded yet.";
        ];
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
      lwt login_box = election_login_box w () in
      base ~title:name ~login_box ~content

    let cast_confirmed w ~result () =
      let module W = (val w : WEB_ELECTION_) in
      let params = W.election.e_params in
      let name = params.e_name in
      let content = [
        h1 [ pcdata name ];
        p [
          pcdata "Your ballot for ";
          em [pcdata name];
          (match result with
             | `Valid hash -> pcdata (" has been accepted, its hash is " ^ hash ^ ".")
             | `Error e -> pcdata (" is rejected, because " ^ Web_common.explain_error e ^ ".")
          );
        ];
        p [
          (let service =
            Eliom_service.preapply
              election_logout (params.e_uuid, ())
          in
          a ~service [
            pcdata "Log out"
          ] ());
          pcdata ".";
        ];
      ] in
      lwt login_box = election_login_box w () in
      base ~title:name ~login_box ~content

let pretty_ballots w hashes () =
  let module W = (val w : WEB_ELECTION_) in
  let params = W.election.e_params in
  let title = params.e_name ^ " — Accepted ballots" in
  let nballots = ref 0 in
  let ballots =
    List.rev_map
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
    h1 [pcdata title];
    ul ballots;
    links;
  ] in
  lwt login_box = election_login_box w () in
  base ~title ~login_box ~content
