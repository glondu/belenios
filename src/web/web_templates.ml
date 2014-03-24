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
open Signatures
open Util
open Serializable_t
open Web_serializable_t
open Web_signatures
open Web_common
open Eliom_content.Html5.F

(* TODO: these pages should be redesigned *)

let site_title = "Election Server"
let welcome_message = "Welcome!"
let admin_background = " background: #FF9999;"

let format_user u =
  em [pcdata (Web_auth.(string_of_user u))]

let make_login_box style auth =
  let style = "float: right; text-align: right;" ^ style in
  let module S = (val auth : AUTH_SERVICES) in
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
          a ~service:S.logout [pcdata "Log out"] ();
          pcdata ".";
        ];
      ]
    | None ->
      [
        div [
          pcdata "Not logged in.";
        ];
        let auth_systems = List.map (fun name ->
          let service = Eliom_service.preapply S.login (Some name) in
          a ~service [pcdata name] ()
        ) (S.get_auth_systems ()) in
        div (
          [ pcdata "Login: " ] @
          list_join (pcdata ", ") auth_systems @
          [ pcdata "." ]
        );
      ]
  )

module Make (S : SITE_SERVICES) : TEMPLATES = struct

  let site_login_box =
    let auth = (module S : AUTH_SERVICES) in
    fun () -> make_login_box admin_background auth

  let base ~title ~login_box ~content =
    Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang "en"]
      (head (Eliom_content.Html5.F.title (pcdata title)) [])
      (body [
        div ~a:[a_id "header"] [
          div [
            div ~a:[a_style "float: left;"] [
              a ~service:S.home [pcdata site_title] ();
            ];
            login_box;
            div ~a:[a_style "clear: both;"] [];
          ];
        ];
        div ~a:[a_id "content"] content;
        hr ();
        div ~a:[a_id "footer"; a_style "text-align: center;" ] [
          pcdata "Powered by ";
          a ~service:S.source_code [pcdata "Belenios"] ();
          pcdata ".";
        ]
       ]))

  let format_one_featured_election election =
    let module W = (val election : WEB_ELECTION_RO) in
    let e = W.election.e_params in
    li [
      h3 [
        a ~service:W.S.home [pcdata e.e_name] ();
      ];
      p [pcdata e.e_description];
    ]

  let home ~featured () =
    let featured_box = match featured with
      | _::_ ->
        div [
          h2 [pcdata "Current featured elections"];
          ul (List.map format_one_featured_election featured);
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
    lwt login_box = site_login_box () in
    base ~title:site_title ~login_box ~content

  module Login (S : AUTH_SERVICES) : LOGIN_TEMPLATES = struct

    let login_box =
      let auth = (module S : AUTH_SERVICES) in
      let style =
        if S.auth_realm = "site" then admin_background else ""
      in
      fun () -> make_login_box style auth

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

    let choose () =
      let content = [
        h1 [pcdata "Log in"];
        div [p [pcdata "Please choose one authentication system."]];
      ] in
      lwt login_box = login_box () in
      base ~title:"Log in" ~login_box ~content

  end

  let format_date (date, _) =
    CalendarLib.Printer.Precise_Fcalendar.sprint "%a, %d %b %Y %T %z" date

  let make_button ~service contents =
    let uri = Eliom_uri.make_string_uri ~service () in
    Printf.ksprintf unsafe_data (* FIXME: unsafe *)
      "<button onclick=\"location.href='%s';\">%s</button>"
      uri
      contents

  module Election (W : WEB_ELECTION_RO) = struct

    let election_login_box =
      let auth = (module W.S : AUTH_SERVICES) in
      fun () -> make_login_box "" auth

    let file x = Eliom_service.preapply W.S.election_dir x

    let home () =
      lwt user = W.S.get_user () in
      let params = W.election.e_params and m = W.metadata in
      lwt permissions =
        match user with
        | None ->
          Lwt.return [
            pcdata "Log in to check if you can vote. ";
            pcdata "Alternatively, you can try to vote and ";
            pcdata "log in at the last moment.";
          ]
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
      let audit_info = div [
        h3 [pcdata "Audit Info"];
        div [
          div [
            pcdata "Election fingerprint: ";
            code [ pcdata W.election.e_fingerprint ];
          ];
          div [
            pcdata "Election data: ";
            a ~service:(file ESRaw) [
              pcdata "parameters"
            ] ();
            pcdata ", ";
            a ~service:(file ESCreds) [
              pcdata "public credentials"
            ] ();
            pcdata ", ";
            a ~service:(file ESKeys) [
              pcdata "trustee public keys"
            ] ();
            pcdata ", ";
            a ~service:(file ESBallots) [
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
              ~service:W.S.election_vote
              "Go to the booth";
            pcdata " or ";
            make_button
              ~service:W.S.election_cast
              "Submit a raw ballot";
          ];
        ];
        br ();
        audit_info;
      ] in
      lwt login_box = election_login_box () in
      base ~title:params.e_name ~login_box ~content

    let update_credential () =
      let params = W.election.e_params in
      let form = post_form ~service:W.S.election_update_credential_post
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
        ) ()
      in
      let content = [
        h1 [ pcdata params.e_name ];
        form;
      ] in
      lwt login_box = election_login_box () in
      base ~title:params.e_name ~login_box ~content

    let cast_raw () =
      let params = W.election.e_params in
      let form_rawballot = post_form ~service:W.S.election_cast_post
        (fun (name, _) ->
          [
            div [pcdata "Please paste your raw ballot in JSON format in the following box:"];
            div [textarea ~a:[a_rows 10; a_cols 40] ~name ()];
            div [string_input ~input_type:`Submit ~value:"Submit" ()];
          ]
        ) ()
      in
      let form_upload = post_form ~service:W.S.election_cast_post
        (fun (_, name) ->
          [
            div [pcdata "Alternatively, you can also upload a file containing your ballot:"];
            div [
              pcdata "File: ";
              file_input ~name ();
            ];
            div [string_input ~input_type:`Submit ~value:"Submit" ()];
          ]
        ) ()
      in
      let content = [
        h1 [ pcdata params.e_name ];
        h3 [ pcdata "Submit by copy/paste" ];
        form_rawballot;
        h3 [ pcdata "Submit by file" ];
        form_upload;
      ] in
      lwt login_box = site_login_box () in
      base ~title:params.e_name ~login_box ~content

    let cast_confirmation ~confirm ~can_vote () =
      lwt user = W.S.get_user () in
      let params = W.election.e_params in
      let name = params.e_name in
      let user_div = match user with
        | Some u when can_vote ->
          let service = confirm () in
          post_form ~service (fun () -> [
            div [
              pcdata "I am ";
              format_user u;
              pcdata " and ";
              string_input ~input_type:`Submit ~value:"I confirm my vote" ();
              pcdata ".";
            ]
          ]) ()
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
          a ~service:W.S.home [
            pcdata "Go back to election"
          ] ();
          pcdata ".";
        ];
      ] in
      lwt login_box = election_login_box () in
      base ~title:name ~login_box ~content

    let cast_confirmed ~result () =
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
          a ~service:W.S.home [
            pcdata "Go back to election"
          ] ();
          pcdata ".";
        ];
      ] in
      lwt login_box = election_login_box () in
      base ~title:name ~login_box ~content

  end

end
