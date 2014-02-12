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

open Signatures
open Util
open Serializable_t
open Eliom_content.Html5.F

(* TODO: these pages should be redesigned *)

let site_title = "Election Server"
let welcome_message = "Welcome!"

let format_user u =
  em [pcdata (Web_common.string_of_user u)]

let base ~auth_systems ~title ~content =
  lwt user = Eliom_reference.get Services.user in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang "en"]
    (head (Eliom_content.Html5.F.title (pcdata title)) [])
    (body [
      div ~a:[a_id "header"] [
        div [
          div ~a:[a_style "float: left;"] [
            a ~service:Services.home [pcdata site_title] ();
          ];
          div ~a:[a_style "float: right; text-align: right;"] (
            match user with
            | Some user ->
              [
                div [
                  pcdata "Logged in as ";
                  format_user user;
                  pcdata ".";
                ];
                div [
                  a ~service:Services.logout [pcdata "Log out"] ();
                  pcdata ".";
                ];
              ]
            | None ->
              [
                div [
                  pcdata "Not logged in.";
                ];
                let auth_systems = List.map (fun (name, service) ->
                  a ~service [pcdata name] ()
                ) auth_systems in
                div (
                  [ pcdata "Login: " ] @
                  list_join (pcdata ", ") auth_systems @
                  [ pcdata "." ]
                );
              ]
          );
          div ~a:[a_style "clear: both;"] [];
        ];
      ];
      div ~a:[a_id "content"] content;
      hr ();
      div ~a:[a_id "footer"; a_style "text-align: center;" ] [
        pcdata "Powered by ";
        a ~service:Services.source_code [pcdata "Belenios"] ();
        pcdata ".";
      ]
     ]))

type answer = {
  count : int;
  answer : string;
  winner : bool;
}

type question = {
  answers : answer list;
  question : string;
}

let format_election_result e r =
  let open Services in
  Array.mapi (fun i q ->
    let q' = e.e_questions.(i) in
    let question = q'.q_question in
    let answers = Array.mapi (fun j a ->
      let answer = q'.q_answers.(j) in
      let count = a in
      (answer, count)
    ) q |> Array.to_list
    in
    let (winners, _) = List.fold_left
      (fun (ws, v) ((_, c) as w) ->
        if c > v then ([w], c)
        else if c = v then (w::ws, v)
        else (ws, v)
      ) ([], 0) answers
    in
    let answers = List.map
      (fun ((answer, count) as x) ->
        let winner = List.memq x winners in
        { answer; count; winner }
      ) answers
    in
    { question; answers }
  ) r.result |>
  Array.to_list

let format_one_featured_election e =
  li [
    h3 [
      a ~service:Services.(preapply_uuid election_index e)
        [pcdata e.e_name] ();
    ];
    p [pcdata e.e_description];
  ]

let index ~auth_systems ~featured =
  lwt user = Eliom_reference.get Services.user in
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
  base ~auth_systems ~title:site_title ~content

let string_login ~kind ~service =
  let title, field_name, input_type = match kind with
    | `Dummy -> "Dummy login", "Username:", `Text
    | `Admin -> "Admin login", "Admin password:", `Password
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
  base ~title ~content

let password_login ~service =
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
  base ~title:"Password login" ~content

let format_date (date, _) =
  CalendarLib.Printer.Precise_Fcalendar.sprint "%a, %d %b %Y %T %z" date

let make_button ~service contents =
  let uri = Eliom_uri.make_string_uri ~service () in
  Printf.ksprintf unsafe_data (* FIXME: unsafe *)
    "<button onclick=\"location.href='%s';\">%s</button>"
    uri
    contents

let election_view ~auth_systems ~election ~user =
  let open Web_common in
  let params = election.election.e_params in
  let service = Services.(election_file params ESRaw) in
  lwt permissions =
    match election.election_web.can_vote with
      | Any ->
        Lwt.return [ pcdata "Anyone can vote in this election." ]
      | Restricted p ->
        match user with
          | None ->
            Lwt.return [
              pcdata "Log in to check if you can vote. Alternatively, you can try to vote and log in at the last moment.";
            ]
          | Some u ->
            lwt b = p u in
            let can = if b then pcdata "can" else pcdata "cannot" in
            Lwt.return [
              pcdata "You ";
              can;
              pcdata " vote in this election.";
            ]
  in
  let voting_period = match election.election.e_meta with
    | Some m ->
      [
        pcdata "This election starts on ";
        em [pcdata (format_date m.e_voting_starts_at)];
        pcdata " and ends on ";
        em [pcdata (format_date m.e_voting_ends_at)];
        pcdata ".";
      ]
    | None ->
      [
        pcdata "This election starts and ends at the administrator's discretion."
      ]
  in
  let audit_info = div [
    h3 [pcdata "Audit Info"];
    div [
      div [
        pcdata "Election fingerprint: ";
        code [ pcdata election.election.e_fingerprint ];
      ];
      div [
        pcdata "Election data: ";
        a ~service [ pcdata "parameters" ] ();
        pcdata ", ";
        a ~service:Services.(election_file params ESCreds) [
          pcdata "public credentials"
        ] ();
        pcdata ", ";
        a ~service:Services.(election_file params ESKeys) [
          pcdata "trustee public keys"
        ] ();
        pcdata ", ";
        a ~service:Services.(election_file params ESBallots) [
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
          ~service:(Services.(preapply_uuid election_vote params))
          "Go to the booth";
        pcdata " or ";
        make_button
          ~service:(Services.(preapply_uuid election_cast params))
          "Submit a raw ballot";
      ];
    ];
    br ();
    audit_info;
  ] in
  base ~auth_systems ~title:params.e_name ~content

let election_cast_raw ~election =
  let open Web_common in
  let params = election.election.e_params in
  let form_rawballot = post_form ~service:Services.election_cast_post
    (fun (name, _) ->
      [
        div [pcdata "Please paste your raw ballot in JSON format in the following box:"];
        div [textarea ~a:[a_rows 10; a_cols 40] ~name ()];
        div [string_input ~input_type:`Submit ~value:"Submit" ()];
      ]
    ) params.e_uuid
  in
  let form_upload = post_form ~service:Services.election_cast_post
    (fun (_, name) ->
      [
        div [pcdata "Alternatively, you can also upload a file containing your ballot:"];
        div [
          pcdata "File: ";
          file_input ~name ();
        ];
        div [string_input ~input_type:`Submit ~value:"Submit" ()];
      ]
    ) params.e_uuid
  in
  let content = [
    h1 [ pcdata params.e_name ];
    h3 [ pcdata "Submit by copy/paste" ];
    form_rawballot;
    h3 [ pcdata "Submit by file" ];
    form_upload;
  ] in
  base ~title:params.e_name ~content

let ballot_received ~election ~confirm ~user ~can_vote =
  let open Web_common in
  let params = election.election.e_params in
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
      ]) params.e_uuid
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
      a ~service:(Services.(preapply_uuid election_index params)) [
        pcdata "Go back to election"
      ] ();
      pcdata ".";
    ];
  ] in
  base ~title:name ~content

let do_cast_ballot ~election ~result =
  let params = election.Web_common.election.e_params in
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
      a ~service:(Services.(preapply_uuid election_index params)) [
        pcdata "Go back to election"
      ] ();
      pcdata ".";
    ];
  ] in
  base ~title:name ~content

let election_update_credential ~election =
  let params = election.Web_common.election.e_params in
  let form = post_form ~service:Services.election_update_credential
    (fun (old, new_) ->
      [
        div [
          p [
            pcdata "\
This form allows you to change a single credential at a time. To get \
the hash of a credential, run the following command:";
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
    ) params.e_uuid
  in
  let content = [
    h1 [ pcdata params.e_name ];
    form;
  ] in
  base ~title:params.e_name ~content
