open Util
open Serializable_t
open Eliom_content.Html5.F

(* TODO: these pages should be redesigned *)

let site_title = "Election Server"
let welcome_message = "Welcome!"

let format_user u =
  let open Common in
  Printf.ksprintf pcdata "%s:%s" u.user_type u.user_name

let base ~title ~content =
  lwt user = Eliom_reference.get Services.user in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang "en"]
    (head (Eliom_content.Html5.F.title (pcdata title)) [])
    (body [
      div ~a:[a_id "header"] [
        div [
          div [
            a ~service:Services.home [pcdata site_title] ();
          ];
          div (
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
                div [
                  pcdata "Login: ";
                  a ~service:Services.login [pcdata "dummy"] ();
                  pcdata ", ";
                  a ~service:Services.login_cas [pcdata "CAS"] None;
                  pcdata ".";
                ];
              ]
          );
        ];
      ];
      div ~a:[a_id "content"] content;
      div ~a:[a_id "footer"] []
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
      a ~service:Services.(preapply_uuid election_view e)
        [pcdata e.Common.election.e_name] ();
    ];
    p [pcdata e.Common.election.e_description];
  ]

let index ~featured =
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
  base ~title:site_title ~content

let dummy_login ~service =
  let form = post_form ~service
    (fun name ->
      [
        tablex [tbody [
          tr [
            th [label ~a:[a_for name] [pcdata "Username:"]];
            td [string_input ~a:[a_maxlength 50] ~input_type:`Text ~name ()];
          ]]
        ];
        div [
          string_input ~input_type:`Submit ~value:"Login" ();
        ]
      ]) ()
  in
  let content = [
    h1 [pcdata "Login"];
    form;
  ] in
  base ~title:"Login" ~content

let election_view ~election ~user =
  let service = Services.(preapply_uuid election_raw election) in
  let booth = Services.make_booth election.Common.election.e_uuid in
  lwt permissions =
    let open Common in
    match election.can_vote with
      | Any ->
        Lwt.return [ pcdata "Anyone can vote in this election." ]
      | Restricted p ->
        match user with
          | None ->
            Lwt.return [
              a ~service:Services.login [pcdata "Log in"] ();
              pcdata " to check if you can vote.";
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
  let audit_info = div [
    h2 [pcdata "Audit Info"];
    div [
      div [
        pcdata "Election URL: ";
        code [
          a ~service [ pcdata (make_string_uri ~absolute:true ~service ()) ] ()
        ];
      ];
      div [
        pcdata "Election Fingerprint: ";
        code [ pcdata election.Common.fingerprint ];
      ];
      div [
        a ~service:Services.(preapply_uuid election_ballots election) [
          pcdata "Ballot Tracking Center";
        ] ();
      ];
      div [
        a ~service:booth [ pcdata "Voting booth" ] ();
      ];
    ]
  ] in
  let content = [
    h1 [ pcdata election.Common.election.e_name ];
    p [
      pcdata "This is an election created by ";
      format_user election.Common.author;
      pcdata " with ";
      pcdata (string_of_int (Array.length election.Common.election.e_questions));
      pcdata " question(s) and ";
      pcdata (string_of_int (Array.length election.Common.public_keys));
      pcdata " trustee(s).";
    ];
    p [pcdata election.Common.election.e_description];
    p permissions;
    (match election.Common.election_result with
      | Some r ->
        let result = format_election_result election.Common.election r in
        let formatted_result =
          List.map (fun question ->
            div [
              h3 [
                pcdata question.question;
              ];
              let table xs = match xs with
                | x :: xs -> table x xs
                | [] -> div [pcdata "Result is not available."]
              in table (
                List.map (fun answer ->
                  let style = if answer.winner then b else span in
                  tr [
                    td [style [pcdata answer.answer]];
                    td [style [pcdata (string_of_int answer.count)]];
                  ]
                ) question.answers
              );
            ]
          ) result
        in div [
          pcdata "This election is complete.";
          h2 [pcdata "Tally"];
          div formatted_result;
        ]
    | None ->
      div [
        div [
          a ~service:(Services.(preapply_uuid election_vote election)) [
            pcdata "Vote in this election";
          ] ();
        ];
        div [
          pcdata "This election ends at the administrator's discretion.";
        ];
      ]
    );
    audit_info;
  ] in
  base ~title:election.Common.election.e_name ~content

let cast_ballot ~election ~result =
  let name = election.Common.election.e_name in
  let content = [
    h1 [ pcdata name ];
    div [
      pcdata "Your ballot for ";
      em [pcdata name];
      (match result with
         | `Valid hash -> pcdata (" is valid, its hash is " ^ hash ^ ".")
         | `Invalid -> pcdata " is invalid!"
         | `Malformed e -> Printf.ksprintf pcdata " is malformed! (%s)" (Printexc.to_string e)
      );
    ]
  ] in
  base ~title:name ~content
