open Util
open Serializable_t
open Eliom_content.Html5.F

(* TODO: these pages should be redesigned *)

let project_name = "Belenios"
let site_title = project_name ^ " Election Server"
let welcome_message = "Welcome to the " ^ project_name ^ " Election Server!"

let format_user u = pcdata u.Common.user_name

let base ~title ~header ~content =
  lwt user = Eliom_reference.get Services.user in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang "en"]
    (head (Eliom_content.Html5.F.title (pcdata title)) [])
    (body [
      div ~a:[a_id "header"] [
        a ~service:Services.home [pcdata site_title] ();
        header;
      ];
      div ~a:[a_id "content"] [content];
      div ~a:[a_id "footer"]
        (match user with
          | Some user ->
            [
              pcdata "Logged in as ";
              format_user user;
              pcdata ". ";
              a ~service:Services.logout [pcdata "Log out"] ();
              pcdata ".";
            ]
          | None ->
            [
              pcdata "Not logged in. ";
              a ~service:Services.login [pcdata "Log in"] ();
              pcdata ".";
            ]
        )
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
  let header = h1 [pcdata site_title] in
  let content =
    let featured_box = match featured with
      | _::_ ->
        div [
          h2 [pcdata "Current Featured Elections"];
          ul (List.map format_one_featured_election featured);
        ]
      | [] ->
        p [
          pcdata "No featured elections at the moment.";
        ]
    in div [
      pcdata welcome_message;
      featured_box;
    ]
  in base ~title:site_title ~header ~content

let dummy_login ~service =
  let title = "Login - " ^ site_title in
  let header = h1 [pcdata "Login"] in
  let content = post_form
    ~service
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
  in base ~title ~header ~content

let election_view ~election ~user =
  let service = Services.(preapply_uuid election_raw election) in
  let booth = Services.make_booth election.Common.election.e_uuid in
  lwt eligibility =
    if not election.Common.private_p then (
      Lwt.return [
        pcdata "Anyone can vote in this election.";
      ]
    ) else (match user with
      | None ->
        Lwt.return [
          a ~service:Services.login [pcdata "Log in"] ();
          pcdata " to check if you can vote.";
        ]
      | Some u ->
        lwt b = Services.is_eligible election.Common.election.e_uuid u in
        let can = if b then pcdata "can" else pcdata "cannot" in
        Lwt.return [
          pcdata "You ";
          can;
          pcdata " vote in this election.";
        ]
    )
  in
  let audit_info = div [
    h2 [pcdata "Audit Info"];
    div [
      p [
        pcdata "Election URL: ";
        code [
          a ~service [ pcdata (make_string_uri ~absolute:true ~service ()) ] ()
        ];
      ];
      p [
        pcdata "Election Fingerprint: ";
        code [ pcdata election.Common.fingerprint ];
      ];
      p [
        a ~service:Services.(preapply_uuid election_ballots election) [
          pcdata "Ballot Tracking Center";
        ] ();
        pcdata " | ";
        a ~service:booth [ pcdata "Voting booth" ] ();
      ];
    ]
  ] in
  let content = div [
    p [
      pcdata "This is a ";
      em [
        pcdata (if election.Common.private_p then "private" else "public")
      ];
      pcdata " election created by ";
      format_user election.Common.admin;
      pcdata " with ";
      pcdata (string_of_int (Array.length election.Common.election.e_questions));
      pcdata " question(s) and ";
      pcdata (string_of_int (Array.length election.Common.public_keys));
      pcdata " trustee(s).";
    ];
    p [pcdata election.Common.election.e_description];
    p eligibility;
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
        p [
          a ~service:(Services.(preapply_uuid election_vote election)) [
            pcdata "Vote in this election";
          ] ();
        ];
        p [
          pcdata "This election ends at the administrator's discretion.";
        ];
      ]
    );
    audit_info;
  ] in
  let title = election.Common.election.e_name ^ " - " ^ project_name in
  let header = h1 [ pcdata election.Common.election.e_name ] in
  base ~title ~header ~content

let cast_ballot ~election ~result =
  let name = election.Common.election.e_name in
  let title = name ^ " - " ^ project_name in
  let header = h1 [ pcdata name ] in
  let content =
    p [
      pcdata "Your ballot for ";
      em [pcdata name];
      (match result with
         | `Valid hash -> pcdata (" is valid, its hash is " ^ hash ^ ".")
         | `Invalid -> pcdata " is invalid!"
         | `Malformed e -> Printf.ksprintf pcdata " is malformed! (%s)" (Printexc.to_string e)
      );
    ]
  in
  base ~title ~header ~content
