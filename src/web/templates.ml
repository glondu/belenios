open Util
open Serializable_t
open Eliom_content.Html5.F

(* TODO: these pages should be redesigned *)

let site_title = "Election Server"
let welcome_message = "Welcome!"

let format_user u =
  let open Web_common in
  let t = string_of_user_type u.user_type in
  Printf.ksprintf pcdata "%s:%s" t u.user_name

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
                let auth_systems = List.map (fun (name, service) ->
                  a ~service [pcdata name] ()
                ) Services.auth_systems in
                div (
                  [ pcdata "Login: " ] @
                  list_join (pcdata ", ") auth_systems @
                  [ pcdata "." ]
                );
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
      a ~service:Services.(preapply_uuid election_index e)
        [pcdata e.Web_common.election.e_name] ();
    ];
    p [pcdata e.Web_common.election.e_description];
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
  let booth = Services.make_booth election.Web_common.election.e_uuid in
  lwt permissions =
    let open Web_common in
    match election.can_vote with
      | Any ->
        Lwt.return [ pcdata "Anyone can vote in this election." ]
      | Restricted p ->
        match user with
          | None ->
            Lwt.return [
              pcdata "Log in to check if you can vote.";
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
        pcdata "Election fingerprint: ";
        code [ pcdata election.Web_common.fingerprint ];
      ];
      div [
        pcdata "Election data: ";
        a ~service [ pcdata "parameters" ] ();
        pcdata ", ";
        a ~service:Services.(preapply_uuid election_public_keys election) [
          pcdata "trustee public keys"
        ] ();
        pcdata ", ";
        a ~service:Services.(preapply_uuid election_ballots election) [
          pcdata "ballots";
        ] ();
        pcdata ".";
      ];
      div [
        a ~service:booth [ pcdata "Voting booth" ] ();
      ];
    ]
  ] in
  let content = [
    h1 [ pcdata election.Web_common.election.e_name ];
    p [pcdata election.Web_common.election.e_description];
    div [
      div [
        a ~service:(Services.(preapply_uuid election_vote election)) [
          pcdata "Vote in this election";
        ] ();
      ];
    ];
    audit_info;
  ] in
  base ~title:election.Web_common.election.e_name ~content

let cast_ballot ~election ~result =
  let name = election.Web_common.election.e_name in
  let content = [
    h1 [ pcdata name ];
    div [
      pcdata "Your ballot for ";
      em [pcdata name];
      (match result with
         | `Valid hash -> pcdata (" has been accepted, its hash is " ^ hash ^ ".")
         | `Invalid -> pcdata " is invalid!"
         | `Malformed e -> Printf.ksprintf pcdata " is malformed! (%s)" (Printexc.to_string e)
         | `Anon -> pcdata " cannot be accepted, you must log in first!"
      );
    ]
  ] in
  base ~title:name ~content
