open Util
open Serializable_compat_t
open Eliom_content.Html5.F

(* FIXME: these pages should be redesigned *)

let site_title = "Helios Election Server"
let welcome_message = "Welcome to the Helios Election Server!"

let s x = Xml.uri_of_string ("/static/" ^ x)

let format_user u size = Services.([
  img
    ~src:(Printf.ksprintf s "auth/login-icons/%s.png" u.user_type)
    ~a:[a_style "border:0;"; a_height size]
    ~alt:u.user_type ();
  pcdata " ";
  pcdata u.user_name;
])

let base ~title ~header ~content =
  lwt user = Eliom_reference.get Services.user in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang "en"]
    (head (Eliom_content.Html5.F.title (pcdata (title ^ " - Helios"))) [
      link
        ~rel:[`Stylesheet]
        ~href:(s "main.css")
        ~a:[a_mime_type "text/css"; a_media [`Screen]]
        ();
      link
        ~rel:[`Stylesheet]
        ~href:(s "helios/css/ui-lightness/jquery-ui-1.8.1.custom.css")
        ~a:[a_mime_type "text/css"]
        ();
      script (pcdata "") ~a:[a_src (s "helios/js/jquery-1.4.2.min.js")];
      script (pcdata "") ~a:[a_src (s "helios/js/jquery-ui-1.8.1.custom.min.js")];
      script (pcdata "") ~a:[a_src (s "helios/js/jqsplitdatetime.js")];
      script (pcdata "") ~a:[a_src (s "helios/helios/jquery.json.min.js")];
      (* block js *)
      (* block extra-head *)
    ])
    (body [
      div ~a:[a_id "content"] [
        div ~a:[a_id "header"] ([
          a ~service:Services.home [
            img
              ~src:(s "logo.gif")
              ~a:[a_style "border:0;"; a_height 110]
              ~alt:"Helios" ()
          ] ();
          br ();
        ] @ header);
        div ~a:[a_id "contentbody"] content;
        div ~a:[a_id "footer"] (
          [span ~a:[a_style "float:right;"] [ (* footer logo *) ]] @
          (match user with
            | Some user ->
              [pcdata "logged in as "] @ (format_user user 15) @ [
                pcdata " [";
                a ~service:Services.logout [pcdata "logout"] ();
                pcdata "]";
                br ()
              ]
            | None ->
              [pcdata "not logged in."] @ [
                pcdata " [";
                a ~service:Services.login [pcdata "log in"] ();
                pcdata "]";
                br ();
              ]
          ) @ [
            a ~service:Services.project_home [
              pcdata "About Helios"
            ] ();
            pcdata " | Help!";
          (* footer links *)
            br ~a:[a_style "clear:right;"] ();
          ]
        )
      ];
     ]))

let not_implemented title = base
  ~title
  ~header:[h2 [pcdata title]]
  ~content:[div [pcdata "This service is not implemented."]]

let login_box auth_systems = List.map
  (fun x ->
    p [
      a
        ~service:Services.login
        ~a:[a_style "font-size: 1.4em;"] [
          img
            ~a:[a_style "border:0;"; a_height 35]
            ~src:(Printf.ksprintf s "auth/login-icons/%s.png" x)
            ~alt:x ();
          pcdata x;
        ] ();
    ]
  ) auth_systems

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
  ) (r.result : int array array) |>
  Array.to_list

let format_one_election e =
  li [pcdata e.Common.election.e_name]

let format_one_featured_election e =
  [
    div ~a:[a_class ["highlight-box-margin"]] ([
      a
        ~service:Services.(preapply_uuid election_view e)
        ~a:[a_style "font-size: 1.4em;"]
        [pcdata e.Common.election.e_name] ();
      pcdata " by ";
    ] @ format_user e.Common.admin 15 @ [
      br ();
      pcdata e.Common.election.e_description;
    ]);
    br ();
  ]

let index ~featured =
  lwt user = Eliom_reference.get Services.user in
  base
  ~title:site_title
  ~header:[h2 [pcdata site_title]]
  ~content:(
    let mystuff = match user with
      | Some u ->
        let voted = [] in
        let recent_votes = [
          h4 [pcdata "Recent votes"];
          match voted with
          | _::_ -> ul (List.map format_one_election voted)
          | [] -> em [pcdata "none yet"]
        ] in
        [
          div ~a:[a_style "font-size:1.4em;"; a_class ["highlight-box"]]
            (format_user u 25)
        ]
        @ recent_votes
      | None ->
        [h3 [pcdata "Log In to Start Voting"]]
        @ (login_box Services.auth_systems)
        @ [br (); br ()]
    in
    let featured_box = match featured with
      | _::_ ->
        [
          h3 [pcdata "Current Featured Elections"];
          div (List.flatten (List.map format_one_featured_election featured));
        ]
      | [] ->
        [
          h4 [pcdata "no featured elections at the moment"];
        ]
    in ([
      div ~a:[a_id "mystuff"] mystuff;
      p ~a:[a_style "font-size: 1.4em;"] [pcdata welcome_message];
    ] @ featured_box @ [
      br ~a:[a_style "clear:right;"] ();
      br ()
    ])
  )

let dummy_login ~service =
  let title = site_title ^ " — Login" in
  let form = post_form
    ~a:[a_id "login_form"; a_class ["prettyform"]]
    ~service
    (fun username_name ->
      [
        tablex [tbody [
          tr [
            th [label ~a:[a_for username_name] [pcdata "Username:"]];
            td [string_input ~a:[a_maxlength 50] ~input_type:`Text ~name:username_name ()];
          ]]
        ];
        div [
          string_input ~input_type:`Submit ~value:"Login" ();
        ]
      ]) ()
  in
  base
    ~title
    ~header:[h2 [pcdata title]]
    ~content:[div [form]]

let election_view ~election ~user =
  let service = Services.(preapply_uuid election_raw election) in
  let booth = Services.make_booth election.Common.election.e_uuid in
  lwt eligibility =
    if not election.Common.private_p && election.Common.election.e_openreg then (
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
  let audit_info = [
    (* FIXME: unsafe_data *)
    unsafe_data "<a href=\"#\" onclick=\"$('#auditbody').slideToggle(250);\">Audit Info</a>";
    div ~a:[
      a_id "auditbody";
      a_style "display:none;";
    ] [
      br ();
      pcdata "Election URL:";
      br ();
      code ~a:[a_style "font-size: 1.2em;"] [
        a ~service [ pcdata (make_string_uri ~absolute:true ~service ()) ] ()
      ];
      br ();
      br ();
      pcdata "Election Fingerprint:";
      br ();
      code ~a:[a_style "font-size: 1.3em; font-weight: bold;"] [
        pcdata election.Common.fingerprint;
      ];
      br ();
      br ();
      p ~a:[a_style "font-size: 1.3em;"] [
        a ~service:Services.(preapply_uuid election_ballots election) [
          pcdata "Ballot Tracking Center";
        ] ();
        pcdata " | ";
        a ~service:booth [
          pcdata "Voting booth";
        ] ();
      ];
    ]
  ] in
  let content = [
    div ~a:[a_style "float: left; margin-right: 50px;"] [
      h2 ~a:[a_class ["title"]] [pcdata election.Common.election.e_name];
      p ~a:[a_style "padding-top:0px; margin-top:0px"] [
        em [
          pcdata (if election.Common.private_p then "private" else "public")
        ];
        pcdata " election created by ";
        u [ b (format_user election.Common.admin 15) ];
        pcdata " with ";
        pcdata (string_of_int (Array.length election.Common.election.e_questions));
        pcdata " question(s) and ";
        pcdata (string_of_int (Array.length election.Common.public_keys));
        pcdata " trustee(s)";
      ];
    ];
    br ();
    br ();
    br ~a:[a_style "clear: left;"] ();
    div ~a:[a_style "margin-bottom: 25px;margin-left: 15px; border-left: 1px solid #aaa; padding-left: 5px; font-size:1.3em;"] [pcdata election.Common.election.e_description];
    (* NOTE: administration things removed from here! *)
    br ();
  ] @ (match election.Common.state, election.Common.election_result with
    | `Finished, Some r ->
      let result = format_election_result election.Common.election r in
      [
        span ~a:[a_class ["highlight-box"; "round"]] [
          pcdata "This election is complete.";
        ];
        br ();
        br ();
        h3 ~a:[a_class ["highlight-box"]] [pcdata "Tally"];
      ] @ (
        List.iteri (fun i question ->
          [
            b [
              span ~a:[a_style "font-size:0.8em;"] [
                pcdata "Question #";
                pcdata (string_of_int i);
              ];
              br ();
              pcdata question.question;
            ];
            br ();
            let table xs = match xs with
              | x :: xs -> table ~a:[a_class ["pretty"]; a_style "width: auto;"] x xs
              | [] -> assert false
            in table (
              List.map (fun answer ->
                let style = if answer.winner then "font-weight:bold;" else "" in
                tr [
                  td ~a:[a_style ("padding-right:80px;" ^ style)] [pcdata answer.answer];
                  td ~a:[a_style ("text-align:right;" ^ style)] [pcdata (string_of_int answer.count)];
                ]
              ) question.answers
            );
          ]
        ) result
      )
    | `Stopped, _ ->
      [
        span ~a:[a_class ["highlight-box"; "round"]] [
          pcdata "Election closed. Tally will be computed soon.";
        ];
        br ();
      ]
    | `Started, _ ->
      [
        span ~a:[
          a_class ["highlight-box"; "round"];
          a_style "font-size: 1.6em; margin-right: 10px;";
          a_id "votelink";
        ] [
          a ~service:(Services.(preapply_uuid election_vote election)) [
            pcdata "Vote in this election";
          ] ()
        ];
        br ();
        br ();
        (* if election.voting_extended_until ... *)
        pcdata "This election ends at the administrator's discretion.";
        br ();
      ]
    | _ ->
      [
        span ~a:[a_class ["highlight-box"; "round"]] [
          pcdata "FIXME";
        ];
        br ();
      ]
  ) @ eligibility @ [
    div ~a:[
      a_style "background: lightyellow; padding:5px; padding-left: 10px; margin-top: 15px; border: 1px solid #aaa; width: 720px;";
      a_class ["round"];
    ] audit_info
  ] in
  base ~title:election.Common.election.e_name ~header:[] ~content

let cast_ballot ~election ~result =
  let title = election.Common.election.e_name in
  let content = [
    h2 ~a:[a_class ["title"]] [
      pcdata title;
    ];
    br ();
    div [
      pcdata "Your ballot for ";
      em [pcdata election.Common.election.e_name];
      (match result with
         | `Valid hash -> pcdata (" is valid, its hash is " ^ hash)
         | `Invalid -> pcdata " is invalid!"
         | `Malformed -> pcdata " is malformed!"
      );
    ]
  ] in base ~title ~header:[] ~content
