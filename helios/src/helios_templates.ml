open Helios_datatypes_t
open Eliom_content.Html5.F

let site_title = "Helios Election Server"
let welcome_message = "This is the default message"

let s x = Xml.uri_of_string ("/static/" ^ x)

let format_user u size = Helios_services.([
  img
    ~src:(Printf.ksprintf s "auth/login-icons/%s.png" u.user_type)
    ~a:[a_style "border:0;"; a_height size]
    ~alt:u.user_type ();
  pcdata " ";
  pcdata u.user_name;
])

let base ~title ~header ~content =
  lwt user = Eliom_reference.get Helios_services.user in
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
          a ~service:Helios_services.home [
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
            | Some (admin_p, user) ->
              [pcdata "logged in as "] @ (format_user user 15) @ [
                pcdata " [";
                a ~service:Helios_services.logout [pcdata "logout"] ();
                pcdata "]";
                br ()
              ]
            | None ->
              [pcdata "not logged in."] @ [
                pcdata " [";
                a ~service:Helios_services.login [pcdata "log in"] ();
                pcdata "]";
                br ();
              ]
          ) @ [
            a ~service:Helios_services.project_home [
              pcdata "About Helios | Help!"
            ] ();
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
        ~service:Helios_services.login
        ~a:[a_style "font-size: 1.4em;"] [
          img
            ~a:[a_style "border:0;"; a_height 35]
            ~src:(Printf.ksprintf s "auth/login-icons/%s.png" x)
            ~alt:x ();
          pcdata x;
        ] ();
    ]
  ) auth_systems

type election = {
  election_short_name : string;
  election_name : string;
  election_description : string;
  election_admin : Helios_services.user;
}

let format_one_election e =
  li [pcdata e.election_name]

let format_one_featured_election e =
  [
    div ~a:[a_class ["highlight-box-margin"]] ([
      a
        ~service:(Eliom_service.preapply
                    Helios_services.election_shortcut
                    e.election_short_name)
        ~a:[a_style "font-size: 1.4em;"]
        [pcdata e.election_name] ();
      pcdata " by ";
    ] @ format_user e.election_admin 15 @ [
      br ();
      pcdata e.election_description;
    ]);
    br ();
  ]

let index ~featured =
  lwt user = Eliom_reference.get Helios_services.user in
  base
  ~title:site_title
  ~header:[h2 [pcdata site_title]]
  ~content:(
    let mystuff = match user with
      | Some (admin_p, u) ->
        let administered = if admin_p then Some [] else None in
        let voted = [] in
        let administration_box = match administered with
          | Some admin ->
            let administered_box = match admin with
              | _::_ -> ul (List.map format_one_election admin)
              | [] -> em [pcdata "none yet"]
            in [
              h4 [pcdata "Administration"];
              administered_box;
              p [pcdata "[";
                 a ~service:Helios_services.elections_administered [
                   pcdata "see all"
                 ] ();
                 pcdata "]"];
              div ~a:[a_style "text-align:right;"] [
                a ~service:Helios_services.election_new
                  ~a:[a_style "font-size: 1.2em; padding:5px; background: #eee; border: 1px solid #888;"]
                  [
                    pcdata "create election >";
                  ] ();
              ]
            ]
          | None -> []
        in
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
        @ administration_box @ recent_votes
      | None ->
        [h3 [pcdata "Log In to Start Voting"]]
        @ (login_box Helios_services.auth_systems)
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
    (fun (username_name, admin_name) ->
      [
        tablex [tbody [
          tr [
            th [label ~a:[a_for username_name] [pcdata "Username:"]];
            td [string_input ~a:[a_maxlength 50] ~input_type:`Text ~name:username_name ()];
          ];
          tr [
            th [label ~a:[a_for admin_name] [pcdata "Admin?"]];
            td [bool_checkbox ~name:admin_name ()];
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
