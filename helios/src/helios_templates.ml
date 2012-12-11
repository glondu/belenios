open Eliom_content.Html5.F

let site_title = "Helios Election Server"
let welcome_message = "This is the default message"

let base ~title ~header ~content =
  let s x = Xml.uri_of_string ("/static/" ^ x) in
  html
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
        div ~a:[a_id "footer"] [
          span ~a:[a_style "float:right;"] [ (* footer logo *) ];
          (* if user/voter... *)
          pcdata "not logged in.";
          br ();
          a ~service:Helios_services.heliosvotingorg [
            pcdata "About Helios"
          ] ();
          (* footer links *)
          br ~a:[a_style "clear:right;"] ();
        ];
      ];
     ])

let index ~user ~featured = base
  ~title:site_title
  ~header:[h2 [pcdata site_title]]
  ~content:(
    let user_box = match user with
      | `User (user, administered, voted) ->
        let administration_box = match administered with
          | Some admin ->
            let administered_box = match admin with
              | _::_ -> ul admin
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
          | _::_ -> ul voted
          | [] -> em [pcdata "none yet"]
        ] in
        [div ~a:[a_style "font-size:1.4em;"; a_class ["highlight-box"]] user]
        @ administration_box @ recent_votes
      | `Login login_box ->
        [h3 [pcdata "Log In to Start Voting"]]
        @ login_box
        @ [br (); br ()]
    in
    let featured_box = match featured with
      | _::_ ->
        [
          h3 [pcdata "Currently Featured Elections"];
          p featured;
        ]
      | [] ->
        [
          h4 [pcdata "no featured elections at the moment"];
        ]
    in [
      div ~a:[a_id "mystuff"] user_box;
      p ~a:[a_style "font-size: 1.4em;"] [pcdata welcome_message];
      div featured_box;
      br ~a:[a_style "clear:right;"] ();
      br ()
    ]
  )
