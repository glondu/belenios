open Eliom_content.Html5.D

let base ~title ~header ~content =
  let s x = Xml.uri_of_string ("/static/" ^ x) in
  html
    (head (Eliom_content.Html5.F.title (pcdata title)) [
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
          a ~service:Helios_services.home_s
            [img
                ~src:(s "logo.gif")
                ~a:[a_style "border:0;"; a_height 110]
                ~alt:"Helios" ()] ();
          br ();
        ] @ header);
        div ~a:[a_id "contentbody"] content;
        div ~a:[a_id "footer"] [
          span ~a:[a_style "float:right;"] [ (* footer logo *) ];
          (* if user/voter... *)
          pcdata "not logged in.";
          br ();
          a
            ~service:Helios_services.heliosvotingorg_s
            [pcdata "About Helios"] ();
          (* footer links *)
          br ~a:[a_style "clear:right;"] ();
        ];
      ];
     ])
