(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Lwt.Syntax
open Belenios_platform
open Belenios
open Serializable_builtin_t
open Common
open Web_serializable_j
open Web_common
open Web_services
open Eliom_content.Html.F
open Eliom_content.Html.F.Form

let direct_a ?target uri text =
  let attributes =
    match target with
    | Some x -> [a_target x]
    | None -> []
  in
  Eliom_content.Html.F.Raw.a
    ~a:(a_href (uri_of_string (fun () -> uri)) :: attributes)
    [txt text]

let static x =
  let service =
    Eliom_service.static_dir_with_params
      ~get_params:(Eliom_parameter.string "version") ()
  in
  make_uri ~service (["static"; x], Belenios_version.build)

let format_user ~site u =
  em [txt (if site then string_of_user u else u.user_name)]

let belenios_url = Eliom_service.extern
  ~prefix:"https://www.belenios.org"
  ~path:[]
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

let base ~title ?login_box ?lang_box ~content ?(footer = div []) ?uuid () =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let administer =
    match uuid with
    | None ->
       a ~service:admin [txt (s_ "Administer elections")] ()
    | Some uuid ->
       a ~service:election_admin ~a:[a_id ("election_admin_" ^ (raw_string_of_uuid uuid))] [txt (s_ "Administer this election")] uuid
  in
  let login_box = match login_box with
    | None ->
       div ~a:[a_style "float: right; padding: 10px;"] [
         img ~a:[a_height 70] ~alt:""
           ~src:(static "placeholder.png") ();
       ]
    | Some x -> x
  in
  let lang_box =
    match lang_box with
    | None -> div []
    | Some x -> div [x; div ~a:[a_style "clear: both;"] []]
  in
  let* warning = match !Web_config.warning_file with
    | None -> return @@ txt ""
    | Some f ->
       let* file = read_file f in
       match file with
       | None -> return @@ txt ""
       | Some x -> return @@ Unsafe.data (String.concat "\n" x)
  in
  Lwt.return (html ~a:[a_dir `Ltr; a_xml_lang lang]
    (head (Eliom_content.Html.F.title (txt title)) [
      script (txt "window.onbeforeunload = function () {};");
      link ~rel:[`Stylesheet] ~href:(static "site.css") ();
    ])
    (body [
      div ~a:[a_id "wrapper"] [
      div ~a:[a_id "header"] [
        div [
          div ~a:[a_style "float: left; padding: 10px;"] [
            a ~service:home [
              img ~alt:(s_ "Election server") ~a:[a_height 70]
                ~src:(static "logo.png") ();
            ] ();
          ];
          login_box;
          h1 ~a:[a_style "text-align: center; padding: 20px;"] [txt title];
          div ~a:[a_style "clear: both;"] [];
        ];
      ];
      warning;
      div ~a:[a_id "main"]
        [
          lang_box;
          div content;
        ];
      div ~a:[a_id "footer"; a_style "text-align: center;" ] [
        div ~a:[a_id "bottom"] [
          footer;
          txt (s_ "Powered by ");
          a ~service:belenios_url [txt "Belenios"] ();
          Belenios_version.(
            Printf.ksprintf txt " %s (%s). " version build
          );
          a ~service:source_code [txt (s_ "Get the source code")] ();
          txt ". ";
          direct_a !Web_config.gdpr_uri (s_ "Privacy policy");
          txt ". ";
          administer;
          txt ".";
        ]
      ]]
     ]))

let lang_box cont =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let langs = List.map (fun l -> Option ([], l, None, l = lang)) available_languages in
  let form =
    get_form ~service:set_language
      (fun (nlang, ncont) ->
        [
          input ~input_type:`Hidden ~name:ncont ~value:cont (user string_of_site_cont);
          txt (s_ "Language:");
          txt " ";
          select ~name:nlang string (List.hd langs) (List.tl langs);
          input ~input_type:`Submit ~value:(s_ "Set") string;
        ]
      )
  in
  return @@ div ~a:[a_class ["lang_box"]]
    [
      form;
      div ~a:[a_style "font-size: 80%; font-style: italic; text-align: right;"]
        [
          txt "(";
          direct_a "https://www.belenios.org/translation.html" (s_ "Wish to help with translations?");
          txt ")";
        ];
    ]

let make_button ~service ?hash ?style ~disabled contents =
  let uri = Eliom_uri.make_string_uri ~service () in
  let uri = match hash with
    | None -> uri
    | Some x -> uri ^ "#" ^ x
  in
  let style =
    match style with
    | None -> ""
    | Some x -> Printf.sprintf " style=\"%s\"" x
  in
  Printf.ksprintf Unsafe.data (* FIXME: unsafe *)
    "<button onclick=\"location.href='%s';\"%s%s>%s</button>"
    uri style (if disabled then " disabled" else "")
    contents

let make_a_with_hash ~service ?hash ?style contents =
  let uri = Eliom_uri.make_string_uri ~service () in
  let uri = match hash with
    | None -> uri
    | Some x -> uri ^ "#" ^ x
  in
  let href = [a_href (uri_of_string (fun () -> uri))] in
  let style =
    match style with
    | None -> []
    | Some x -> [a_style x]
  in
  Eliom_content.Html.F.Raw.a ~a:(href @ style) [txt contents]

let a_mailto ~dest ~subject ~body contents =
  let uri = Printf.sprintf "mailto:%s?subject=%s&body=%s" dest
    (Netencoding.Url.encode ~plus:false subject)
    (Netencoding.Url.encode ~plus:false body)
  in
  (* target="_blank" does not work in Firefox,
     see https://bugzilla.mozilla.org/show_bug.cgi?id=646552 *)
  direct_a ~target:"_blank" uri contents

let generic_page ~title ?service message () =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let proceed = match service with
    | None -> txt ""
    | Some service ->
       div [
         a ~service ~a:[a_id "generic_proceed_link"] [txt (s_ "Proceed")] ();
       ]
  in
  let content = [
    p [txt message];
    proceed;
  ] in
  base ~title ~content ()

let raw_textarea ?rows ?cols id contents =
  let id = [a_id id] in
  let rows = match rows with
    | None -> []
    | Some i -> [a_rows i]
  in
  let cols = match cols with
    | None -> []
    | Some i -> [a_cols i]
  in
  Eliom_content.Html.F.Raw.textarea ~a:(id @ rows @ cols) (txt contents)

let login_title service =
  let* l = get_preferred_gettext () in
  let open (val l) in
  return @@ Printf.sprintf (f_ "Log in with %s") service

let login_choose auth_systems service () =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let auth_systems =
    auth_systems |>
    List.map (fun name ->
      a ~service:(service name) [txt name] ()
    ) |> List.join (txt ", ")
  in
  let content = [
    div [p (
      [txt (s_ "Please log in:"); txt " ["] @ auth_systems @ [txt "]"]
    )]
  ] in
  base ~title:(s_ "Log in") ~content ()

let login_generic ~service ~state =
  let field_name, input_type = "Username:", `Text in
  let form = post_form ~service
    (fun (nstate, name) ->
      [
        input ~input_type:`Hidden ~name:nstate ~value:state string;
        tablex [tbody [
          tr [
            th [label ~a:[a_label_for (Eliom_parameter.string_of_param_name name)] [txt field_name]];
            td [input ~input_type ~name string];
          ]]
        ];
        div [
          input ~input_type:`Submit ~value:"Login" string;
        ]
      ]) ()
  in
  return @@ div [form]

let login_dummy = login_generic ~service:dummy_post
let login_email = login_generic ~service:email_post

let login_password ~service ~allowsignups ~state =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let signup =
    if allowsignups then
      div [
          br ();
          txt (s_ "You can also ");
          a ~service:signup_captcha [txt (s_ "create an account")] service;
          txt (s_ ", or ");
          a ~service:changepw_captcha [txt (s_ "change your password")] service;
          txt (s_ " (if you forgot it, for example).");
        ]
    else txt ""
  in
  let form = post_form ~service:password_post
    (fun (lstate, (llogin, lpassword)) ->
      [
        input ~input_type:`Hidden ~name:lstate ~value:state string;
        tablex [tbody [
          tr [
            th [label ~a:[a_label_for (Eliom_parameter.string_of_param_name llogin)] [txt (s_ "Username:")]];
            td [input ~input_type:`Text ~name:llogin string];
          ];
          tr [
            th [label ~a:[a_label_for (Eliom_parameter.string_of_param_name lpassword)] [txt (s_ "Password:")]];
            td [input ~input_type:`Password ~name:lpassword string];
          ];
        ]];
        div [
          input ~input_type:`Submit ~value:(s_ "Login") string;
        ]
      ]) ()
  in
  return @@ div [form; signup]

let login_failed ~service () =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let title = s_ "Authentication failed" in
  let content =
    [
      div [txt (s_ "Authentication failed, probably because of a bad username or password, or you are not allowed to perform this operation.")];
      div [
          txt (s_ "You can ");
          a ~service [txt (s_ "try to log in again")] ();
          txt ".";
        ];
    ]
  in
  base ~title ~content ()

let email_login () =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let form =
    post_form ~service:email_login_post
      (fun lcode ->
        [
          div [
              txt (s_ "Please enter the verification code received by e-mail:");
              txt " ";
              input ~input_type:`Text ~name:lcode string;
            ];
          div [
              input ~input_type:`Submit ~value:(s_ "Submit") string;
            ];
        ]
      ) ()
  in
  let content = [form] in
  base ~title:(s_ "Log in") ~content ()

let email_email ~address ~code =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let open Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b; add_newline b;
  add_sentence b (s_ "Your e-mail address has been used to log in to our Belenios server.");
  add_sentence b (s_ "Use the following code:");
  add_newline b; add_newline b;
  add_string b "  "; add_string b code;
  add_newline b; add_newline b;
  add_sentence b (s_ "Warning: this code is valid for 15 minutes, and previous codes sent to this address are no longer valid.");
  add_newline b; add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b; add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b (s_ "Belenios Server");
  let body = contents b in
  let subject = s_ "Belenios login" in
  Lwt.return (subject, body)

let signup_captcha_img challenge =
  let src = make_uri ~service:signup_captcha_img challenge in
  img ~src ~alt:"CAPTCHA" ()

let format_captcha_error l e =
  let open (val l : Web_i18n_sig.GETTEXT) in
  match e with
  | None -> txt ""
  | Some x ->
     let msg = match x with
       | BadCaptcha -> s_ "Bad security code!"
       | BadAddress -> s_ "Bad e-mail address!"
     in
     div ~a:[a_style "color: red;"] [txt msg]

let login_email_captcha ~state error challenge email =
  let* l = get_preferred_gettext () in
  let open (val l) in
  let form =
    post_form ~service:email_captcha_post
      (fun (lstate, (lchallenge, (lresponse, lemail))) ->
        [
          div [
              txt (s_ "E-mail address:");
              txt " ";
              input ~input_type:`Text ~name:lemail ~value:email string;
            ];
          div [
              input ~input_type:`Hidden ~name:lstate ~value:state string;
              input ~input_type:`Hidden ~name:lchallenge ~value:challenge string;
              txt (s_ "Please enter ");
              signup_captcha_img challenge;
              txt (s_ " in the following box: ");
              input ~input_type:`Text ~name:lresponse string;
            ];
          div [
              input ~input_type:`Submit ~value:(s_ "Submit") string;
            ];
        ]
      ) ()
  in
  let error = format_captcha_error l error in
  return @@ div [error; form]

let login_email_not_now () =
  let* l = get_preferred_gettext () in
  let open (val l) in
  return @@ div [txt (s_ "You cannot log in now. Please try later.")]
