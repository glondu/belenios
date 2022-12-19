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
open Belenios_core
open Common
open Web_common
open Eliom_content.Html.F
open Eliom_content.Html.F.Form

module Make (Web_i18n : Web_i18n_sig.S) (Web_services : Web_services_sig.S) = struct

  open Web_services

  let direct_a ?target uri text =
    let attributes =
      match target with
      | Some x -> [a_target x]
      | None -> []
    in
    Eliom_content.Html.F.Raw.a
      ~a:(a_href (Xml.uri_of_string uri) :: attributes)
      [txt text]

  let raw_a ~service ?(a = []) contents x =
    let href = Xml.uri_of_string (Eliom_uri.make_string_uri ~service x) in
    Eliom_content.Html.F.Raw.a ~a:(a_href href :: a) contents

  let absolute_uri_of_service ~service x =
    Eliom_uri.make_string_uri ~absolute:true ~service x
    |> rewrite_prefix
    |> Eliom_content.Xml.uri_of_string

  let static x =
    let service =
      Eliom_service.static_dir_with_params
        ~get_params:(Eliom_parameter.string "version") ()
    in
    Eliom_uri.make_string_uri ~absolute:true ~service (["static"; x], Version.build)
    |> rewrite_prefix
    |> Eliom_content.Xml.uri_of_string

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

  let read_snippet ?(default = txt "") ~lang file = match file with
    | None -> return default
    | Some f ->
       let* f =
         let f' = f ^ "." ^ lang in
         let* b = Lwt_unix.file_exists f' in
         return (if b then f' else f)
       in
       let* file = read_file f in
       match file with
       | None -> return default
       | Some x -> return @@ Unsafe.data (String.concat "\n" x)

  module UiBase = struct
    module Xml = Eliom_content.Xml
    module Svg = Eliom_content.Svg.F.Raw
    module Html = Eliom_content.Html.F.Raw

    module Uris = struct
      let home = absolute_uri_of_service ~service:home ()
      let logo = absolute_uri_of_service ~service:logo ()
      let belenios = Eliom_content.Xml.uri_of_string Belenios_ui.Links.belenios
      let source_code = absolute_uri_of_service ~service:source_code ()
      let privacy_policy = Xml.uri_of_string !Web_config.gdpr_uri
    end
  end

  module Ui = Belenios_ui.Pages_common.Make(UiBase)

  let base ~title ?full_title ?(login_box = txt "") ?lang_box ~content ?(footer = txt "") ?uuid ?static:(static_page = false) () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let administer =
      match uuid with
      | None ->
         raw_a ~service:admin [txt (s_ "Administer elections")] ()
      | Some uuid ->
         raw_a ~service:election_admin ~a:[a_id ("election_admin_" ^ (Uuid.unwrap uuid))] [txt (s_ "Administer this election")] uuid
    in
    let lang_box =
      match lang_box with
      | None -> txt ""
      | Some x -> div [x; div ~a:[a_style "clear: both;"] []]
    in
    let maybe_static x = if static_page then Lwt.return @@ txt "" else read_snippet ~lang x in
    let* warning = maybe_static !Web_config.warning_file in
    let* extra_footer = maybe_static !Web_config.footer_file in
    let full_title =
      match full_title with
      | None -> [txt title]
      | Some x -> txt_br x
    in
    Lwt.return
      (html ~a:[a_dir `Ltr; a_xml_lang lang]
         (head (Eliom_content.Html.F.title (txt title)) [
              meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1"] ();
              script (txt "window.onbeforeunload = function () {};");
              link ~rel:[`Stylesheet] ~href:(static "responsive_site.css") ();
         ])
         (body (Ui.base_body l ~full_title ~login_box ~warning ~lang_box ~content ~footer ~administer ~extra_footer ()))
      )

  let lang_box cont =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let langs = List.map (fun (l, x) -> Option ([], l, Some (txt x), l = lang)) Belenios_ui.Languages.available in
    let form =
      get_form ~a:[a_id "lang_form"] ~service:set_language
        (fun (nlang, ncont) ->
          [
            input ~input_type:`Hidden ~name:ncont ~value:cont (user string_of_site_cont);
            txt (s_ "Language:");
            txt " ";
            select ~a:[a_id "lang_select"] ~name:nlang string (List.hd langs) (List.tl langs);
            input ~a:[a_id "lang_submit"] ~input_type:`Submit ~value:(s_ "Set") string;
          ]
        )
    in
    return @@ div ~a:[a_class ["lang_box"]]
                [
                  form;
                  div ~a:[a_style "font-size: 80%; font-style: italic; text-align: right;"]
                    [
                      txt "(";
                      direct_a Belenios_ui.Links.translation (s_ "Wish to help with translations?");
                      txt ")";
                    ];
                ]

  let make_a_with_hash ~service ?hash ?style contents =
    let uri = Eliom_uri.make_string_uri ~service () in
    let uri = match hash with
      | None -> uri
      | Some x -> uri ^ "#" ^ x
    in
    let href = [a_href (Xml.uri_of_string uri)] in
    let style =
      match style with
      | None -> []
      | Some x -> [a_style x]
    in
    Eliom_content.Html.F.Raw.a ~a:(href @ style) [txt contents]

  let a_mailto ?(dest = "") ~subject ~body contents =
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

  let login_title site_or_election service =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let format =
      match site_or_election with
      | `Site -> f_ "Log in with %s"
      | `Election -> f_ "Authenticate with %s"
    in
    Printf.ksprintf return format service

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

  let login_generic site_or_election username_or_address ~service ~state =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let field_name =
      match username_or_address with
      | `Username -> s_ "Username:"
      | `Address -> s_ "E-mail address:"
    in
    let form = post_form ~service
                 (fun (nstate, name) ->
                   [
                     input ~input_type:`Hidden ~name:nstate ~value:state string;
                     tablex [tbody [
                                 tr [
                                     th [label ~a:[a_label_for "username"] [txt field_name]];
                                     td [input ~a:[a_id "username"] ~input_type:`Text ~name string];
                               ]]
                       ];
                     div [
                         let value =
                           match site_or_election with
                           | `Site -> s_ "Log in"
                           | `Election -> s_ "Authenticate"
                         in
                         input ~input_type:`Submit ~value string;
                       ]
                 ]) ()
    in
    return @@ div [form]

  let login_dummy = login_generic ~service:dummy_post
  let login_email = login_generic ~service:email_post

  let login_password site_or_election username_or_address ~service ~allowsignups ~state =
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
    let username_label =
      match username_or_address with
      | `Username -> s_ "Username:"
      | `Address -> s_ "E-mail address:"
    in
    let form = post_form ~service:password_post
                 (fun (lstate, (llogin, lpassword)) ->
                   [
                     input ~input_type:`Hidden ~name:lstate ~value:state string;
                     tablex ~a:[a_class ["authentication-table"]] [tbody [
                                                                       tr [
                                                                           th [label ~a:[a_label_for "username"] [txt username_label]];
                                                                           td [input ~a:[a_id "username"; a_class ["nice-text-input"]] ~input_type:`Text ~name:llogin string];
                                                                         ];
                                                                       tr [
                                                                           th [label ~a:[a_label_for "password"] [txt (s_ "Password:")]];
                                                                           td [input ~a:[a_id "password"; a_class ["nice-password-input"]] ~input_type:`Password ~name:lpassword string];
                                                                         ];
                       ]];
                     div ~a:[a_style "text-align: center;"] [
                         let value =
                           match site_or_election with
                           | `Site -> s_ "Log in"
                           | `Election -> s_ "Authenticate"
                         in
                         input ~a:[a_class ["nice-button nice-button--blue"]] ~input_type:`Submit ~value string;
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

  let email_login site_or_election =
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
    let title =
      match site_or_election with
      | `Site -> s_ "Log in"
      | `Election -> s_ "Authenticate"
    in
    base ~title ~content ()

  let email_email ~address ~code =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open Belenios_ui.Mail_formatter in
    let b = create () in
    add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
    add_newline b; add_newline b;
    add_sentence b (s_ "Your e-mail address has been used to authenticate with our Belenios server.");
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
    let subject = s_ "Belenios authentication" in
    Lwt.return (subject, body)

  let signup_captcha_img challenge =
    let src = make_uri ~service:signup_captcha_img challenge in
    img ~src ~alt:"CAPTCHA" ()

  let format_captcha_error l e =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
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

end
