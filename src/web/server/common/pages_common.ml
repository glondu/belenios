(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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
open Belenios
open Belenios_server_core
open Web_common
open Eliom_content.Html.F
open Eliom_content.Html.F.Form

module Make (Web_i18n : Web_i18n_sig.S) (Web_services : Web_services_sig.S) =
struct
  open Web_services

  let direct_a ?target uri text =
    let attributes =
      match target with Some x -> [ a_target x ] | None -> []
    in
    Eliom_content.Html.F.Raw.a
      ~a:(a_href (Xml.uri_of_string uri) :: attributes)
      [ txt text ]

  let raw_a ~service ?(a = []) contents x =
    let href = Xml.uri_of_string (Eliom_uri.make_string_uri ~service x) in
    Eliom_content.Html.F.Raw.a ~a:(a_href href :: a) contents

  let api_a endpoint uuid contents =
    let path = (endpoint uuid).Belenios_web_api.Endpoints.path in
    let href = Printf.sprintf "%s/api/%s" !Web_config.prefix path in
    Eliom_content.Html.F.Raw.a ~a:[ a_href @@ Xml.uri_of_string href ] contents

  let static x =
    let service =
      Eliom_service.static_dir_with_params
        ~get_params:(Eliom_parameter.string "version")
        ()
    in
    make_absolute_string_uri ~service ("static" :: x, Version.build)
    |> Eliom_content.Xml.uri_of_string

  let get_preferred_gettext ?lang () =
    Web_i18n.get_preferred_gettext ?lang "voter"

  let read_snippet ?(default = txt "") ~lang file =
    match file with
    | None -> return default
    | Some f -> (
        let* file = Filesystem.read_file_i18n ~lang f in
        match file with
        | None -> return default
        | Some x -> return @@ Unsafe.data x)

  module UiBase = struct
    module Xml = Eliom_content.Xml
    module Svg = Eliom_content.Svg.F.Raw'
    module Html = Eliom_content.Html.F.Raw'

    let uris = Api_generic.get_configuration_uris ()
  end

  module Ui = Belenios_ui.Pages_common.Make (UiBase)

  let markup = Ui.markup
  let confirmation_fragment = Ui.confirmation_fragment

  let base ~title ?full_title ?(login_box = txt "") ?lang_box ~content
      ?(footer = txt "") ?sticky_footer ?uuid ?static:(static_page = false)
      ?redirect () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let administer =
      match uuid with
      | None -> raw_a ~service:home [ txt (s_ "Administer elections") ] ()
      | Some uuid ->
          let uri = Xml.uri_of_string @@ make_admin_link (Some uuid) in
          let id = Printf.sprintf "election_admin_%s" (Uuid.unwrap uuid) in
          Eliom_content.Html.F.Raw.a
            ~a:[ a_id id; a_href uri ]
            [ txt (s_ "Administer this election") ]
    in
    let lang_box =
      match lang_box with
      | None -> txt ""
      | Some x -> div [ x; div ~a:[ a_class [ "separator--clear" ] ] [] ]
    in
    let maybe_static x =
      if static_page then Lwt.return @@ txt "" else read_snippet ~lang x
    in
    let* warning = maybe_static !Web_config.warning_file in
    let* extra_footer = maybe_static !Web_config.footer_file in
    let full_title =
      match full_title with None -> markup title | Some x -> markup x
    in
    let restricted_mode = !Web_config.restricted_mode in
    let head_content =
      match redirect with
      | None -> []
      | Some uri ->
          [
            meta
              ~a:
                [
                  a_http_equiv "refresh";
                  a_content (Printf.sprintf "0;URL='%s'" uri);
                ]
              ();
          ]
    in
    let head_content =
      meta
        ~a:
          [ a_name "viewport"; a_content "width=device-width, initial-scale=1" ]
        ()
      :: script (txt "window.onbeforeunload = function () {};")
      :: link ~rel:[ `Stylesheet ]
           ~href:(static [ "css"; "site.bundle.css" ])
           ()
      :: head_content
    in
    Lwt.return
      (html
         ~a:[ a_dir `Ltr; a_xml_lang (Language.unwrap lang) ]
         (head (Eliom_content.Html.F.title (txt title)) head_content)
         (body
            (Ui.base_body l ~full_title ~login_box ~warning ~lang_box ~content
               ~footer ~administer ~extra_footer ?sticky_footer ~restricted_mode
               ())))

  let lang_box cont =
    let cont = default_admin cont in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let langs =
      List.map
        (fun (l, x) -> Option ([], Language.unwrap l, Some (txt x), l = lang))
        Language.available
    in
    let form =
      get_form
        ~a:[ a_id "lang_form" ]
        ~service:set_language
        (fun (nlang, ncont) ->
          [
            input ~input_type:`Hidden ~name:ncont ~value:cont
              (user string_of_site_cont);
            txt (s_ "Language:");
            txt " ";
            select
              ~a:[ a_id "lang_select" ]
              ~name:nlang string (List.hd langs) (List.tl langs);
            input
              ~a:[ a_id "lang_submit" ]
              ~input_type:`Submit ~value:(s_ "Set") string;
          ])
    in
    return
    @@ div
         ~a:[ a_class [ "lang_box" ] ]
         [
           form;
           div
             ~a:[ a_class [ "contribute-to-translations" ] ]
             [
               txt "(";
               direct_a Belenios_ui.Links.translation
                 (s_ "Wish to help with translations?");
               txt ")";
             ];
         ]

  let make_a_with_hash ~service ?hash contents =
    let uri = Eliom_uri.make_string_uri ~service () in
    let uri = match hash with None -> uri | Some x -> uri ^ "#" ^ x in
    let href = [ a_href (Xml.uri_of_string uri) ] in
    Eliom_content.Html.F.Raw.a ~a:href [ txt contents ]

  let a_mailto ?(dest = "") ~subject ~body contents =
    let uri =
      Printf.sprintf "mailto:%s?subject=%s&body=%s" dest
        (Netencoding.Url.encode ~plus:false subject)
        (Netencoding.Url.encode ~plus:false body)
    in
    (* target="_blank" does not work in Firefox,
       see https://bugzilla.mozilla.org/show_bug.cgi?id=646552 *)
    direct_a ~target:"_blank" uri contents

  let generic_page ~title ?uri ?service message () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let proceed =
      match (uri, service) with
      | None, Some service ->
          div
            [
              a ~service
                ~a:[ a_id "generic_proceed_link" ]
                [ txt (s_ "Proceed") ]
                ();
            ]
      | Some uri, None ->
          div
            [
              Eliom_content.Html.F.Raw.a
                ~a:[ a_id "generic_proceed_link"; a_href uri ]
                [ txt (s_ "Proceed") ];
            ]
      | _ -> txt ""
    in
    let content = [ p [ txt message ]; proceed ] in
    base ~title ~content ()

  let raw_textarea ?rows ?cols id contents =
    let id = [ a_id id ] in
    let rows = match rows with None -> [] | Some i -> [ a_rows i ] in
    let cols = match cols with None -> [] | Some i -> [ a_cols i ] in
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
      auth_systems
      |> List.map (fun name ->
          a
            ~a:[ a_id (Printf.sprintf "login_%s" name) ]
            ~service:(service name)
            [ txt name ]
            ())
      |> List.join (txt ", ")
    in
    let content =
      [
        div
          [
            p
              ([ txt (s_ "Please log in:"); txt " [" ]
              @ auth_systems
              @ [ txt "]" ]);
          ];
      ]
    in
    base ~title:(s_ "Log in") ~content ()

  let login_generic site_or_election username_or_address ~service ~state =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let field_name =
      match username_or_address with
      | `Username -> s_ "Username:"
      | `Address -> s_ "E-mail address:"
    in
    let form =
      post_form ~service
        (fun (nstate, name) ->
          [
            input ~input_type:`Hidden ~name:nstate ~value:state string;
            tablex
              [
                tbody
                  [
                    tr
                      [
                        th
                          [
                            label
                              ~a:[ a_label_for "username" ]
                              [ txt field_name ];
                          ];
                        td
                          [
                            input
                              ~a:[ a_id "username" ]
                              ~input_type:`Text ~name string;
                          ];
                      ];
                  ];
              ];
            div
              [
                (let value =
                   match site_or_election with
                   | `Site -> s_ "Log in"
                   | `Election -> s_ "Authenticate"
                 in
                 input ~input_type:`Submit ~value string);
              ];
          ])
        ()
    in
    return @@ div [ form ]

  let login_dummy = login_generic ~service:dummy_post
  let login_email = login_generic ~service:email_post

  let login_password site_or_election username_or_address ~service ~allowsignups
      ~state =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let signup =
      if allowsignups then
        div
          ~a:[ a_class [ "signup-actions" ] ]
          [
            br ();
            txt (s_ "You can also ");
            a ~service:signup_captcha [ txt (s_ "create an account") ] service;
            txt (s_ ", or ");
            a ~service:changepw_captcha
              [ txt (s_ "change your password") ]
              service;
            txt (s_ " (if you forgot it, for example).");
          ]
      else txt ""
    in
    let username_label =
      match username_or_address with
      | `Username -> s_ "Username:"
      | `Address -> s_ "E-mail address:"
    in
    let form =
      post_form ~service:password_post
        (fun (lstate, (llogin, lpassword)) ->
          [
            input ~input_type:`Hidden ~name:lstate ~value:state string;
            tablex
              ~a:[ a_class [ "authentication-table" ] ]
              [
                tbody
                  [
                    tr
                      [
                        th
                          [
                            label
                              ~a:[ a_label_for "username" ]
                              [ txt username_label ];
                          ];
                        td
                          [
                            input
                              ~a:
                                [
                                  a_id "username"; a_class [ "nice-text-input" ];
                                ]
                              ~input_type:`Text ~name:llogin string;
                          ];
                      ];
                    tr
                      [
                        th
                          [
                            label
                              ~a:[ a_label_for "password" ]
                              [ txt (s_ "Password:") ];
                          ];
                        td
                          [
                            input
                              ~a:
                                [
                                  a_id "password";
                                  a_class [ "nice-password-input" ];
                                ]
                              ~input_type:`Password ~name:lpassword string;
                          ];
                      ];
                  ];
              ];
            div
              ~a:[ a_class [ "container--center" ] ]
              [
                (let value =
                   match site_or_election with
                   | `Site -> s_ "Log in"
                   | `Election -> s_ "Authenticate"
                 in
                 input
                   ~a:[ a_class [ "nice-button nice-button--blue" ] ]
                   ~input_type:`Submit ~value string);
              ];
          ])
        ()
    in
    return @@ div [ form; signup ]

  let login_failed ~service () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Authentication failed" in
    let content =
      [
        div
          [
            txt
              (s_
                 "Authentication failed, probably because of a bad username or \
                  password, or you are not allowed to perform this operation.");
          ];
        div
          [
            txt (s_ "You can ");
            a ~service [ txt (s_ "try to log in again") ] ();
            txt ".";
          ];
      ]
    in
    base ~title ~content ()

  let email_login ?lang ?address ?code ~state site_or_election =
    let* l = get_preferred_gettext ?lang () in
    let open (val l) in
    let form address =
      post_form ~service:email_login_post
        (fun (lstate, lcode) ->
          [
            input ~input_type:`Hidden ~name:lstate ~value:state string;
            address;
            div
              [
                txt
                @@ s_
                     "This code is composed of 6 digits. It can take up to 15 \
                      minutes to be delivered, please be patient.";
              ];
            div
              [
                txt
                  (s_ "Please enter the verification code received by e-mail:");
                txt " ";
                input ~input_type:`Text ~name:lcode string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        ()
    in
    let form_code code =
      post_form ~service:email_login_post
        (fun (lstate, lcode) ->
          let msg =
            match site_or_election with
            | `Site -> s_ "You are authenticating as an administrator."
            | `Election -> s_ "You are authenticating as a voter."
          in
          [
            input ~input_type:`Hidden ~name:lstate ~value:state string;
            input ~input_type:`Hidden ~name:lcode ~value:code string;
            div [ txt msg ];
            div [ input ~input_type:`Submit ~value:(s_ "Confirm") string ];
          ])
        ()
    in
    let content =
      match (code, address) with
      | Some code, _ -> [ form_code code ]
      | _, None -> [ form (txt "") ]
      | _, Some (Ok address) ->
          let address =
            div
              [
                txt
                @@ Printf.sprintf
                     (f_ "A verification code has been sent to %s.")
                     address;
              ]
          in
          [ form address ]
      | _, Some (Error ()) ->
          [ div [ txt @@ s_ "You cannot authenticate right now." ] ]
    in
    let title =
      match site_or_election with
      | `Site -> s_ "Log in"
      | `Election -> s_ "Authenticate"
    in
    base ~title ~content ()

  let signup_captcha_img challenge =
    let src = make_uri ~service:signup_captcha_img challenge in
    img ~src ~alt:"CAPTCHA" ()

  let format_captcha_error l e =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    match e with
    | None -> txt ""
    | Some x ->
        let msg =
          match x with
          | BadCaptcha -> s_ "Bad security code!"
          | BadAddress -> s_ "Bad e-mail address!"
        in
        div ~a:[ a_class [ "status--failure" ] ] [ txt msg ]

  let login_email_captcha ~state error challenge email =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:email_captcha_post
        (fun (lstate, (lchallenge, (lresponse, lemail))) ->
          [
            div
              [
                txt (s_ "E-mail address:");
                txt " ";
                input ~input_type:`Text ~name:lemail ~value:email string;
              ];
            div
              [
                input ~input_type:`Hidden ~name:lstate ~value:state string;
                input ~input_type:`Hidden ~name:lchallenge ~value:challenge
                  string;
                txt (s_ "Please enter ");
                signup_captcha_img challenge;
                txt (s_ " in the following box: ");
                input ~input_type:`Text ~name:lresponse string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        ()
    in
    let error = format_captcha_error l error in
    return @@ div [ error; form ]

  let login_email_not_now () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    return @@ div [ txt (s_ "You cannot log in now. Please try later.") ]

  let authentication_impossible () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let content =
      [
        txt @@ s_ "Authentication is impossible.";
        txt @@ " ";
        txt @@ s_ "Maybe cookies are blocked.";
      ]
    in
    let title = s_ "Authentication impossible" in
    base ~title ~content ()

  let html_redirection uri =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let content =
      [
        Eliom_content.Html.F.Raw.a
          ~a:[ a_href (Xml.uri_of_string uri) ]
          [ txt @@ s_ "Click here" ];
      ]
    in
    let title = s_ "Redirection" in
    base ~title ~content ~redirect:uri ()

  let make_unsafe_script x = Printf.ksprintf Unsafe.data "<script>%s</script>" x

  let html_js_exec x =
    html
      (head (title (txt "Belenios")) [])
      (body
         [
           script ~a:[ a_src (static [ "belenios_jslib.js" ]) ] (txt "");
           make_unsafe_script x;
         ])
end
