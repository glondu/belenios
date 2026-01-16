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
open Belenios_storage_api
open Web_common
open Eliom_content.Html.F
open Eliom_content.Html.F.Form

module Make
    (Web_state : Web_state_sig.S)
    (Web_i18n : Web_i18n_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S)
    (Mails_admin : Belenios_ui.Mails_admin_sig.S) =
struct
  open Web_services
  open Pages_common

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "admin"

  let privacy_notice cont =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Election server" ^^^ s_ "Terms of service" in
    let content =
      [
        div
          [
            txt (s_ "To use this site, you must accept our ");
            direct_a !Web_config.tos (s_ "terms of service");
            txt ".";
          ];
        post_form ~service:privacy_notice_accept
          (fun ncont ->
            [
              div
                [
                  input ~input_type:`Hidden ~name:ncont ~value:cont
                    (user string_of_privacy_cont);
                  input
                    ~a:[ a_id "accept_tos" ]
                    ~input_type:`Submit ~value:(s_ "Accept") string;
                ];
            ])
          ();
      ]
    in
    base ~title ~content ()

  let login_box ?cont () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* user = Eliom_reference.get Web_state.site_user in
    let auth_systems =
      List.map (fun x -> x.auth_instance) !Web_config.site_auth_config
    in
    let cont = match cont with None -> ContSiteHome | Some x -> x in
    let cont = default_admin cont in
    let login service =
      Eliom_service.preapply ~service:site_login (Some service, cont)
    in
    let logout () = Eliom_service.preapply ~service:logout cont in
    let home =
      div
        ~a:[ a_class [ "nav-menu__item"; "noselect" ] ]
        [
          a
            ~a:[ a_class [ "nav-menu__link" ] ]
            ~service:home
            [ txt @@ s_ "Home" ]
            ();
        ]
    in
    let blank = div ~a:[ a_class [ "nav-menu__item-blank"; "noselect" ] ] [] in
    let body =
      match user with
      | Some (_, account, _) ->
          [
            div
              ~a:[ a_class [ "nav-menu__item"; "noselect" ] ]
              [
                div ~a:[ a_id "nav_username" ] [ txt account.name ];
                img
                  ~a:[ a_id "avatar" ]
                  ~src:(static [ "avatar.png" ]) ~alt:"Avatar" ();
              ];
            div
              ~a:[ a_id "logout"; a_class [ "nav-menu__item"; "noselect" ] ]
              [ a ~service:(logout ()) [ txt @@ s_ "Log out" ] () ];
          ]
      | None ->
          let auth_systems =
            List.map
              (fun name ->
                a
                  ~a:[ a_id ("login_" ^ name); a_class [ "nav-menu__link" ] ]
                  ~service:(login name)
                  [ txt name ]
                  ())
              auth_systems
            |> List.join (txt ", ")
          in
          [
            div
              ~a:[ a_class [ "nav-menu__item"; "noselect" ] ]
              ([ txt (s_ "Log in:"); txt " [" ] @ auth_systems @ [ txt "]" ]);
          ]
    in
    div ~a:[ a_class [ "nav-menu" ] ] (home :: blank :: body) |> Lwt.return

  let admin_login get_handler =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let contact =
      match !Web_config.contact_uri with
      | None -> txt ""
      | Some uri ->
          div
            [
              txt (s_ "If you do not have any account, you may ");
              direct_a ~target:"_blank" uri (s_ "contact us");
              txt ".";
            ]
    in
    let* auth_div =
      match !Web_config.site_auth_config with
      | [] -> return @@ txt ""
      | { auth_instance = service; _ } :: others ->
          let* default = get_handler service in
          let default =
            match default with
            | Web_auth_sig.Html x ->
                div ~a:[ a_class [ "embedded-login-form" ] ] [ x ]
            | Web_auth_sig.Redirection _ ->
                div
                  [
                    txt (s_ "Log in with");
                    txt " ";
                    a ~service:site_login
                      [ txt service ]
                      (Some service, default_admin ContSiteHome);
                    txt ".";
                  ]
          in
          let others =
            List.map
              (fun { auth_instance = service; _ } ->
                div
                  [
                    txt (s_ "You can also log in with");
                    txt " ";
                    a ~service:site_login
                      [ txt service ]
                      (Some service, default_admin ContSiteHome);
                    txt ".";
                  ])
              others
          in
          return @@ div (default :: others)
    in
    let* body =
      let default =
        div
          [ txt (s_ "To administer an election, you need to log in."); contact ]
      in
      read_snippet ~default ~lang !Web_config.admin_home
    in
    let content = [ body; auth_div ] in
    let title = !Web_config.vendor ^^^ s_ "Election server" in
    let* login_box = login_box ~cont:ContSiteHome () in
    base ~title ~login_box ~content ()

  let script_with_lang ~lang file =
    let file = static file in
    let dir = Filename.dirname (string_of_uri file) in
    div
      [
        Printf.ksprintf Unsafe.data
          "<script>var belenios_lang = %S; var belenios_dir = %S;</script>"
          (Language.unwrap lang) (dir ^ "/");
        script ~a:[ a_src file ] (txt "");
      ]

  let signup_captcha ~service error challenge email =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:signup_captcha_post
        (fun (lchallenge, (lresponse, lemail)) ->
          [
            div
              [
                txt (s_ "E-mail address:");
                txt " ";
                input ~input_type:`Text ~name:lemail ~value:email string;
              ];
            div
              [
                input ~input_type:`Hidden ~name:lchallenge ~value:challenge
                  string;
                txt (s_ "Please enter ");
                Pages_common.signup_captcha_img challenge;
                txt (s_ " in the following box: ");
                input ~input_type:`Text ~name:lresponse string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        service
    in
    let error = format_captcha_error l error in
    let content = [ error; form ] in
    base ~title:(s_ "Create an account") ~content ()

  let signup_changepw ~service error challenge email username =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:changepw_captcha_post
        (fun (lchallenge, (lresponse, (lemail, lusername))) ->
          [
            div
              [
                txt (s_ "E-mail address:");
                txt " ";
                input ~input_type:`Text ~name:lemail ~value:email string;
                txt (s_ " or username: ");
                input ~input_type:`Text ~name:lusername ~value:username string;
                txt ".";
              ];
            div
              [
                input ~input_type:`Hidden ~name:lchallenge ~value:challenge
                  string;
                txt (s_ "Please enter ");
                Pages_common.signup_captcha_img challenge;
                txt (s_ " in the following box: ");
                input ~input_type:`Text ~name:lresponse string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        service
    in
    let error = format_captcha_error l error in
    let content = [ error; form ] in
    base ~title:(s_ "Change password") ~content ()

  let signup_login address =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let content =
      match address with
      | Ok address ->
          [
            post_form ~service:signup_login_post
              (fun lcode ->
                [
                  div
                    [
                      txt
                      @@ Printf.sprintf
                           (f_ "A verification code has been sent to %s.")
                           address;
                      txt " ";
                      txt
                        (s_
                           "Please enter the verification code received by \
                            e-mail:");
                      txt " ";
                      input ~input_type:`Text ~name:lcode string;
                    ];
                  div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
                ])
              ();
          ]
      | Error () -> [ div [ txt @@ s_ "You cannot sign up right now." ] ]
    in
    base ~title:(s_ "Account management") ~content ()

  let signup address error username =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let error =
      match error with
      | None -> txt ""
      | Some e ->
          let msg =
            match e with
            | UsernameTaken -> s_ "the username is already taken"
            | AddressTaken ->
                s_ "there is already an account with this e-mail address"
            | BadUsername -> s_ "the username is invalid"
            | BadPassword e ->
                Printf.sprintf (f_ "the password is too weak (%s)") e
            | PasswordMismatch -> s_ "the two passwords are not the same"
            | BadSpaceInPassword ->
                s_ "the password starts or ends with a space"
            | DatabaseError -> s_ "there is an error in the database"
          in
          div
            [
              txt (s_ "The account creation ");
              span ~a:[ a_class [ "status--failure" ] ] [ txt (s_ "failed") ];
              txt (s_ " because ");
              txt msg;
              txt (s_ ". Please try again with a different one.");
            ]
    in
    let form =
      post_form ~service:signup_post
        (fun (lusername, (lpassword, lpassword2)) ->
          [
            div [ txt (s_ "Your e-mail address is: "); txt address; txt "." ];
            div
              [
                txt (s_ "Please choose a username: ");
                input ~input_type:`Text ~name:lusername ~value:username string;
                txt (s_ " and a password: ");
                input ~input_type:`Password ~name:lpassword string;
                txt ".";
              ];
            div
              [
                txt (s_ "Type the password again: ");
                input ~input_type:`Password ~name:lpassword2 string;
                txt ".";
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        ()
    in
    let content = [ error; form ] in
    base ~title:(s_ "Create an account") ~content ()

  let changepw ~username ~address error =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let error =
      match error with
      | None -> txt ""
      | Some e ->
          let reason =
            match e with
            | PasswordMismatch -> s_ "the two passwords are not the same"
            | BadPassword e ->
                Printf.sprintf (f_ "the new password is too weak (%s)") e
            | BadSpaceInPassword ->
                s_ "the new password starts or ends with a space"
            | _ -> s_ "of an unknown reason"
          in
          div
            [
              txt (s_ "The change ");
              span ~a:[ a_class [ "status--failure" ] ] [ txt (s_ "failed") ];
              txt (s_ " because ");
              txt reason;
              txt (s_ ". Please try again with a different one.");
            ]
    in
    let form =
      post_form ~service:changepw_post
        (fun (lpassword, lpassword2) ->
          [
            div
              [
                txt (s_ "Your username is: ");
                txt username;
                txt (s_ " and your e-mail address is: ");
                txt address;
                txt ".";
              ];
            div
              [
                txt (s_ "Please choose a password: ");
                input ~input_type:`Password ~name:lpassword string;
                txt ".";
              ];
            div
              [
                txt (s_ "Type the password again: ");
                input ~input_type:`Password ~name:lpassword2 string;
                txt ".";
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        ()
    in
    let content = [ error; form ] in
    base ~title:(s_ "Change password") ~content ()

  let compute_fingerprint () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let interactivity =
      div
        ~a:[ a_id "interactivity" ]
        [ script_with_lang ~lang [ "tool_js_fingerprint.js" ] ]
    in
    let content = [ interactivity ] in
    base ~title:(s_ "Compute fingerprint") ~content ()

  let set_email () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:set_email_post
        (fun name ->
          [
            div
              [
                txt (s_ "There is no e-mail address attached to your account.");
                txt " ";
                txt (s_ "Please provide one:");
                txt " ";
                input ~input_type:`Text ~name string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Proceed") string ];
          ])
        ()
    in
    let content = [ form ] in
    let title = s_ "Your e-mail address" in
    base ~title ~content ()

  let set_email_confirm address =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let content =
      match address with
      | Ok address ->
          [
            post_form ~service:set_email_confirm
              (fun name ->
                [
                  div
                    [
                      txt
                        (Printf.sprintf
                           (f_ "An e-mail with a code has been sent to %s.")
                           address);
                      txt " ";
                      txt (s_ "Please enter the code here:");
                      txt " ";
                      input ~input_type:`Text ~name string;
                    ];
                  div [ input ~input_type:`Submit ~value:(s_ "Proceed") string ];
                ])
              ();
          ]
      | Error () ->
          [
            div [ txt @@ s_ "You cannot change your e-mail address right now." ];
          ]
    in
    let title = s_ "Your e-mail address" in
    base ~title ~content ()

  let sudo () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:sudo_post
        (fun (ndomain, nuser) ->
          [
            div
              [
                txt "Domain: ";
                input ~input_type:`Text ~name:ndomain string;
                txt ", user: ";
                input ~input_type:`Text ~name:nuser string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Proceed") string ];
          ])
        ()
    in
    let content = [ form ] in
    let title = s_ "Impersonate a user" in
    base ~title ~content ()

  let connect_consent ?uuid ~account ~callback ~server ~state () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:connect_consent
        (fun () -> [ input ~input_type:`Submit ~value:(s_ "Proceed") string ])
        (callback, state)
    in
    let for_election =
      match uuid with
      | None -> txt @@ s_ "as an administrator."
      | Some uuid ->
          txt
          @@ Printf.sprintf
               (f_ "as a voter for election %s.")
               (Uuid.unwrap uuid)
    in
    let msg =
      [
        txt @@ s_ "By proceeding, you authorize:";
        ul [ li [ txt callback; txt " ("; txt server; txt ")" ] ];
        txt @@ s_ "to access:";
        ul
          [
            li
              [
                txt @@ s_ "your account id:";
                txt " ";
                txt @@ string_of_int account.id;
              ];
            li [ txt @@ s_ "your name:"; txt " "; txt @@ account.name ];
            li
              [
                txt @@ s_ "your e-mail address:";
                txt " ";
                txt @@ Option.value ~default:"(none)" account.email;
              ];
          ];
        for_election;
      ]
    in
    let content = [ div msg; form ] in
    let title = "Belenios Connect" in
    base ~title ~content ()
end
