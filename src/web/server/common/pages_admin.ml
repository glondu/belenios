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
open Belenios_api.Serializable_t
open Belenios_server_core
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

  let checkpriv_link l uuid =
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    let uri =
      make_trustee_link uuid `Check |> Eliom_content.Xml.uri_of_string
    in
    Eliom_content.Html.F.Raw.a
      ~a:[ a_href uri ]
      [ txt @@ s_ "Check private key ownership" ]

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
        [ a ~service:home [ txt @@ s_ "Home" ] () ]
    in
    let blank = div ~a:[ a_class [ "nav-menu__item-blank"; "noselect" ] ] [] in
    let body =
      match user with
      | Some (_, account, _) ->
          [
            div
              ~a:[ a_class [ "nav-menu__item"; "noselect" ] ]
              [
                div
                  ~a:[ a_id "nav_username" ]
                  [ a ~service:Web_services.account [ txt account.name ] () ];
                img
                  ~a:[ a_id "avatar" ]
                  ~src:(static "avatar.png") ~alt:"Avatar" ();
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
                  ~a:[ a_id ("login_" ^ name) ]
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

  let try_new_ui l uuid =
    let@ () =
     fun cont -> if !Web_config.restricted_mode then txt "" else cont ()
    in
    let open (val l : Belenios_ui.I18n.GETTEXT) in
    div
    @@
    let str, xml =
      let s_ = Belenios_ui.I18n.s_xml s_ in
      s_
        "<div><b>New:</b> try the <a>experimental interface</a> (more \
         ergonomic)</div>"
    in
    match xml with
    | Element ("div", [], xs) ->
        List.map
          (function
            | Xml_light_types.PCData x -> txt x
            | Element ("b", [], [ PCData x ]) ->
                span ~a:[ a_class [ "markup-b" ] ] [ txt x ]
            | Element ("a", [], [ PCData x ]) ->
                Eliom_content.Html.F.Raw.a
                  ~a:[ a_href (make_admin_new_uri uuid); a_id "experimental" ]
                  [ txt x ]
            | _ -> txt str)
          xs
    | _ -> [ txt str ]

  let script_with_lang ~lang file =
    let file = static file in
    let dir = Filename.dirname (string_of_uri file) in
    div
      [
        Printf.ksprintf Unsafe.data
          "<script>var belenios_lang = %S; var belenios_dir = %S;</script>" lang
          (dir ^ "/");
        script ~a:[ a_src file ] (txt "");
      ]

  let election_admin ?shuffle_token ?tally_token s election metadata status () =
    let langs = get_languages metadata.e_languages in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let title = template.t_name ^^^ s_ "Administration" in
    let* dates = Web_persist.get_election_automatic_dates s uuid in
    let auto_form () =
      let format = function
        | None -> ""
        | Some x -> Datetime.format @@ Datetime.from_unixfloat x
      in
      return
      @@ post_form ~service:election_auto_post
           (fun (lopen, lclose) ->
             [
               div [ txt (s_ "Alternatively, you may set up automatic dates.") ];
               div
                 [
                   b [ txt (s_ "Note:") ];
                   txt " ";
                   txt (s_ "times are in UTC. Now is ");
                   txt (Datetime.format @@ Datetime.now ());
                   txt ".";
                 ];
               div
                 ~a:[ a_style "margin-left: 3em;" ]
                 [
                   div
                     [
                       txt (s_ "Automatically open the election at:");
                       txt " ";
                       input ~name:lopen ~input_type:`Text
                         ~value:(format dates.auto_date_open)
                         string;
                     ];
                   div
                     [
                       txt (s_ "Automatically close the election at:");
                       txt " ";
                       input ~name:lclose ~input_type:`Text
                         ~value:(format dates.auto_date_close)
                         string;
                     ];
                   div
                     [
                       txt
                         (s_
                            "Enter dates in UTC format, as per YYYY-MM-DD \
                             HH:MM:SS, leave empty for no date.");
                     ];
                 ];
               div
                 [
                   input ~input_type:`Submit
                     ~value:(s_ "Change automatic dates")
                     string;
                 ];
             ])
           uuid
    in
    let state_form checked =
      let service, value, msg, msg2 =
        if checked then
          ( election_close,
            s_ "Close election",
            s_ "The election is open. Voters can vote.",
            s_ " You may re-open the election when it is closed." )
        else
          ( election_open,
            s_ "Open election",
            s_ "The election is closed. No one can vote.",
            "" )
      in
      post_form ~service
        (fun () ->
          [
            div ~a:[ a_style "text-align: center;" ] [ txt msg; txt " " ];
            br ();
            input ~input_type:`Submit ~value string;
            txt msg2;
          ])
        uuid
    in
    let* state_div =
      match status.status_state with
      | `Draft -> Lwt.return @@ txt "(should not happen)"
      | `Open ->
          let* auto_form = auto_form () in
          return @@ div [ state_form true; br (); auto_form ]
      | `Closed ->
          let* auto_form = auto_form () in
          return
          @@ div
               [
                 state_form false;
                 br ();
                 auto_form;
                 br ();
                 hr ();
                 post_form ~service:election_compute_encrypted_tally
                   (fun () ->
                     [
                       input ~input_type:`Submit
                         ~value:(s_ "Proceed to vote counting")
                         string;
                       txt " ";
                       txt
                         (s_
                            "Warning: This action is irreversible; the \
                             election will be definitively closed.");
                     ])
                   uuid;
               ]
      | `Shuffling ->
          let* shuffles = Api_elections.get_shuffles s uuid metadata in
          let shufflers = shuffles.shuffles_shufflers in
          let select_disabled =
            List.exists (fun x -> x.shuffler_token <> None) shufflers
          in
          let* table_contents =
            Lwt_list.map_s
              (fun x ->
                let skip, hash, done_ =
                  let mk_skip disabled =
                    post_form ~service:election_shuffler_skip_confirm
                      (fun (nuuid, ntrustee) ->
                        let a = if disabled then [ a_disabled () ] else [] in
                        [
                          input ~input_type:`Hidden ~name:nuuid ~value:uuid
                            (user Uuid.unwrap);
                          input ~input_type:`Hidden ~name:ntrustee
                            ~value:x.shuffler_address string;
                          input ~a ~input_type:`Submit ~value:(s_ "Skip") string;
                        ])
                      ()
                  in
                  match x.shuffler_fingerprint with
                  | None -> (mk_skip false, txt "", false)
                  | Some h ->
                      ( mk_skip true,
                        txt (if h = "" then s_ "(skipped)" else h),
                        true )
                in
                let this_line =
                  match shuffle_token with
                  | Some y when x.shuffler_token = Some y -> true
                  | _ -> false
                in
                let* cell =
                  match x.shuffler_token with
                  | Some token ->
                      let uri = make_trustee_link uuid (`Shuffle token) in
                      let* subject, body = Mails_admin.mail_shuffle langs uri in
                      return
                      @@ div
                           [
                             a_mailto ~dest:x.shuffler_address ~subject ~body
                               (s_ "E-mail");
                             txt " | ";
                             (if this_line then
                                a ~service:election_admin
                                  [ txt (s_ "Hide link") ]
                                  uuid
                              else
                                Raw.a
                                  ~a:
                                    [
                                      a_href (Xml.uri_of_string uri);
                                      a_id "shuffle-link";
                                    ]
                                  [ txt (s_ "Link") ]);
                           ]
                  | None ->
                      return
                      @@ post_form ~service:election_shuffler_select
                           (fun (nuuid, ntrustee) ->
                             let a =
                               if select_disabled || done_ then
                                 [ a_disabled () ]
                               else []
                             in
                             [
                               input ~input_type:`Hidden ~name:nuuid ~value:uuid
                                 (user Uuid.unwrap);
                               input ~input_type:`Hidden ~name:ntrustee
                                 ~value:x.shuffler_address string;
                               input ~a ~input_type:`Submit
                                 ~value:(s_ "Select this trustee") string;
                             ])
                           ()
                in
                let first_line =
                  tr
                    [
                      td [ txt x.shuffler_address ];
                      td [ cell ];
                      td [ (if done_ then txt (s_ "Yes") else txt (s_ "No")) ];
                      td [ skip ];
                      td [ hash ];
                    ]
                in
                let second_line =
                  match (this_line, x.shuffler_token) with
                  | true, Some token ->
                      let uri = make_trustee_link uuid (`Shuffle token) in
                      [
                        tr
                          [
                            td
                              ~a:[ a_colspan 5 ]
                              [
                                txt
                                  (s_ "The link that must be sent to trustee ");
                                txt x.shuffler_address;
                                txt (s_ " is:");
                                br ();
                                txt uri;
                              ];
                          ];
                      ]
                  | _, _ -> []
                in
                return (first_line :: second_line))
              shufflers
          in
          let proceed =
            if List.for_all (fun x -> x.shuffler_fingerprint <> None) shufflers
            then
              post_form ~service:election_decrypt
                (fun () ->
                  [
                    input ~input_type:`Submit
                      ~value:(s_ "Proceed to decryption")
                      string;
                  ])
                uuid
            else txt ""
          in
          return
            (div
               [
                 div
                   [
                     div
                       ~a:[ a_style "text-align: center;" ]
                       [ txt (s_ "Shuffling of ballots") ];
                     table
                       (tr
                          [
                            th [ txt (s_ "Trustee") ];
                            th [];
                            th [ txt (s_ "Done?") ];
                            th [];
                            th [ txt (s_ "Fingerprint") ];
                          ]
                       :: List.flatten table_contents);
                   ];
                 proceed;
               ])
      | `EncryptedTally ->
          let* p = Api_elections.get_partial_decryptions s uuid metadata in
          let threshold_or_not =
            match p.partial_decryptions_threshold with
            | None -> txt ""
            | Some x ->
                txt
                  (" "
                  ^ Printf.sprintf (f_ "At least %d trustee(s) must act.") x)
          in
          let* trustees =
            p.partial_decryptions_trustees
            |> Lwt_list.map_s (fun t ->
                   let this_line =
                     match tally_token with
                     | Some x when x = t.trustee_pd_token -> true
                     | _ -> false
                   in
                   let uri =
                     make_trustee_link uuid (`Decrypt t.trustee_pd_token)
                   in
                   let* mail, link =
                     if t.trustee_pd_address = "server" then
                       return (txt (s_ "(server)"), txt (s_ "(server)"))
                     else
                       let* subject, body =
                         Mails_admin.mail_trustee_tally langs uri
                       in
                       let mail =
                         a_mailto ~dest:t.trustee_pd_address ~subject ~body
                           (s_ "E-mail")
                       in
                       let link =
                         if this_line then
                           a ~service:election_admin
                             [ txt (s_ "Hide link") ]
                             uuid
                         else
                           Raw.a
                             ~a:[ a_href (Xml.uri_of_string uri) ]
                             [ txt (s_ "Link") ]
                       in
                       return (mail, link)
                   in
                   let first_line =
                     tr
                       [
                         td [ txt t.trustee_pd_address ];
                         td [ mail ];
                         td [ link ];
                         td
                           [
                             txt
                               (if t.trustee_pd_done then s_ "Yes" else s_ "No");
                           ];
                       ]
                   in
                   let second_line =
                     if this_line then
                       [
                         tr
                           [
                             td
                               ~a:[ a_colspan 4 ]
                               [
                                 txt
                                   (s_ "The link that must be sent to trustee ");
                                 txt t.trustee_pd_address;
                                 txt (s_ " is:");
                                 br ();
                                 txt uri;
                               ];
                           ];
                       ]
                     else []
                   in
                   return (first_line :: second_line))
          in
          let* release_form =
            match dates.auto_date_publish with
            | Some t ->
                let scheduled =
                  div
                    [
                      Printf.sprintf
                        (f_ "The result is scheduled to be published after %s.")
                        (Datetime.unwrap (Datetime.from_unixfloat t))
                      |> txt;
                    ]
                in
                post_form ~service:election_show_result
                  (fun () ->
                    [
                      scheduled;
                      input ~input_type:`Submit
                        ~value:(s_ "Publish the result as soon as possible")
                        string;
                    ])
                  uuid
                |> return
            | None ->
                let postpone_form =
                  post_form ~service:election_hide_result
                    (fun date ->
                      [
                        div
                          [
                            txt
                              (s_
                                 "You may postpone the publication of the \
                                  election result.");
                          ];
                        div
                          [
                            input ~input_type:`Submit
                              ~value:(s_ "Postpone publication until")
                              string;
                            txt " ";
                            input ~name:date ~input_type:`Text string;
                          ];
                        div
                          [
                            txt
                              (s_
                                 "Enter the date in UTC fornat, as per \
                                  YYYY-MM-DD HH:MM:SS. For example, today is ");
                            txt
                              (String.sub
                                 (string_of_datetime (Datetime.now ()))
                                 1 19);
                            txt ".";
                          ];
                      ])
                    uuid
                in
                let release_form =
                  post_form ~service:election_tally_release
                    (fun () ->
                      [
                        div
                          [
                            txt
                            @@ s_
                                 "You may force the computation of the result \
                                  now, if the required number of trustees have \
                                  done their job, by clicking on the following \
                                  button.";
                            txt " ";
                            txt
                            @@ s_
                                 "Note: no more partial decryptions will be \
                                  allowed.";
                          ];
                        div
                          [
                            input ~input_type:`Submit
                              ~value:(s_ "Compute the result") string;
                          ];
                      ])
                    uuid
                in
                div [ postpone_form; hr (); release_form ] |> return
          in
          let encrypted_tally =
            let link =
              api_a Belenios_api.Endpoints.election_encrypted_tally uuid
                [ txt (s_ "encrypted tally") ]
            in
            div [ txt (s_ "The "); link; txt (s_ " has been computed.") ]
          in
          return
          @@ div
               [
                 encrypted_tally;
                 div
                   [
                     div [ txt (s_ "Awaiting trustees…"); threshold_or_not ];
                     table
                       (tr
                          [
                            th [ txt (s_ "Trustee") ];
                            th [ txt (s_ "E-mail") ];
                            th [ txt (s_ "Link") ];
                            th [ txt (s_ "Done?") ];
                          ]
                       :: List.flatten trustees);
                   ];
                 release_form;
               ]
      | `Tallied ->
          return
          @@ div
               [
                 div
                   [
                     txt (s_ "This election has been tallied.");
                     txt " ";
                     a ~service:election_download_archive
                       [ txt (s_ "Download archive.") ]
                       (uuid, ());
                   ];
               ]
      | `Archived ->
          return
          @@ div
               [
                 txt (s_ "This election is archived.");
                 txt " ";
                 a ~service:election_download_archive
                   [ txt (s_ "Download archive.") ]
                   (uuid, ());
               ]
    in
    let archive_date =
      match status.status_auto_archive_date with
      | None -> txt ""
      | Some t ->
          div
            [
              txt (s_ "This election will be automatically archived after ");
              txt (Datetime.format @@ Datetime.from_unixfloat t);
              txt ".";
            ]
    in
    let div_archive =
      match status.status_state with
      | `Archived -> txt ""
      | _ -> div [ br (); hr (); archive_date ]
    in
    let delete_date =
      let t = status.status_auto_delete_date in
      div
        [
          txt (s_ "This election will be automatically deleted after ");
          txt (Datetime.format @@ Datetime.from_unixfloat t);
          txt ".";
        ]
    in
    let div_delete =
      div
        [
          br ();
          hr ();
          delete_date;
          post_form ~service:election_delete
            (fun () ->
              [
                input ~input_type:`Submit ~value:(s_ "Delete the election")
                  string;
                txt " ";
                txt (s_ "Warning: This action is irreversible.");
              ])
            uuid;
        ]
    in
    let password =
      match metadata.e_auth_config with
      | Some [ { auth_system = "password"; _ } ] -> true
      | _ -> false
    in
    let div_regenpwd =
      if
        password
        && match status.status_state with `Open | `Closed -> true | _ -> false
      then
        div
          [
            a
              ~a:[ a_id "election_regenpwd" ]
              ~service:election_regenpwd
              [ txt (s_ "Regenerate and e-mail a password") ]
              uuid;
          ]
      else txt ""
    in
    let voters =
      api_a Belenios_api.Endpoints.election_voters uuid
        [ txt (s_ "Voter list") ]
    in
    let content =
      [
        try_new_ui l (Some uuid);
        div
          [
            Eliom_content.Html.F.Raw.a
              ~a:[ a_href @@ Xml.uri_of_string @@ get_election_home_url uuid ]
              [ txt (s_ "Election home") ];
          ];
        voters;
        div
          [
            a ~service:election_pretty_records
              [ txt (s_ "Voting records") ]
              (uuid, ());
          ];
        div
          [
            a ~service:election_missing_voters
              [ txt (s_ "Missing voters") ]
              (uuid, ());
          ];
        div [ checkpriv_link l uuid ];
        div_regenpwd;
        hr ();
        div [ state_div ];
        div_archive;
        div_delete;
      ]
    in
    let* login_box = login_box ~cont:(ContSiteElection uuid) () in
    base ~title ~login_box ~content ()

  let regenpwd uuid () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:election_regenpwd_post
        (fun user ->
          [
            div
              [
                txt (s_ "Username:");
                txt " ";
                input ~name:user ~input_type:`Text string;
              ];
            div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
          ])
        uuid
    in
    let content = [ form ] in
    let title = s_ "Regenerate and e-mail a password" in
    let* login_box = login_box ~cont:(ContSiteElection uuid) () in
    base ~title ~login_box ~content ~uuid ()

  let pretty_records s election records () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let title = template.t_name ^^^ s_ "Records" in
    let nrecords = List.length records in
    let records =
      List.map
        (fun { vr_date; vr_username } ->
          tr
            [
              td [ txt @@ Datetime.format @@ Datetime.from_unixfloat vr_date ];
              td [ txt vr_username ];
            ])
        records
    in
    let* voters = Web_persist.get_all_voters s uuid in
    let nvoters = List.length voters in
    let summary =
      div
        [ Printf.ksprintf txt (f_ "Number of records: %d/%d") nrecords nvoters ]
    in
    let table =
      match records with
      | [] -> div [ txt (s_ "Nobody voted!") ]
      | _ ->
          div
            [
              table
                (tr
                   [
                     th [ txt (s_ "Date/Time (UTC)") ];
                     th [ txt (s_ "Username") ];
                   ]
                :: records);
            ]
    in
    let raw_data =
      api_a Belenios_api.Endpoints.election_records uuid [ txt (s_ "raw data") ]
    in
    let content =
      [
        div [ txt (s_ "You can also access the "); raw_data; txt "." ];
        summary;
        table;
      ]
    in
    let* login_box = login_box ~cont:(ContSiteElection uuid) () in
    base ~title ~login_box ~content ()

  let election_shuffler_skip_confirm uuid trustee =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = Printf.sprintf (f_ "Skipping trustee %s") trustee in
    let content =
      [
        post_form ~service:election_shuffler_skip
          (fun (nuuid, ntrustee) ->
            [
              div
                [
                  txt
                    (s_
                       "You may skip a trustee if they do not answer. Be aware \
                        that this reduces the security.");
                ];
              div
                [
                  input ~input_type:`Hidden ~name:nuuid ~value:uuid
                    (user Uuid.unwrap);
                  input ~input_type:`Hidden ~name:ntrustee ~value:trustee string;
                  input ~input_type:`Submit ~value:(s_ "Confirm") string;
                  txt " ";
                  a ~service:Web_services.election_admin
                    [ txt (s_ "Cancel") ]
                    uuid;
                ];
            ])
          ();
      ]
    in
    base ~title ~content ()

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

  let signup_login () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
      post_form ~service:signup_login_post
        (fun lcode ->
          [
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
    let content = [ form ] in
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
              span ~a:[ a_style "color: red;" ] [ txt (s_ "failed") ];
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
              span ~a:[ a_style "color: red;" ] [ txt (s_ "failed") ];
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
        [ script_with_lang ~lang "tool_js_fingerprint.js" ]
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

  let set_email_confirm ~address =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let form =
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
        ()
    in
    let content = [ form ] in
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

  let account account =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let title = s_ "Account settings" in
    let content =
      [
        post_form ~service:account_post
          (fun name ->
            [
              div
                [
                  txt (s_ "Name:");
                  txt " ";
                  input ~input_type:`Text ~name ~value:account.name string;
                ];
              div
                [
                  txt (s_ "E-mail address:");
                  txt " ";
                  txt @@ Option.value ~default:"(none)" account.email;
                ];
              div
                [
                  txt (s_ "Authentication methods:");
                  txt " ";
                  ul
                    (List.map
                       (fun u ->
                         li
                           [
                             Printf.ksprintf txt "%s:%s" u.user_domain
                               u.user_name;
                           ])
                       account.authentications);
                ];
              div
                [
                  txt (s_ "Consent date:");
                  txt " ";
                  txt
                    (match account.consent with
                    | None -> s_ "(none)"
                    | Some t -> Datetime.format t);
                ];
              div [ input ~input_type:`Submit ~value:(s_ "Submit") string ];
            ])
          ();
      ]
    in
    base ~title ~content ()
end

let mail_confirmation_link l address code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Your e-mail address has been used to create an account on our Belenios \
        server.");
  add_sentence b (s_ "To confirm this creation, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.server_name;
  let body = contents b in
  let subject = !Web_config.vendor ^^^ s_ "Create account" in
  (subject, body)

let mail_changepw_link l address code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "There has been a request to change the password of your account on our \
        Belenios server.");
  add_sentence b (s_ "To confirm this, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.server_name;
  let body = contents b in
  let subject = !Web_config.vendor ^^^ s_ "Change password" in
  (subject, body)

let mail_set_email l address code =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let open Belenios_ui.Mail_formatter in
  let b = create () in
  add_sentence b (Printf.sprintf (f_ "Dear %s,") address);
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Someone is trying to associate your e-mail address to an account on \
        our Belenios server.");
  add_sentence b (s_ "To confirm this, please use the following code:");
  add_newline b;
  add_newline b;
  add_string b "  ";
  add_string b code;
  add_newline b;
  add_newline b;
  add_sentence b
    (s_
       "Warning: this code is valid for 15 minutes, and previous codes sent to \
        this address are no longer valid.");
  add_newline b;
  add_newline b;
  add_sentence b (s_ "Best regards,");
  add_newline b;
  add_newline b;
  add_string b "-- ";
  add_newline b;
  add_string b !Web_config.server_name;
  let body = contents b in
  let subject = !Web_config.vendor ^^^ s_ "Change e-mail address" in
  (subject, body)
