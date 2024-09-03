(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Eliom_service
open Eliom_parameter
open Web_common

module Make () = struct
  let uuid_and_token = uuid "uuid" ** string "token"
  let banner = create ~path:(Path [ "banner" ]) ~meth:(Get (string "lang")) ()
  let home = create ~path:(Path [ "" ]) ~meth:(Get unit) ()
  let apps = create ~path:(Path []) ~meth:(Get (suffix (string "page"))) ()

  let default_rewrite_prefix x =
    let prefix = Eliom_uri.make_string_uri ~absolute:true ~service:home () in
    let nprefix = String.length prefix in
    if String.starts_with ~prefix x then
      String.concat "/"
        [ !Web_config.prefix; String.sub x nprefix (String.length x - nprefix) ]
    else x

  let rewrite_fun = ref default_rewrite_prefix

  let set_rewrite_prefix ~src ~dst =
    let prefix = if String.ends_with ~suffix:"/" src then src else src ^ "/" in
    let dst = if String.ends_with ~suffix:"/" dst then dst else dst ^ "/" in
    let nprefix = String.length prefix in
    let f x =
      if String.starts_with ~prefix x then
        dst ^ String.sub x nprefix (String.length x - nprefix)
      else x
    in
    rewrite_fun := f

  let make_absolute_string_uri ?fragment ~service x =
    (* We assume fragment is already encoded, that's why we don't use
       the labelled argument of Eliom_uri.make_string_uri. *)
    Eliom_uri.make_string_uri ~absolute:true ~service x |> !rewrite_fun
    |> fun x ->
    match fragment with None -> x | Some y -> String.concat "#" [ x; y ]

  let admin_basic () =
    Eliom_service.preapply
      ~service:(Eliom_service.static_dir ())
      [ "static"; "admin_basic.html" ]

  let admin_new () = Eliom_service.preapply ~service:apps "admin"

  let make_admin_new_uri uuid =
    let fragment = Option.map Belenios.Uuid.unwrap uuid in
    let base = make_absolute_string_uri ?fragment ~service:(admin_new ()) () in
    Eliom_content.Xml.uri_of_string base

  let make_trustee_link uuid kind =
    let uuid = Belenios.Uuid.unwrap uuid in
    let fragment =
      match kind with
      | `Generate token -> Printf.sprintf "generate/%s/%s" uuid token
      | `Decrypt token -> Printf.sprintf "decrypt/%s/%s" uuid token
      | `Shuffle token -> Printf.sprintf "shuffle/%s/%s" uuid token
      | `Check -> Printf.sprintf "check/%s" uuid
    in
    make_absolute_string_uri ~fragment ~service:apps "trustee"

  let make_credauth_link uuid kind =
    let uuid = Belenios.Uuid.unwrap uuid in
    let fragment =
      match kind with
      | `Generate token -> Printf.sprintf "generate/%s/%s" uuid token
    in
    make_absolute_string_uri ~fragment ~service:apps "credauth"

  let privacy_notice_accept =
    create ~path:No_path ~csrf_safe:true
      ~meth:(Post (unit, privacy_cont "cont"))
      ()

  let site_login =
    create ~path:(Path [ "login" ])
      ~meth:(Get (opt (string "service") ** site_cont "cont"))
      ()

  let logout =
    create ~path:(Path [ "logout" ]) ~meth:(Get (site_cont "cont")) ()

  let source_code =
    create ~path:(Path [ "belenios.tar.gz" ]) ~meth:(Get unit) ()

  let logo = create ~path:(Path [ "LOGO" ]) ~meth:(Get unit) ()
  let favicon = create ~path:(Path [ "favicon.ico" ]) ~meth:(Get unit) ()
  let sealing = create ~path:(Path [ "SEALING" ]) ~meth:(Get unit) ()

  let election_draft_new =
    create_attached_post ~csrf_safe:true ~fallback:home
      ~post_params:
        (radio string "credmgmt" ** radio string "auth"
        ** opt (string "cas_server"))
      ()

  let election_draft_pre =
    create ~path:(Path [ "draft"; "new" ]) ~meth:(Get unit) ()

  let election_draft =
    create ~path:(Path [ "draft"; "election" ]) ~meth:(Get (uuid "uuid")) ()

  let election_draft_questions =
    create ~path:(Path [ "draft"; "questions" ]) ~meth:(Get (uuid "uuid")) ()

  let election_draft_questions_post =
    create_attached_post ~csrf_safe:true ~fallback:election_draft_questions
      ~post_params:(string "questions" ** int "booth_version")
      ()

  let election_draft_description =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:(string "name" ** string "description")
      ()

  let election_draft_languages =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:(string "languages") ()

  let election_draft_contact =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:(string "contact") ()

  let election_draft_admin_name =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:(string "name") ()

  let election_draft_voters =
    create ~path:(Path [ "draft"; "voters" ]) ~meth:(Get (uuid "uuid")) ()

  let election_draft_voters_add =
    create_attached_post ~csrf_safe:true ~fallback:election_draft_voters
      ~post_params:(string "voters") ()

  let election_draft_voters_remove =
    create_attached_post ~csrf_safe:true ~fallback:election_draft_voters
      ~post_params:(string "voter") ()

  let election_draft_voters_remove_all =
    create_attached_post ~csrf_safe:true ~fallback:election_draft_voters
      ~post_params:unit ()

  let election_draft_voters_passwd =
    create_attached_post ~csrf_safe:true ~fallback:election_draft_voters
      ~post_params:(string "voter") ()

  let election_draft_trustee_add =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:(string "id" ** string "name")
      ()

  let election_draft_trustee_del =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:(string "address") ()

  let election_draft_credential_authority =
    create
      ~path:(Path [ "draft"; "credential-authority" ])
      ~meth:(Get (uuid "uuid"))
      ()

  let election_draft_set_credential_authority =
    create_attached_post ~csrf_safe:true
      ~fallback:election_draft_credential_authority ~post_params:(string "name")
      ()

  let election_draft_credentials =
    create
      ~path:(Path [ "draft"; "credentials" ])
      ~meth:(Get (suffix uuid_and_token))
      ()

  let election_draft_credentials_server =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:unit ()

  let election_draft_credentials_get =
    create
      ~path:(Path [ "draft"; "get-credentials" ])
      ~meth:(Get (uuid "uuid"))
      ()

  let election_draft_trustees =
    create ~path:(Path [ "draft"; "trustees" ]) ~meth:(Get (uuid "uuid")) ()

  let election_draft_trustee =
    create
      ~path:(Path [ "draft"; "trustee" ])
      ~meth:(Get (suffix uuid_and_token))
      ()

  let election_draft_threshold_trustees =
    create
      ~path:(Path [ "draft"; "threshold-trustees" ])
      ~meth:(Get (uuid "uuid"))
      ()

  let election_draft_threshold_trustee =
    create
      ~path:(Path [ "draft"; "threshold-trustee" ])
      ~meth:(Get (suffix uuid_and_token))
      ()

  let election_draft_threshold_set =
    create_attached_post ~csrf_safe:true
      ~fallback:election_draft_threshold_trustees ~post_params:(int "threshold")
      ()

  let election_draft_threshold_trustee_add =
    create_attached_post ~csrf_safe:true
      ~fallback:election_draft_threshold_trustees
      ~post_params:(string "id" ** string "name")
      ()

  let election_draft_threshold_trustee_del =
    create_attached_post ~csrf_safe:true
      ~fallback:election_draft_threshold_trustees
      ~post_params:(string "address") ()

  let election_draft_confirm =
    create ~path:(Path [ "draft"; "confirm" ]) ~meth:(Get (uuid "uuid")) ()

  let election_draft_prebilling =
    create
      ~path:(Path [ "draft"; "prebilling" ])
      ~meth:(Get (string "id" ** site_cont "cont"))
      ()

  let election_draft_postbilling =
    create ~path:(Path [ "draft"; "postbilling" ]) ~meth:(Get unit) ()

  let election_draft_create =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:unit ()

  let election_draft_destroy =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:unit ()

  let election_draft_auth_genpwd =
    create_attached_post ~csrf_safe:true ~fallback:election_draft
      ~post_params:unit ()

  let election_draft_import =
    create ~path:(Path [ "draft"; "import" ]) ~meth:(Get (uuid "uuid")) ()

  let election_draft_import_post =
    create_attached_post ~csrf_safe:true ~fallback:election_draft_import
      ~post_params:(string "from") ()

  let election_draft_import_trustees =
    create
      ~path:(Path [ "draft"; "import-trustees" ])
      ~meth:(Get (uuid "uuid"))
      ()

  let election_draft_import_trustees_post =
    create_attached_post ~csrf_safe:true
      ~fallback:election_draft_import_trustees ~post_params:(string "from") ()

  let election_home_dir =
    create ~path:(Path [ "elections" ]) ~meth:(Get (suffix (uuid "uuid"))) ()

  let election_home =
    create ~path:(Path [ "elections" ])
      ~meth:(Get (suffix (uuid "uuid" ** suffix_const "")))
      ()

  let set_consent = create ~path:No_path ~meth:(Get (site_cont "cont")) ()

  let election_admin =
    create ~path:(Path [ "actions"; "admin" ]) ~meth:(Get (uuid "uuid")) ()

  let election_regenpwd =
    create ~path:(Path [ "actions"; "regenpwd" ]) ~meth:(Get (uuid "uuid")) ()

  let election_regenpwd_post =
    create_attached_post ~csrf_safe:true ~fallback:election_regenpwd
      ~post_params:(string "user") ()

  let election_login =
    create ~path:(Path [ "elections" ])
      ~meth:
        (Get
           (suffix_prod
              (uuid "uuid" ** suffix_const "login")
              (opt (string "service"))))
      ()

  let election_open =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:unit ()

  let election_close =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:unit ()

  let election_hide_result =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:(string "date") ()

  let election_show_result =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:unit ()

  let election_auto_post =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:(string "open" ** string "close")
      ()

  let election_delete =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:unit ()

  let booth_v2 () =
    Eliom_service.preapply
      ~service:(Eliom_service.static_dir ())
      [ "static"; "frontend"; "booth"; "vote.html" ]

  type booth =
    | Booth :
        (unit ->
        ( unit,
          unit,
          get,
          att,
          non_co,
          non_ext,
          'reg,
          [ `WithoutSuffix ],
          unit,
          unit,
          non_ocaml )
        Eliom_service.t)
        -> booth

  let booths = [| (Booth booth_v2, "Version 2") |]

  let election_cast =
    create ~path:(Path [ "actions"; "cast" ]) ~meth:(Get (uuid "uuid")) ()

  let election_submit_ballot =
    create
      ~path:(Path [ "actions"; "submit-ballot" ])
      ~meth:(Post (unit, string "encrypted_vote"))
      ()

  let election_submit_ballot_file =
    create
      ~path:(Path [ "actions"; "submit-ballot-file" ])
      ~meth:(Post (unit, file "encrypted_vote"))
      ()

  let election_submit_ballot_check =
    create ~path:(Path [ "actions"; "submit-ballot-check" ]) ~meth:(Get unit) ()

  let election_cast_confirm =
    create ~path:(Path [ "actions"; "confirm" ]) ~meth:(Get (uuid "uuid")) ()

  let election_pretty_ballots =
    create ~path:(Path [ "elections" ])
      ~meth:(Get (suffix (uuid "uuid" ** suffix_const "ballots")))
      ()

  let election_pretty_ballot =
    create ~path:(Path [ "elections" ])
      ~meth:
        (Get
           (suffix_prod (uuid "uuid" ** suffix_const "ballot") (string "hash")))
      ()

  let election_pretty_records =
    create ~path:(Path [ "elections" ])
      ~meth:(Get (suffix (uuid "uuid" ** suffix_const "pretty-records")))
      ()

  let election_project_result =
    create ~path:(Path [ "elections" ])
      ~meth:
        (Get
           (suffix_prod
              (uuid "uuid" ** suffix_const "project-result")
              (int "index")))
      ()

  let election_missing_voters =
    create ~path:(Path [ "elections" ])
      ~meth:(Get (suffix (uuid "uuid" ** suffix_const "missing")))
      ()

  let election_download_archive =
    create ~path:(Path [ "elections" ])
      ~meth:(Get (suffix (uuid "uuid" ** suffix_const "archive.zip")))
      ()

  let election_compute_encrypted_tally =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:unit ()

  let election_shuffle_link =
    create
      ~path:(Path [ "actions"; "shuffle" ])
      ~meth:(Get (suffix uuid_and_token))
      ()

  let election_shuffler_select =
    create ~csrf_safe:true ~path:No_path
      ~meth:(Post (unit, uuid "uuid" ** string "trustee"))
      ()

  let election_shuffler_skip_confirm =
    create ~csrf_safe:true ~path:No_path
      ~meth:(Post (unit, uuid "uuid" ** string "trustee"))
      ()

  let election_shuffler_skip =
    create ~csrf_safe:true ~path:No_path
      ~meth:(Post (unit, uuid "uuid" ** string "trustee"))
      ()

  let election_decrypt =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:unit ()

  let election_tally_trustees =
    create
      ~path:(Path [ "actions"; "trustees" ])
      ~meth:(Get (suffix uuid_and_token))
      ()

  let election_tally_release =
    create_attached_post ~csrf_safe:true ~fallback:election_admin
      ~post_params:unit ()

  let dummy_post =
    create ~csrf_safe:true ~path:No_path
      ~meth:(Post (unit, string "state" ** string "username"))
      ()

  let email_post =
    create ~csrf_safe:true ~path:No_path
      ~meth:(Post (unit, string "state" ** string "username"))
      ()

  let email_election_login = create ~path:No_path ~meth:(Get unit) ()

  let email_captcha_post =
    create ~csrf_safe:true ~path:No_path
      ~meth:
        (Post
           ( unit,
             string "state" ** string "challenge" ** string "response"
             ** string "username" ))
      ()

  let email_login_post =
    create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "code")) ()

  let password_post =
    create ~csrf_safe:true ~path:No_path
      ~meth:
        (Post (unit, string "state" ** string "username" ** string "password"))
      ()

  let set_language =
    create ~csrf_safe:true ~path:No_path
      ~meth:(Get (string "lang" ** site_cont "cont"))
      ()

  let signup_captcha =
    create ~path:(Path [ "signup"; "" ]) ~meth:(Get (string "service")) ()

  let signup_captcha_post =
    create_attached_post ~csrf_safe:true ~fallback:signup_captcha
      ~post_params:(string "challenge" ** string "response" ** string "email")
      ()

  let signup_captcha_img =
    create
      ~path:(Path [ "signup"; "captcha" ])
      ~meth:(Get (string "challenge"))
      ()

  let signup_login_post =
    create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "code")) ()

  let signup = create ~path:(Path [ "signup"; "account" ]) ~meth:(Get unit) ()

  let signup_post =
    create_attached_post ~csrf_safe:true ~fallback:signup
      ~post_params:(string "username" ** string "password" ** string "password2")
      ()

  let changepw_captcha =
    create
      ~path:(Path [ "signup"; "changepw" ])
      ~meth:(Get (string "service"))
      ()

  let changepw_captcha_post =
    create_attached_post ~csrf_safe:true ~fallback:changepw_captcha
      ~post_params:
        (string "challenge" ** string "response" ** string "email"
       ** string "username")
      ()

  let changepw_post =
    create_attached_post ~csrf_safe:true ~fallback:signup
      ~post_params:(string "password" ** string "password2")
      ()

  let compute_fingerprint =
    create ~path:(Path [ "tools"; "compute-fingerprint" ]) ~meth:(Get unit) ()

  let set_email_post =
    create_attached_post ~csrf_safe:true ~fallback:home
      ~post_params:(string "email") ()

  let set_email_confirm =
    create_attached_post ~csrf_safe:true ~fallback:home
      ~post_params:(string "code") ()

  let sudo = create ~path:(Path [ "sudo" ]) ~meth:(Get unit) ()

  let sudo_post =
    create_attached_post ~csrf_safe:true ~fallback:sudo
      ~post_params:(string "domain" ** string "name")
      ()

  let account = create ~path:(Path [ "account" ]) ~meth:(Get unit) ()

  let account_post =
    create_attached_post ~csrf_safe:true ~fallback:account
      ~post_params:(string "name") ()

  let api_token = create ~path:(Path [ "api-token" ]) ~meth:(Get unit) ()
end
