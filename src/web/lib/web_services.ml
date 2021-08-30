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

open Eliom_service
open Eliom_parameter
open Web_common

module Make () = struct

  let uuid_and_token = uuid "uuid" ** string "token"

  let home = create ~path:(Path [""]) ~meth:(Get unit) ()
  let admin = create ~path:(Path ["admin"]) ~meth:(Get unit) ()
  let privacy_notice_accept = create ~path:No_path ~csrf_safe:true ~meth:(Post (unit, privacy_cont "cont")) ()
  let site_login = create ~path:(Path ["login"]) ~meth:(Get (opt (string "service") ** site_cont "cont")) ()
  let logout = create ~path:(Path ["logout"]) ~meth:(Get (site_cont "cont")) ()

  let source_code = create ~path:(Path ["belenios.tar.gz"]) ~meth:(Get unit) ()

  let election_draft_new = create_attached_post ~csrf_safe:true ~fallback:admin ~post_params:(radio string "credmgmt" ** radio string "auth" ** opt (string "cas_server")) ()
  let election_draft_pre = create ~path:(Path ["draft"; "new"]) ~meth:(Get unit) ()
  let election_draft = create ~path:(Path ["draft"; "election"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_questions = create ~path:(Path ["draft"; "questions"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_questions_post = create_attached_post ~csrf_safe:true ~fallback:election_draft_questions ~post_params:(string "questions" ** int "booth_version") ()
  let election_draft_preview = create ~path:(Path ["draft"; "preview"]) ~meth:(Get (suffix (uuid "uuid" ** suffix_const "election.json"))) ()
  let election_draft_description = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:(string "name" ** string "description") ()
  let election_draft_languages = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:(string "languages") ()
  let election_draft_contact = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:(string "contact") ()
  let election_draft_admin_name = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:(string "name") ()
  let election_draft_voters = create ~path:(Path ["draft"; "voters"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_voters_add = create_attached_post ~csrf_safe:true ~fallback:election_draft_voters ~post_params:(string "voters") ()
  let election_draft_voters_remove = create_attached_post ~csrf_safe:true ~fallback:election_draft_voters ~post_params:(string "voter") ()
  let election_draft_voters_remove_all = create_attached_post ~csrf_safe:true ~fallback:election_draft_voters ~post_params:unit ()
  let election_draft_voters_passwd = create_attached_post ~csrf_safe:true ~fallback:election_draft_voters ~post_params:(string "voter") ()
  let election_draft_trustee_add = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:(string "id" ** string "name") ()
  let election_draft_trustee_del = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:(int "index") ()
  let election_draft_credential_authority = create ~path:(Path ["draft"; "credential-authority"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_set_credential_authority = create_attached_post ~csrf_safe:true ~fallback:election_draft_credential_authority ~post_params:(string "name") ()
  let election_draft_credentials = create ~path:(Path ["draft"; "credentials"]) ~meth:(Get uuid_and_token) ()
  let election_draft_credentials_post = create ~csrf_safe:true ~path:(Path ["draft"; "submit-credentials"]) ~meth:(Post (uuid_and_token, string "public_creds")) ()
  let election_draft_credentials_post_file = create ~csrf_safe:true ~path:(Path ["draft"; "submit-credentials-file"]) ~meth:(Post (uuid_and_token, file "public_creds")) ()
  let election_draft_credentials_server = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:unit ()
  let election_draft_credentials_get = create ~path:(Path ["draft"; "get-credentials"]) ~meth:(Get (uuid "uuid")) ()

  let election_draft_trustees = create ~path:(Path ["draft"; "trustees"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_trustee = create ~path:(Path ["draft"; "trustee"]) ~meth:(Get uuid_and_token) ()
  let election_draft_trustee_post = create ~csrf_safe:true ~path:(Path ["draft"; "submit-trustee"]) ~meth:(Post (uuid_and_token, string "public_key")) ()

  let election_draft_threshold_trustees = create ~path:(Path ["draft"; "threshold-trustees"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_threshold_trustee = create ~path:(Path ["draft"; "threshold-trustee"]) ~meth:(Get uuid_and_token) ()
  let election_draft_threshold_trustee_post = create ~csrf_safe:true ~path:(Path ["draft"; "submit-threshold-trustee"]) ~meth:(Post (uuid_and_token, string "data")) ()
  let election_draft_threshold_set = create_attached_post ~csrf_safe:true ~fallback:election_draft_threshold_trustees ~post_params:(int "threshold") ()
  let election_draft_threshold_trustee_add = create_attached_post ~csrf_safe:true ~fallback:election_draft_threshold_trustees ~post_params:(string "id" ** string "name") ()
  let election_draft_threshold_trustee_del = create_attached_post ~csrf_safe:true ~fallback:election_draft_threshold_trustees ~post_params:(int "index") ()

  let election_draft_confirm = create ~path:(Path ["draft"; "confirm"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_create = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:unit ()
  let election_draft_destroy = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:unit ()
  let election_draft_auth_genpwd = create_attached_post ~csrf_safe:true ~fallback:election_draft ~post_params:unit ()

  let election_draft_import = create ~path:(Path ["draft"; "import"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_import_post = create_attached_post ~csrf_safe:true ~fallback:election_draft_import ~post_params:(string "from") ()
  let election_draft_import_trustees = create ~path:(Path ["draft"; "import-trustees"]) ~meth:(Get (uuid "uuid")) ()
  let election_draft_import_trustees_post = create_attached_post ~csrf_safe:true ~fallback:election_draft_import_trustees ~post_params:(string "from") ()

  let election_home_dir = create ~path:(Path ["elections"]) ~meth:(Get (suffix (uuid "uuid"))) ()
  let election_home = create ~path:(Path ["elections"]) ~meth:(Get (suffix (uuid "uuid" ** suffix_const ""))) ()
  let set_cookie_disclaimer = create ~path:No_path ~meth:(Get (site_cont "cont")) ()
  let election_admin = create ~path:(Path ["election"; "admin"]) ~meth:(Get (uuid "uuid")) ()
  let election_regenpwd = create ~path:(Path ["election"; "regenpwd"]) ~meth:(Get (uuid "uuid")) ()
  let election_regenpwd_post = create_attached_post ~csrf_safe:true ~fallback:election_regenpwd ~post_params:(string "user") ()
  let election_login = create ~path:(Path ["elections"]) ~meth:(Get (suffix_prod (uuid "uuid" ** suffix_const "login") (opt (string "service")))) ()
  let election_open = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:unit ()
  let election_close = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:unit ()
  let election_hide_result = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:(string "date") ()
  let election_show_result = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:unit ()
  let election_auto_post = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:(string "open" ** string "close") ()
  let election_delete = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:unit ()

  let booth_v1 = create ~path:(Path ["vote.html"]) ~meth:(Get unit) ()

  let booth_v2 () =
    Eliom_service.preapply ~service:(Eliom_service.static_dir ())
      ["static"; "frontend"; "booth"; "vote.html"]

  type booth =
    | Booth :
        (unit ->
         (unit, unit, get, att, non_co, non_ext, 'reg, [ `WithoutSuffix ],
          unit, unit, non_ocaml) Eliom_service.t) ->
        booth

  let booths =
    [|
      Booth (fun () -> booth_v1), "Version 1";
      Booth booth_v2, "Version 2";
    |]

  let election_cast = create ~path:(Path ["election"; "cast"]) ~meth:(Get (uuid "uuid")) ()
  let election_submit_ballot = create ~path:(Path ["election"; "submit-ballot"]) ~meth:(Post (unit, string "encrypted_vote")) ()
  let election_submit_ballot_file = create ~path:(Path ["election"; "submit-ballot-file"]) ~meth:(Post (unit, file "encrypted_vote")) ()
  let election_submit_ballot_check = create ~path:(Path ["election"; "submit-ballot-check"]) ~meth:(Get unit) ()
  let election_cast_fallback = create ~path:(Path ["election"; "confirm"]) ~meth:(Get (uuid "uuid")) ()
  let election_cast_confirm = create_attached_post ~csrf_safe:true ~fallback:election_cast_fallback ~post_params:unit ()
  let election_pretty_ballots = create ~path:(Path ["elections"]) ~meth:(Get (suffix (uuid "uuid" ** suffix_const "ballots"))) ()
  let election_pretty_ballot = create ~path:(Path ["elections"]) ~meth:(Get (suffix_prod (uuid "uuid" ** suffix_const "ballot") (string "hash"))) ()
  let election_pretty_records = create ~path:(Path ["elections"]) ~meth:(Get (suffix (uuid "uuid" ** suffix_const "pretty-records"))) ()
  let election_project_result = create ~path:(Path ["elections"]) ~meth:(Get (suffix_prod (uuid "uuid" ** suffix_const "project-result") (int "index"))) ()

  let election_missing_voters = create ~path:(Path ["elections"]) ~meth:(Get (suffix (uuid "uuid" ** suffix_const "missing"))) ()
  let election_download_archive = create ~path:(Path ["elections"]) ~meth:(Get (suffix (uuid "uuid" ** suffix_const "archive.zip"))) ()

  let election_compute_encrypted_tally = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:unit ()
  let election_nh_ciphertexts = create ~path:(Path ["election"; "nh-ciphertexts"]) ~meth:(Get (uuid "uuid")) ()
  let election_shuffle_link = create ~path:(Path ["election"; "shuffle"]) ~meth:(Get uuid_and_token) ()
  let election_shuffle_post = create ~path:(Path ["election"; "submit-shuffle"]) ~meth:(Post (uuid_and_token, string "shuffle")) ()
  let election_shuffler_select = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, uuid "uuid" ** string "trustee")) ()
  let election_shuffler_skip_confirm = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, uuid "uuid" ** string "trustee")) ()
  let election_shuffler_skip = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, uuid "uuid" ** string "trustee")) ()
  let election_decrypt = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:unit ()
  let election_tally_trustees = create ~path:(Path ["election"; "trustees"]) ~meth:(Get uuid_and_token) ()
  let election_tally_trustees_post = create ~csrf_safe:true ~path:(Path ["election"; "submit-partial-decryption"]) ~meth:(Post (uuid_and_token, string "partial_decryption")) ()
  let election_tally_release = create_attached_post ~csrf_safe:true ~fallback:election_admin ~post_params:unit ()

  let election_dir = create ~path:(Path ["elections"]) ~meth:(Get (suffix (uuid "uuid" ** election_file "file"))) ()

  let dummy_post = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "state" ** string "username")) ()
  let email_post = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "state" ** string "username")) ()
  let email_captcha_post = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "state" ** string "challenge" ** string "response" ** string "username")) ()
  let email_login_post = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "code")) ()
  let password_post = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "state" ** string "username" ** string "password")) ()

  let set_language = create ~csrf_safe:true ~path:No_path ~meth:(Get (string "lang" ** site_cont "cont")) ()

  let signup_captcha = create ~path:(Path ["signup"; ""]) ~meth:(Get (string "service")) ()
  let signup_captcha_post = create_attached_post ~csrf_safe:true ~fallback:signup_captcha ~post_params:(string "challenge" ** string "response" ** string "email") ()
  let signup_captcha_img = create ~path:(Path ["signup"; "captcha"]) ~meth:(Get (string "challenge")) ()
  let signup_login_post = create ~csrf_safe:true ~path:No_path ~meth:(Post (unit, string "code")) ()
  let signup = create ~path:(Path ["signup"; "account"]) ~meth:(Get unit) ()
  let signup_post = create_attached_post ~csrf_safe:true ~fallback:signup ~post_params:(string "username" ** string "password" ** string "password2") ()

  let changepw_captcha = create ~path:(Path ["signup"; "changepw"]) ~meth:(Get (string "service")) ()
  let changepw_captcha_post = create_attached_post ~csrf_safe:true ~fallback:changepw_captcha ~post_params:(string "challenge" ** string "response" ** string "email" ** string "username") ()
  let changepw_post = create_attached_post ~csrf_safe:true ~fallback:signup ~post_params:(string "password" ** string "password2") ()

  let method_schulze = create ~path:(Path ["methods"; "schulze"]) ~meth:(Get (uuid "uuid" ** int "question")) ()
  let method_mj = create ~path:(Path ["methods"; "mj"]) ~meth:(Get (uuid "uuid" ** int "question" ** opt (int "ngrades"))) ()
  let method_stv = create ~path:(Path ["methods"; "stv"]) ~meth:(Get (uuid "uuid" ** int "question" ** opt (int "nseats"))) ()

  let compute_fingerprint = create ~path:(Path ["compute-fingerprint"]) ~meth:(Get unit) ()

  let set_email_post = create_attached_post ~csrf_safe:true ~fallback:admin ~post_params:(string "email") ()
  let set_email_confirm = create_attached_post ~csrf_safe:true ~fallback:admin ~post_params:(string "code") ()

  let sudo = create ~path:(Path ["sudo"]) ~meth:(Get unit) ()
  let sudo_post = create_attached_post ~csrf_safe:true ~fallback:sudo ~post_params:(string "domain" ** string "name") ()
end
