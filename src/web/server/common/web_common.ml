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
open Belenios_platform
open Belenios_core
open Platform
open Common
open Serializable_t
open Web_serializable_j

let ( let&* ) x f = match x with None -> Lwt.return_none | Some x -> f x
let ( !! ) x = !Web_config.spool_dir // x
let ( /// ) uuid x = !!(Uuid.unwrap uuid // x)

module Datetime = Web_types.Datetime
module Period = Web_types.Period

module Random = struct
  let init_prng () = lazy (pseudo_rng (random_string secure_rng 16))
  let prng = ref (init_prng ())

  let () =
    let rec loop () =
      let* () = Lwt_unix.sleep 1800. in
      prng := init_prng ();
      loop ()
    in
    Lwt.async loop

  let random q =
    let size = bytes_to_sample q in
    let r = random_string (Lazy.force !prng) size in
    Z.(of_bits r mod q)
end

type error =
  | ElectionClosed
  | UnauthorizedVoter
  | CastError of Signatures.cast_error

exception BeleniosWebError of error

let fail e = Lwt.fail (BeleniosWebError e)

let explain_error l e =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  match e with
  | ElectionClosed -> s_ "the election is closed"
  | UnauthorizedVoter -> s_ "you are not allowed to vote"
  | CastError (`SerializationError e) ->
      Printf.sprintf
        (f_ "your ballot has a syntax error (%s)")
        (Printexc.to_string e)
  | CastError `NonCanonical -> s_ "your ballot is not in canonical form"
  | CastError `InvalidBallot -> s_ "some proofs failed verification"
  | CastError `InvalidCredential -> s_ "your credential is invalid"
  | CastError `RevoteNotAllowed -> s_ "you are not allowed to revote"
  | CastError `UsedCredential -> s_ "your credential has already been used"
  | CastError `WrongCredential ->
      s_ "you are not allowed to vote with this credential"
  | CastError `WrongWeight -> s_ "your credential has a bad weight"
  | CastError `DuplicateBallot -> s_ "this ballot has already been accepted"
  | CastError `ExpiredBallot -> s_ "this ballot has expired"
  | CastError `WrongUsername -> s_ "your username is wrong"

let decompose_seconds s =
  let s = float_of_int s in
  let h = int_of_float (s /. 3600.) in
  let s = s -. (float_of_int h *. 3600.) in
  let m = int_of_float (s /. 60.) in
  let s = s -. (float_of_int m *. 60.) in
  (h, m, int_of_float s)

let format_period l x =
  let open (val l : Belenios_ui.I18n.GETTEXT) in
  let y, m, d, s = Period.ymds x in
  let y = if y = 0 then "" else string_of_int y ^ s_ " year(s)" in
  let m = if m = 0 then "" else string_of_int m ^ s_ " month(s)" in
  let d = if d = 0 then "" else string_of_int d ^ s_ " day(s)" in
  let hrs, min, sec = decompose_seconds s in
  let hrs = if hrs = 0 then "" else string_of_int hrs ^ s_ " hour(s)" in
  let min = if min = 0 then "" else string_of_int min ^ s_ " minute(s)" in
  let sec = if sec = 0 then "" else string_of_int sec ^ s_ " second(s)" in
  let approx =
    String.concat " " (List.filter (fun x -> x <> "") [ y; m; d; hrs; min ])
  in
  if approx = "" then sec else approx

let security_logfile = ref None

let open_security_log f =
  let* () =
    match !security_logfile with
    | Some ic -> Lwt_io.close ic
    | None -> return ()
  in
  let* ic =
    Lwt_io.(
      open_file
        ~flags:Unix.[ O_WRONLY; O_APPEND; O_CREAT ]
        ~perm:0o600 ~mode:output f)
  in
  security_logfile := Some ic;
  return ()

let security_log s =
  match !security_logfile with
  | None -> return ()
  | Some ic ->
      Lwt_io.atomic
        (fun ic ->
          let* () = Lwt_io.write ic (string_of_datetime (Datetime.now ())) in
          let* () = Lwt_io.write ic ": " in
          let* () = Lwt_io.write_line ic (s ()) in
          Lwt_io.flush ic)
        ic

let fail_http status =
  Lwt.fail
    (Ocsigen_extensions.Ocsigen_http_error (Ocsigen_cookie_map.empty, status))

let rewrite_fun = ref (fun x -> x)
let rewrite_prefix x = !rewrite_fun x

let set_rewrite_prefix ~src ~dst =
  let nsrc = String.length src in
  let f x =
    let n = String.length x in
    if n >= nsrc && String.sub x 0 nsrc = src then
      dst ^ String.sub x nsrc (n - nsrc)
    else x
  in
  rewrite_fun := f

let get_election_home_url uuid =
  Printf.sprintf "%s/elections/%s/" !Web_config.prefix (Uuid.unwrap uuid)

type election_file =
  | ESArchive of uuid
  | ESRaw
  | ESVoters
  | ESRecords
  | ESETally
  | ESResult
  | ESSalts

let election_file_of_string = function
  | "election.json" -> ESRaw
  | "records" -> ESRecords
  | "voters.txt" -> ESVoters
  | "encrypted_tally.json" -> ESETally
  | "result.json" -> ESResult
  | "salts.json" -> ESSalts
  | x -> (
      match Filename.chop_suffix_opt ~suffix:".bel" x with
      | Some uuid_s -> ESArchive (Uuid.wrap uuid_s)
      | None -> invalid_arg ("election_dir_item: " ^ x))

let string_of_election_file = function
  | ESArchive x -> Uuid.unwrap x ^ ".bel"
  | ESRaw -> "election.json"
  | ESRecords -> "records"
  | ESVoters -> "voters.txt"
  | ESETally -> "encrypted_tally.json"
  | ESResult -> "result.json"
  | ESSalts -> "salts.json"

let election_file x =
  Eliom_parameter.user_type ~of_string:election_file_of_string
    ~to_string:string_of_election_file x

let uuid x =
  Eliom_parameter.user_type ~of_string:Uuid.wrap ~to_string:Uuid.unwrap x

type site_cont_path = ContSiteHome | ContSiteAdmin | ContSiteElection of uuid
type site_cont_admin = Classic | Basic | New
type site_cont = { path : site_cont_path; admin : site_cont_admin }

let default_admin path = { path; admin = Classic }

let site_cont_of_string x =
  let fail () = invalid_arg "site_cont_of_string" in
  let path, admin =
    match String.split_on_char '@' x with
    | [ path; "basic" ] -> (path, Basic)
    | [ path; "new" ] -> (path, New)
    | [ path ] -> (path, Classic)
    | _ -> fail ()
  in
  let path =
    match String.split_on_char '/' path with
    | [ "home" ] -> ContSiteHome
    | [ "admin" ] -> ContSiteAdmin
    | [ "elections"; uuid ] -> ContSiteElection (Uuid.wrap uuid)
    | _ -> fail ()
  in
  { path; admin }

let string_of_site_cont x =
  let path =
    match x.path with
    | ContSiteHome -> "home"
    | ContSiteAdmin -> "admin"
    | ContSiteElection uuid -> Printf.sprintf "elections/%s" (Uuid.unwrap uuid)
  in
  let admin =
    match x.admin with Classic -> "" | Basic -> "@basic" | New -> "@new"
  in
  path ^ admin

let site_cont x =
  Eliom_parameter.user_type ~of_string:site_cont_of_string
    ~to_string:string_of_site_cont x

type privacy_cont = ContAdmin | ContSignup of string

let privacy_cont_of_string x =
  match String.split_on_char '/' x with
  | [ "admin" ] -> ContAdmin
  | [ "signup"; service ] -> ContSignup service
  | _ -> invalid_arg "privacy_cont_of_string"

let string_of_privacy_cont = function
  | ContAdmin -> "admin"
  | ContSignup service -> "signup/" ^ service

let privacy_cont x =
  Eliom_parameter.user_type ~of_string:privacy_cont_of_string
    ~to_string:string_of_privacy_cont x

type captcha_error = BadCaptcha | BadAddress

type add_account_error =
  | UsernameTaken
  | AddressTaken
  | BadUsername
  | BadPassword of string
  | PasswordMismatch
  | BadSpaceInPassword

include MakeGenerateToken (Random)

let format_password x =
  if String.length x = 15 then
    String.sub x 0 5 ^ "-" ^ String.sub x 5 5 ^ "-" ^ String.sub x 10 5
  else x

let string_of_user { user_domain; user_name } = user_domain ^ ":" ^ user_name

let mailer =
  match Sys.getenv_opt "BELENIOS_SENDMAIL" with
  | None -> "/usr/lib/sendmail"
  | Some x -> x

let sendmail ?return_path message =
  let mailer =
    match return_path with
    | None -> mailer
    | Some x -> Printf.sprintf "%s -f %s" mailer x
  in
  Netsendmail.sendmail ~mailer message

type mail_kind =
  | MailCredential of uuid
  | MailPassword of uuid
  | MailConfirmation of uuid
  | MailAutomaticWarning of uuid
  | MailAccountCreation
  | MailPasswordChange
  | MailLogin
  | MailSetEmail

let stringuuid_of_mail_kind = function
  | MailCredential uuid -> ("credential", Some uuid)
  | MailPassword uuid -> ("password", Some uuid)
  | MailConfirmation uuid -> ("confirmation", Some uuid)
  | MailAutomaticWarning uuid -> ("autowarning", Some uuid)
  | MailAccountCreation -> ("account-creation", None)
  | MailPasswordChange -> ("password-change", None)
  | MailLogin -> ("login", None)
  | MailSetEmail -> ("set-email", None)

let send_email kind ~recipient ~subject ~body =
  let contents =
    Netsendmail.compose
      ~from_addr:(!Web_config.server_name, !Web_config.server_mail)
      ~to_addrs:[ (recipient, recipient) ]
      ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8 ~subject body
  in
  let headers, _ = contents in
  let token = generate_token ~length:6 () in
  let date = Datetime.format ~fmt:"%Y%m%d%H%M%S" (Datetime.now ()) in
  let message_id = Printf.sprintf "<%s.%s@%s>" date token !Web_config.domain in
  headers#update_field "Message-ID" message_id;
  headers#update_field "Belenios-Domain" !Web_config.domain;
  let reason, uuid = stringuuid_of_mail_kind kind in
  headers#update_field "Belenios-Reason" reason;
  let () =
    match uuid with
    | None -> ()
    | Some uuid -> headers#update_field "Belenios-UUID" (Uuid.unwrap uuid)
  in
  let return_path = !Web_config.return_path in
  let sendmail = sendmail ?return_path in
  let rec loop retry =
    Lwt.catch
      (fun () -> Lwt_preemptive.detach sendmail contents)
      (function
        | Unix.Unix_error (Unix.EAGAIN, _, _) when retry > 0 ->
            Ocsigen_messages.warning
              "Failed to fork for sending an e-mail; will try again in 1s";
            let* () = Lwt_unix.sleep 1. in
            loop (retry - 1)
        | e ->
            let msg =
              Printf.sprintf "Failed to send an e-mail to %s: %s" recipient
                (Printexc.to_string e)
            in
            Ocsigen_messages.errlog msg;
            Lwt.return_unit)
  in
  loop 2

let get_languages xs = match xs with None -> [ "en" ] | Some xs -> xs
let string_of_languages xs = String.concat " " (get_languages xs)
let languages_of_string x = Re.Pcre.(split ~rex:(regexp "\\s+") x)
let urlize = String.map (function '+' -> '-' | '/' -> '_' | c -> c)
let unurlize = String.map (function '-' -> '+' | '_' -> '/' | c -> c)

let markup x =
  let open Eliom_content.Html.F in
  let open Belenios_ui in
  let p =
    {
      Markup.bold = (fun _ xs -> span ~a:[ a_class [ "markup-b" ] ] xs);
      text = (fun _ x -> txt x);
      br = (fun _ -> br ());
      italic = (fun _ xs -> span ~a:[ a_class [ "markup-i" ] ] xs);
    }
  in
  try
    let lexbuf = Lexing.from_string x in
    let xs = Markup_parser.full Markup_lexer.token lexbuf in
    let xs = Markup.render p xs in
    span xs
  with _ -> span ~a:[ a_class [ "markup-error" ] ] [ txt x ]

let webize_trustee_public_key pk =
  {
    web_trustee_pok = pk.trustee_pok;
    web_trustee_public_key = pk.trustee_public_key;
    web_trustee_server =
      (if pk.trustee_name = Some "server" then Some true else None);
  }

let unwebize_trustee_public_key pk =
  {
    trustee_pok = pk.web_trustee_pok;
    trustee_public_key = pk.web_trustee_public_key;
    trustee_name =
      (if pk.web_trustee_server = Some true then Some "server" else None);
  }

let get_suitable_group_kind
    (Belenios.Election.Template (V1, { t_questions; _ })) =
  let group = ref `H in
  Array.iter
    (function Belenios_question.NonHomomorphic _ -> group := `NH | _ -> ())
    t_questions;
  !group

let get_booth_index = function Some 2 -> Some 0 | _ -> None

let compute_hash_link ~service ~uuid ~token =
  Eliom_uri.make_string_uri ~absolute:true ~service ()
  |> (fun x -> Printf.sprintf "%s#%s-%s" x (Uuid.unwrap uuid) token)
  |> rewrite_prefix

type credential_record = {
  cr_ballot : string option;
  cr_weight : weight;
  cr_username : string option;
}

let check_password_with_file ~db ~name_or_email ~password =
  let name_or_email = String.trim name_or_email |> String.lowercase_ascii in
  let check_name_or_email =
    if is_email name_or_email then function
      | u :: _ :: _ :: _ when String.lowercase_ascii u = name_or_email ->
          (* When authenticating as a voter, the username may be an email *)
          true
      | _ :: _ :: _ :: e :: _ when String.lowercase_ascii e = name_or_email ->
          (* When authenticating as an admin, email is 4th CSV field *)
          true
      | _ -> false
    else function
      | u :: _ :: _ :: _ when String.lowercase_ascii u = name_or_email -> true
      | _ -> false
  in
  let* db = Lwt_preemptive.detach Csv.load db in
  match List.find_opt check_name_or_email db with
  | Some (u :: salt :: hashed :: xs) ->
      if sha256_hex (salt ^ String.trim password) = hashed then
        let email = match xs with [] -> "" | x :: _ -> x in
        return_some (u, email)
      else return_none
  | _ -> return_none

let has_explicit_weights voters =
  List.exists
    (fun v ->
      let (_, { weight; _ }) : Voter.t = v.sv_id in
      weight <> None)
    voters

type draft_election =
  | Draft :
      'a Belenios.Election.version * 'a raw_draft_election
      -> draft_election

let draft_election_of_string x =
  let abstract = raw_draft_election_of_string Yojson.Safe.read_json x in
  let open Belenios.Election in
  match version_of_int abstract.se_version with
  | Version (V1 as v) ->
      let x =
        raw_draft_election_of_string Belenios_v1.Serializable_j.read_question x
      in
      Draft (v, x)

let string_of_draft_election (Draft (V1, x)) =
  string_of_raw_draft_election Belenios_v1.Serializable_j.write_question x
