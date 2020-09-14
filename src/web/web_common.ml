(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2020 Inria                                           *)
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
open Belenios_platform
open Belenios
open Platform
open Common
open Serializable_builtin_t
open Serializable_t
open Web_serializable_builtin_t
open Web_serializable_j

module LwtRandom = struct

  type 'a t = 'a Lwt.t
  let yield = Lwt_main.yield
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail

  let init_prng () = lazy (pseudo_rng (random_string secure_rng 16))

  let prng = ref (init_prng ())

  let () =
    let rec loop () =
      let%lwt () = Lwt_unix.sleep 1800. in
      prng := init_prng ();
      loop ()
    in
    Lwt.async loop

  let random q =
    let size = bytes_to_sample q in
    let rng = Lazy.force !prng in
    let r = random_string rng size in
    return Z.(of_bits r mod q)

end

type cast_error =
  | ECastSerialization of exn
  | ECastMissingCredential
  | ECastInvalidCredential
  | ECastProofCheck
  | ECastWrongCredential
  | ECastRevoteNotAllowed
  | ECastReusedCredential

type error =
  | ElectionClosed
  | UnauthorizedVoter
  | CastError of cast_error

exception BeleniosWebError of error

let fail e = Lwt.fail (BeleniosWebError e)

let explain_error l e =
  let open (val l : Web_i18n_sig.GETTEXT) in
  match e with
  | ElectionClosed -> s_ "the election is closed"
  | UnauthorizedVoter -> s_ "you are not allowed to vote"
  | CastError (ECastSerialization e) -> Printf.sprintf (f_ "your ballot has a syntax error (%s)") (Printexc.to_string e)
  | CastError ECastProofCheck -> s_ "some proofs failed verification"
  | CastError ECastMissingCredential -> s_ "a credential is missing"
  | CastError ECastInvalidCredential -> s_ "your credential is invalid"
  | CastError ECastRevoteNotAllowed -> s_ "you are not allowed to revote"
  | CastError ECastReusedCredential -> s_ "your credential has already been used"
  | CastError ECastWrongCredential -> s_ "you are not allowed to vote with this credential"

let decompose_seconds s =
  let h = int_of_float (s /. 3600.) in
  let s = s -. float_of_int h *. 3600. in
  let m = int_of_float (s /. 60.) in
  let s = s -. float_of_int m *. 60. in
  (h, m, int_of_float s)

let format_period l x =
  let open (val l : Web_i18n_sig.GETTEXT) in
  let y, m, d, s = ymds x in
  let y = if y = 0 then "" else string_of_int y ^ (s_ " year(s)") in
  let m = if m = 0 then "" else string_of_int m ^ (s_ " month(s)") in
  let d = if d = 0 then "" else string_of_int d ^ (s_ " day(s)") in
  let hrs, min, sec = decompose_seconds s in
  let hrs = if hrs = 0 then "" else string_of_int hrs ^ (s_ " hour(s)") in
  let min = if min = 0 then "" else string_of_int min ^ (s_ " minute(s)") in
  let sec = if sec = 0 then "" else string_of_int sec ^ (s_ " second(s)") in
  let approx = String.concat " " (List.filter (fun x -> x <> "") [y; m; d; hrs; min]) in
  if approx = "" then sec else approx

let security_logfile = ref None

let open_security_log f =
  let%lwt () =
    match !security_logfile with
      | Some ic -> Lwt_io.close ic
      | None -> return ()
  in
  let%lwt ic = Lwt_io.(
    open_file ~flags:Unix.(
      [O_WRONLY; O_APPEND; O_CREAT]
    ) ~perm:0o600 ~mode:output f
  ) in
  security_logfile := Some ic;
  return ()

let security_log s =
  match !security_logfile with
    | None -> return ()
    | Some ic ->
       Lwt_io.atomic (fun ic ->
           let%lwt () = Lwt_io.write ic (string_of_datetime (now ())) in
           let%lwt () = Lwt_io.write ic ": " in
           let%lwt () = Lwt_io.write_line ic (s ()) in
           Lwt_io.flush ic
    ) ic

let fail_http status =
  Lwt.fail (
      Ocsigen_extensions.Ocsigen_http_error
        (Ocsigen_cookies.empty_cookieset, status)
    )

let forbidden () = fail_http 403

let rewrite_fun = ref (fun x -> x)

let rewrite_prefix x = !rewrite_fun x

let set_rewrite_prefix ~src ~dst =
  let nsrc = String.length src in
  let f x =
    let n = String.length x in
    if n >= nsrc && String.sub x 0 nsrc = src then
      dst ^ String.sub x nsrc (n-nsrc)
    else x
  in rewrite_fun := f

type election_file =
  | ESRaw
  | ESTrustees
  | ESCreds
  | ESBallots
  | ESVoters
  | ESRecords
  | ESETally
  | ESResult
  | ESShuffles

let election_file_of_string = function
  | "election.json" -> ESRaw
  | "trustees.json" -> ESTrustees
  | "public_creds.txt" -> ESCreds
  | "ballots.jsons" -> ESBallots
  | "records" -> ESRecords
  | "voters.txt" -> ESVoters
  | "encrypted_tally.json" -> ESETally
  | "result.json" -> ESResult
  | "shuffles.jsons" -> ESShuffles
  | x -> invalid_arg ("election_dir_item: " ^ x)

let string_of_election_file = function
  | ESRaw -> "election.json"
  | ESTrustees -> "trustees.json"
  | ESCreds -> "public_creds.txt"
  | ESBallots -> "ballots.jsons"
  | ESRecords -> "records"
  | ESVoters -> "voters.txt"
  | ESETally -> "encrypted_tally.json"
  | ESResult -> "result.json"
  | ESShuffles -> "shuffles.jsons"

let election_file x =
  Eliom_parameter.user_type
    ~of_string:election_file_of_string
    ~to_string:string_of_election_file
    x

let uuid x =
  Eliom_parameter.user_type
    ~of_string:uuid_of_raw_string
    ~to_string:raw_string_of_uuid
    x

type site_cont =
  | ContSiteHome
  | ContSiteAdmin
  | ContSiteElection of uuid

let site_cont_of_string x =
  match Pcre.split ~pat:"/" x with
  | ["home"] -> ContSiteHome
  | ["admin"] -> ContSiteAdmin
  | ["elections"; uuid] -> ContSiteElection (uuid_of_raw_string uuid)
  | _ -> invalid_arg "site_login_cont_of_string"

let string_of_site_cont = function
  | ContSiteHome -> "home"
  | ContSiteAdmin -> "admin"
  | ContSiteElection uuid -> Printf.sprintf "elections/%s" (raw_string_of_uuid uuid)

let site_cont x =
  Eliom_parameter.user_type
    ~of_string:site_cont_of_string
    ~to_string:string_of_site_cont
    x

type privacy_cont =
  | ContAdmin
  | ContSignup of string

let privacy_cont_of_string x =
  match Pcre.split ~pat:"/" x with
  | ["admin"] -> ContAdmin
  | ["signup"; service] -> ContSignup service
  | _ -> invalid_arg "privacy_cont_of_string"

let string_of_privacy_cont = function
  | ContAdmin -> "admin"
  | ContSignup service -> "signup/" ^ service

let privacy_cont x =
  Eliom_parameter.user_type
    ~of_string:privacy_cont_of_string
    ~to_string:string_of_privacy_cont
    x

type captcha_error =
  | BadCaptcha
  | BadAddress

type add_account_error =
  | UsernameTaken
  | AddressTaken
  | BadUsername
  | BadPassword of string
  | PasswordMismatch
  | BadSpaceInPassword

let b58_digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
let prng = lazy (pseudo_rng (random_string secure_rng 16))

let random_char () =
  let rng = Lazy.force prng in
  return (int_of_char (random_string rng 1).[0])

let generate_token ?(length=14) () =
  let res = Bytes.create length in
  let rec loop i =
    if i < length then (
      let%lwt digit = random_char () in
      let digit = digit mod 58 in
      Bytes.set res i b58_digits.[digit];
      loop (i+1)
    ) else return (Bytes.to_string res)
  in loop 0

let string_of_user {user_domain; user_name} =
  user_domain ^ ":" ^ user_name

let mailer =
  match Sys.getenv_opt "BELENIOS_SENDMAIL" with
  | None -> "/usr/lib/sendmail"
  | Some x -> x

let sendmail ?return_path message =
  let mailer =
    match return_path with
    | None -> mailer
    | Some x -> Printf.sprintf "%s -f %s" mailer x in
  Netsendmail.sendmail ~mailer message

let send_email recipient subject body =
  let contents =
    Netsendmail.compose
      ~from_addr:("Belenios public server", !Web_config.server_mail)
      ~to_addrs:[recipient, recipient]
      ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8
      ~subject body
  in
  let return_path = !Web_config.return_path in
  let sendmail = sendmail ?return_path in
  let rec loop () =
    try%lwt Lwt_preemptive.detach sendmail contents with
    | Unix.Unix_error (Unix.EAGAIN, _, _) ->
       let%lwt () = Lwt_unix.sleep 1. in
       loop ()
  in loop ()

let split_identity x =
  let n = String.length x in
  match String.index_opt x ',' with
  | Some i -> String.sub x 0 i, String.sub x (i+1) (n-i-1)
  | None -> x, x

let available_languages = ["en"; "fr"; "de"; "ro"; "it"; "nb"]

let get_languages xs =
  match xs with
  | None -> ["en"]
  | Some xs -> xs

let string_of_languages xs =
  String.concat " " (get_languages xs)

let languages_of_string x =
  Pcre.split x

let pcre_exec_opt ~rex x =
  try Some (Pcre.exec ~rex x) with Not_found -> None

let email_rex = "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}"

let is_email =
  let rex = Pcre.regexp ~flags:[`CASELESS] ("^" ^ email_rex ^ "$") in
  fun x ->
  match pcre_exec_opt ~rex x with
  | Some _ -> true
  | None -> false

let extract_email =
  let rex = Pcre.regexp ~flags:[`CASELESS] ("<(" ^ email_rex ^ ")>") in
  fun x ->
  if is_email x then
    Some x
  else (
    match pcre_exec_opt ~rex x with
    | Some s -> Some (Pcre.get_substring s 1)
    | None -> None
  )

let file_exists x =
  match%lwt Lwt_unix.(access x [R_OK]) with
  | () -> return true
  | exception _ -> return false

let get_fname uuid x =
  match uuid with
  | None -> x
  | Some uuid ->
     let ( / ) = Filename.concat in
     !Web_config.spool_dir / raw_string_of_uuid uuid / x

let read_file ?uuid x =
  match%lwt Lwt_io.lines_of_file (get_fname uuid x) |> Lwt_stream.to_list with
  | lines -> return_some lines
  | exception _ -> return_none

let write_file ?uuid x lines =
  let fname = get_fname uuid x in
  let fname_new = fname ^ ".new" in
  let%lwt () =
    Lwt_io.(
      with_file ~mode:Output fname_new (fun oc ->
          Lwt_list.iter_s (write_line oc) lines
        )
    )
  in
  Lwt_unix.rename fname_new fname

let cleanup_file f =
  try%lwt Lwt_unix.unlink f with
  | _ -> return_unit

let rmdir dir =
  let command = "rm", [| "rm"; "-rf"; dir |] in
  let%lwt _ = Lwt_process.exec command in
  return_unit

let urlize = String.map (function '+' -> '-' | '/' -> '_' | c -> c)
let unurlize = String.map (function '-' -> '+' | '_' -> '/' | c -> c)

let webize_trustee_public_key pk =
  {
    web_trustee_pok = pk.trustee_pok;
    web_trustee_public_key = pk.trustee_public_key;
    web_trustee_server = if pk.trustee_name = Some "server" then Some true else None;
  }

let unwebize_trustee_public_key pk =
  {
    trustee_pok = pk.web_trustee_pok;
    trustee_public_key = pk.web_trustee_public_key;
    trustee_name = if pk.web_trustee_server = Some true then Some "server" else None;
  }

let get_suitable_group_kind {t_questions; _} =
  let group = ref `H in
  Array.iter (function
      | Question.NonHomomorphic _ -> group := `NH
      | Question.Homomorphic _ -> ()
    ) t_questions;
  !group

let is_group_fixed se =
  se.se_public_creds_received
  || se.se_public_keys <> []
  || (match se.se_threshold_trustees with
      | Some l -> l <> []
      | None -> false
     )

let default_contact = "Name <user@example.org>"

let default_questions =
  let open Question_h_t in
  let question = {
      q_answers = [| "Answer 1"; "Answer 2"; "Answer 3" |];
      q_blank = None;
      q_min = 1;
      q_max = 2;
      q_question = "Question 1?";
    }
  in
  [| Question.Homomorphic question |]

let default_name = "Name of the election"
let default_description = "Description of the election."

let default_creation_date = datetime_of_string "\"2018-11-26 00:00:00.000000\""
let default_validation_date = datetime_of_string "\"2015-10-01 00:00:00.000000\""
let default_tally_date = datetime_of_string "\"2018-11-26 00:00:00.000000\""
let default_archive_date = datetime_of_string "\"2018-11-26 00:00:00.000000\""

let days_to_archive = 7
let days_to_delete = 365
let days_to_mail = 30
let days_between_mails = 7
let days_to_publish_result = 7

let max_election_name_size = 80
