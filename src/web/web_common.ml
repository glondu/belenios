(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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
open Platform
open Common
open Serializable_builtin_t
open Serializable_t
open Web_serializable_builtin_t
open Web_serializable_j

let site_auth_config = ref []
let spool_dir = ref "."
let server_mail = ref "noreply@example.org"
let return_path = ref None
let contact_uri = ref None
let gdpr_uri = ref ""

module LwtRandom = struct

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail

  let prng = lazy (pseudo_rng (random_string secure_rng 16))

  let random q =
    let size = bytes_to_sample q in
    let%lwt rng = Lwt_preemptive.detach Lazy.force prng in
    let r = random_string rng size in
    return Z.(of_bits r mod q)

end

type error =
  | Serialization of exn
  | ProofCheck
  | ElectionClosed
  | MissingCredential
  | InvalidCredential
  | RevoteNotAllowed
  | ReusedCredential
  | WrongCredential
  | UsedCredential
  | CredentialNotFound
  | UnauthorizedVoter

exception Error of error

let fail e = Lwt.fail (Error e)

let explain_error l e =
  let module L = (val l : Web_i18n_sig.LocalizedStrings) in
  match e with
  | Serialization e -> Printf.sprintf L.error_Serialization (Printexc.to_string e)
  | ProofCheck -> L.error_ProofCheck
  | ElectionClosed -> L.error_ElectionClosed
  | MissingCredential -> L.error_MissingCredential
  | InvalidCredential -> L.error_InvalidCredential
  | RevoteNotAllowed -> L.error_RevoteNotAllowed
  | ReusedCredential -> L.error_ReusedCredential
  | WrongCredential -> L.error_WrongCredential
  | UsedCredential -> L.error_UsedCredential
  | CredentialNotFound -> L.error_CredentialNotFound
  | UnauthorizedVoter -> L.error_UnauthorizedVoter

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
    | Some ic -> Lwt_io.atomic (fun ic ->
      Lwt_io.write ic (
        string_of_datetime (now ())
      ) >>
      Lwt_io.write ic ": " >>
      Lwt_io.write_line ic (s ()) >>
      Lwt_io.flush ic
    ) ic

let fail_http status =
  [%lwt raise (
    Ocsigen_extensions.Ocsigen_http_error
      (Ocsigen_cookies.empty_cookieset, status)
  )]

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
  | ESKeys
  | ESTParams
  | ESCreds
  | ESBallots
  | ESVoters
  | ESRecords
  | ESETally
  | ESResult

let election_file_of_string = function
  | "election.json" -> ESRaw
  | "public_keys.jsons" -> ESKeys
  | "threshold.json" -> ESTParams
  | "public_creds.txt" -> ESCreds
  | "ballots.jsons" -> ESBallots
  | "records" -> ESRecords
  | "voters.txt" -> ESVoters
  | "encrypted_tally.json" -> ESETally
  | "result.json" -> ESResult
  | x -> invalid_arg ("election_dir_item: " ^ x)

let string_of_election_file = function
  | ESRaw -> "election.json"
  | ESKeys -> "public_keys.jsons"
  | ESTParams -> "threshold.json"
  | ESCreds -> "public_creds.txt"
  | ESBallots -> "ballots.jsons"
  | ESRecords -> "records"
  | ESVoters -> "voters.txt"
  | ESETally -> "encrypted_tally.json"
  | ESResult -> "result.json"

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

let b58_digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
let prng = lazy (pseudo_rng (random_string secure_rng 16))

let random_char () =
  let%lwt rng =
    if Lazy.is_val prng then return (Lazy.force prng) else
    Lwt_preemptive.detach (fun () -> Lazy.force prng) ()
  in
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

let sendmail ?return_path message =
  let mailer =
    match return_path with
    | None -> None
    | Some x -> Some (Printf.sprintf "/usr/lib/sendmail -f %s" x) in
  Netsendmail.sendmail ?mailer message

let send_email recipient subject body =
  let contents =
    Netsendmail.compose
      ~from_addr:("Belenios public server", !server_mail)
      ~to_addrs:[recipient, recipient]
      ~in_charset:`Enc_utf8 ~out_charset:`Enc_utf8
      ~subject body
  in
  let return_path = !return_path in
  let sendmail = sendmail ?return_path in
  let rec loop () =
    try%lwt
      Lwt_preemptive.detach sendmail contents
    with Unix.Unix_error (Unix.EAGAIN, _, _) ->
      Lwt_unix.sleep 1. >> loop ()
  in loop ()

let split_identity x =
  let n = String.length x in
  try
    let i = String.index x ',' in
    String.sub x 0 i, String.sub x (i+1) (n-i-1)
  with Not_found ->
    x, x

let available_languages = ["en"; "fr"; "de"; "ro"; "it"]

let get_languages xs =
  match xs with
  | None -> ["en"]
  | Some xs -> xs

let string_of_languages xs =
  String.concat " " (get_languages xs)

let languages_of_string x =
  Pcre.split x

let email_rex = "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}"

let is_email =
  let rex = Pcre.regexp ~flags:[`CASELESS] ("^" ^ email_rex ^ "$") in
  fun x ->
  try ignore (Pcre.pcre_exec ~rex x); true
  with Not_found -> false

let extract_email =
  let rex = Pcre.regexp ~flags:[`CASELESS] ("<(" ^ email_rex ^ ")>") in
  fun x ->
  if is_email x then
    Some x
  else (
    try
      let s = Pcre.exec ~rex x in
      Some (Pcre.get_substring s 1)
    with Not_found -> None
  )

let file_exists x =
  try%lwt
    Lwt_unix.(access x [R_OK]) >>
    return true
  with _ ->
    return false

let get_fname uuid x =
  match uuid with
  | None -> x
  | Some uuid ->
     let ( / ) = Filename.concat in
     !spool_dir / raw_string_of_uuid uuid / x

let read_file ?uuid x =
  try%lwt
    let%lwt lines = Lwt_io.lines_of_file (get_fname uuid x) |> Lwt_stream.to_list in
    return (Some lines)
  with _ -> return_none

let write_file ?uuid x lines =
  let fname = get_fname uuid x in
  let fname_new = fname ^ ".new" in
  Lwt_io.(
    with_file Output fname_new (fun oc ->
        Lwt_list.iter_s (write_line oc) lines
      )
  ) >> Lwt_unix.rename fname_new fname

let cleanup_file f =
  try%lwt Lwt_unix.unlink f
  with _ -> return_unit

let rmdir dir =
  let command = "rm", [| "rm"; "-rf"; dir |] in
  let%lwt _ = Lwt_process.exec command in
  return_unit

let compile_auth_config {auth_system; auth_instance; auth_config} =
  auth_instance, (auth_system, List.map snd auth_config)

let urlize = String.map (function '+' -> '-' | '/' -> '_' | c -> c)
let unurlize = String.map (function '-' -> '+' | '_' -> '/' | c -> c)

let default_contact = "Name <user@example.org>"

let default_questions =
  let question = {
      q_answers = [| "Answer 1"; "Answer 2"; "Answer 3" |];
      q_blank = None;
      q_min = 1;
      q_max = 2;
      q_question = "Question 1?";
    }
  in
  [| question |]

let default_name = "Name of the election"
let default_description = "Description of the election."

let default_creation_date = datetime_of_string "\"2018-06-06 00:00:00.000000\""
let default_validation_date = datetime_of_string "\"2015-10-01 00:00:00.000000\""
let default_tally_date = datetime_of_string "\"2018-06-06 00:00:00.000000\""
let default_archive_date = datetime_of_string "\"2018-06-06 00:00:00.000000\""

let days_to_archive = 4
let days_to_delete = 14
let days_to_mail = 7
let days_between_mails = 1
