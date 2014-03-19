(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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

open Signatures
open Lwt
open Util
open Serializable_builtin_t
open Serializable_t
open Web_serializable_t

let enforce_single_element s =
  let open Lwt_stream in
  lwt t = next s in
  lwt b = is_empty s in
  (assert_lwt b) >>
  Lwt.return t

let load_from_file read fname =
  let i = open_in fname in
  let buf = Lexing.from_channel i in
  let lex = Yojson.init_lexer ~fname () in
  let result = read lex buf in
  close_in i;
  result

let make_rng = Lwt_preemptive.detach (fun () ->
  Cryptokit.Random.(pseudo_rng (string secure_rng 16))
)

module type LWT_RANDOM = Signatures.RANDOM with type 'a t = 'a Lwt.t

module type LWT_RNG = sig
  val rng : Cryptokit.Random.rng Lwt.t
end

module MakeLwtRandom (X : LWT_RNG) = struct

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail

  let random q =
    let size = Z.size q * Sys.word_size / 8 in
    lwt rng = X.rng in
    let r = Cryptokit.Random.string rng size in
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

exception Error of error

let fail e = Lwt.fail (Error e)

let explain_error = function
  | Serialization e ->
    Printf.sprintf "your ballot has a syntax error (%s)" (Printexc.to_string e)
  | ProofCheck -> "some proofs failed verification"
  | ElectionClosed -> "the election is closed"
  | MissingCredential -> "a credential is missing"
  | InvalidCredential -> "your credential is invalid"
  | RevoteNotAllowed -> "you are not allowed to revote"
  | ReusedCredential -> "your credential has already been used"
  | WrongCredential -> "you are not allowed to vote with this credential"
  | UsedCredential -> "the credential has already been used"
  | CredentialNotFound -> "the credential has not been found"

let security_logfile = ref None

let open_security_log f =
  lwt () =
    match !security_logfile with
      | Some ic -> Lwt_io.close ic
      | None -> return ()
  in
  lwt ic = Lwt_io.(
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
        Serializable_builtin_j.string_of_datetime (
          CalendarLib.Fcalendar.Precise.now (),
          None
        )
      ) >>
      Lwt_io.write ic ": " >>
      Lwt_io.write_line ic (s ()) >>
      Lwt_io.flush ic
    ) ic

let fail_http status =
  raise_lwt (
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

let empty_metadata = {
  e_voting_starts_at = None;
  e_voting_ends_at = None;
  e_readers = None;
  e_voters = None;
  e_owner = None;
}

let check_acl a u =
  match a with
  | Some acls ->
    let rec loop = function
      | [] -> false
      | `Any :: _ -> true
      | `Domain x :: _ when x = u.user_domain -> true
      | `User x :: _ when x = u -> true
      | _ :: xs -> loop xs
    in loop acls
  | _ -> false

let uuid = Eliom_parameter.user_type
  ~of_string:(fun x -> match Uuidm.of_string x with
    | Some x -> x
    | None -> invalid_arg "uuid")
  ~to_string:Uuidm.to_string

type election_file =
  | ESIndex
  | ESRaw
  | ESKeys
  | ESCreds
  | ESBallots
  | ESRecords

let election_file_of_string = function
  | "" -> ESIndex
  | "election.json" -> ESRaw
  | "public_keys.jsons" -> ESKeys
  | "public_creds.txt" -> ESCreds
  | "ballots.jsons" -> ESBallots
  | "records" -> ESRecords
  | x -> invalid_arg ("election_dir_item: " ^ x)

let string_of_election_file = function
  | ESIndex -> ""
  | ESRaw -> "election.json"
  | ESKeys -> "public_keys.jsons"
  | ESCreds -> "public_creds.txt"
  | ESBallots -> "ballots.jsons"
  | ESRecords -> "records"

let election_file = Eliom_parameter.user_type
  ~of_string:election_file_of_string
  ~to_string:string_of_election_file
