open Lwt
open Util
open Serializable_builtin_t
open Serializable_t

type user_type = Dummy | CAS

type user = {
  user_name : string;
  user_type : user_type;
}

let string_of_user {user_name; user_type} =
  match user_type with
    | Dummy -> Printf.sprintf "dummy:%s" user_name
    | CAS -> user_name

(* FIXME: use a dedicated user_type *)
let is_admin = function
  | Some { user_name = "admin"; user_type = Dummy } -> true
  | _ -> false

type acl =
  | Any
  | Restricted of (user -> bool Lwt.t)

type election_data = {
  fn_election : string;
  fingerprint : string;
  election : ff_pubkey election;
  fn_public_keys : string;
  featured_p : bool;
  can_read : acl;
  can_vote : acl;
}

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

module MakeLwtRandom (G : Signatures.GROUP) = struct

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail

  let prng = Lwt_preemptive.detach (fun () ->
    Cryptokit.Random.(pseudo_rng (string secure_rng 16))
  ) ()

  let random q =
    let size = Z.size q * Sys.word_size / 8 in
    lwt prng = prng in
    let r = Cryptokit.Random.string prng size in
    return Z.(of_bits r mod q)

end

exception Serialization of exn
exception ProofCheck
exception ElectionClosed

module type LWT_ELECTION = Signatures.ELECTION
  with type elt = Z.t
  and type 'a m = 'a Lwt.t

module type WEB_BBOX = sig
  include Signatures.BALLOT_BOX
  with type 'a m := 'a Lwt.t
  and type ballot = string
  and type record = string * datetime
end

module MakeBallotBox (P : Signatures.ELECTION_PARAMS) (E : LWT_ELECTION) = struct

  (* TODO: enforce E is derived from P *)

  let suffix = "_" ^ String.map (function
    | '-' -> '_'
    | c -> c
  ) (Uuidm.to_string E.election_params.e_uuid)

  let ballot_table = Ocsipersist.open_table ("ballots" ^ suffix)
  let record_table = Ocsipersist.open_table ("records" ^ suffix)

  type ballot = string
  type record = string * Serializable_builtin_t.datetime

  let cast rawballot (user, date) =
    let voting_open = match P.metadata with
      | Some m ->
        let date = fst date in
        let open CalendarLib.Fcalendar.Precise in
        compare (fst m.e_voting_starts_at) date <= 0 &&
        compare date (fst m.e_voting_ends_at) < 0
      | None -> true
    in
    if not voting_open then fail ElectionClosed else return () >>
    lwt ballot =
      try Lwt.return (
        Serializable_j.ballot_of_string
          Serializable_builtin_j.read_number rawballot
      ) with e -> Lwt.fail (Serialization e)
    in
    if E.check_ballot ballot then (
      Ocsipersist.add ballot_table (sha256_b64 rawballot) rawballot >>
      Ocsipersist.add record_table user date
    ) else (
      Lwt.fail ProofCheck
    )


  let fold_ballots f x =
    Ocsipersist.fold_step (fun k v x -> f v x) ballot_table x

  let fold_records f x =
    Ocsipersist.fold_step (fun k v x -> f (k, v) x) record_table x

  let turnout = Ocsipersist.length ballot_table
end

module type WEB_ELECTION = sig
  module G : Signatures.GROUP
  module P : Signatures.ELECTION_PARAMS
  module E : LWT_ELECTION
  module B : WEB_BBOX
  val data : election_data
end
