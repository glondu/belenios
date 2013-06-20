open Lwt
open Util
open Serializable_t

type user_type = Dummy | CAS

let string_of_user_type = function
  | Dummy -> "dummy"
  | CAS -> "cas"

type user = {
  user_name : string;
  user_type : user_type;
}

type acl =
  | Any
  | Restricted of (user -> bool Lwt.t)

type election_data = {
  raw : string;
  fingerprint : string;
  election : ff_pubkey election;
  public_keys : Z.t trustee_public_key array;
  public_keys_file : string;
  election_result : Z.t result option;
  author : user;
  featured_p : bool;
  can_read : acl;
  can_vote : acl;
  can_admin : acl;
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

let load_elections_and_votes dirname =
  Lwt_unix.files_of_directory dirname |>
  Lwt_stream.filter_map_s (fun x ->
    let n = String.length x in
    if n = 38 && x.[0] = '{' && x.[n-1] = '}' then (
      match Uuidm.of_string ~pos:1 x with
      | Some uuid ->
        let dirname = Filename.concat dirname x in
        let data x = Filename.concat dirname x in
        lwt raw =
          data "election.json" |>
          Lwt_io.lines_of_file |>
          enforce_single_element
        in
        let election = Serializable_j.election_of_string
          Serializable_j.read_ff_pubkey raw
        in
        (assert_lwt (Uuidm.equal uuid election.e_uuid)) >>
        let public_keys_file = data "public_keys.jsons" in
        lwt public_keys =
          public_keys_file |>
          Lwt_io.lines_of_file |>
          Lwt_stream.map (fun x ->
            Serializable_j.trustee_public_key_of_string Serializable_builtin_j.read_number x
          ) |>
          Lwt_stream.to_list >>= wrap1 Array.of_list
        in
        let election_result =
          try Some (
            data "result.json" |>
            load_from_file (Serializable_j.read_result Serializable_builtin_j.read_number)
          ) with Sys_error _ -> None
        in
        let fingerprint = sha256_b64 raw in
        let ballots =
          let file = data "ballots.json" in
          if Sys.file_exists file then (
            Lwt_io.lines_of_file file |>
            Lwt_stream.map (fun x ->
              let v = Serializable_j.ballot_of_string Serializable_builtin_j.read_number x in
              assert (Uuidm.equal uuid v.election_uuid);
              x, v
            )
          ) else Lwt_stream.from_direct (fun () -> None)
        in
        let election_data = {
          raw;
          fingerprint;
          election;
          public_keys;
          public_keys_file;
          election_result;
          author = { user_name = "admin"; user_type = Dummy };
          featured_p = true;
          can_read = Any;
          can_vote = Any;
          can_admin = Any;
        } in
        Lwt.return (Some (election_data, ballots))
      | None -> assert false
    ) else Lwt.return None
  )

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

module type LWT_ELECTION = Signatures.ELECTION
  with type elt = Z.t
  and type 'a m = 'a Lwt.t

module MakeBallotBox (E : LWT_ELECTION) = struct

  let suffix = "_" ^ String.map (function
    | '-' -> '_'
    | c -> c
  ) (Uuidm.to_string E.election_params.e_uuid)

  let ballot_table = Ocsipersist.open_table ("ballots" ^ suffix)
  let record_table = Ocsipersist.open_table ("records" ^ suffix)

  type ballot = string
  type record = string * Serializable_builtin_t.datetime

  let cast rawballot (user, date) =
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
