open Lwt
open Util
open Serializable_t

type user = {
  user_name : string;
  user_type : string;
}

type acl =
  | Any
  | Restricted of (user -> bool Lwt.t)

type election_data = {
  raw : string;
  fingerprint : string;
  election : ff_pubkey election;
  public_keys : Z.t trustee_public_key array;
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

let hashB x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
)

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
        lwt public_keys =
          data "public_keys.jsons" |>
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
        let fingerprint = hashB raw in
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
          election_result;
          author = { user_name = "admin"; user_type = "dummy" };
          featured_p = true;
          can_read = Any;
          can_vote = Any;
          can_admin = Any;
        } in
        Lwt.return (Some (election_data, ballots))
      | None -> assert false
    ) else Lwt.return None
  )
