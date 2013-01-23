open StdExtra
open Helios_datatypes_t

type election_data = {
  raw : string;
  fingerprint : string;
  election : Z.t election;
  public_data : Z.t election_public_data;
}

let enforce_single_element s =
  let open Lwt_stream in
  lwt t = next s in
  lwt b = is_empty s in
  (assert_lwt b) >>
  Lwt.return t

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
        let election = Helios_datatypes_j.election_of_string
          Core_datatypes_j.read_number raw
        in
        (assert_lwt (Uuidm.equal uuid election.e_uuid)) >>
        let public_data =
          data "public.json" |>
          load_from_file (Helios_datatypes_j.read_election_public_data Core_datatypes_j.read_number)
        in
        let fingerprint = hashB raw in
        let ballots =
          let file = data "ballots.json" in
          if Sys.file_exists file then (
            Lwt_io.lines_of_file file |>
            Lwt_stream.map (fun x ->
              let v = Helios_datatypes_j.ballot_of_string Core_datatypes_j.read_number x in
              assert (Uuidm.equal uuid v.election_uuid);
              v
            )
          ) else Lwt_stream.from_direct (fun () -> None)
        in
        let voters =
          let file = data "voters.json" in
          if Sys.file_exists file then (
            Lwt_io.lines_of_file file |>
            Lwt_stream.map (fun x ->
              let v = Helios_datatypes_j.voter_of_string x in
              v
            )
          ) else Lwt_stream.from_direct (fun () -> None)
        in
        let election_data = { raw; fingerprint; election; public_data } in
        Lwt.return (Some (election_data, ballots, voters))
      | None -> assert false
    ) else Lwt.return None
  )

let concat s l f = String.concat s (List.map f (Array.to_list l))

let hash_ballot v =
  concat "//" v.answers (fun a ->
    concat "|" a.choices (fun c ->
      Printf.sprintf "%s,%s" (Z.to_string c.alpha) (Z.to_string c.beta)
    ) ^
    "#" ^
    concat "|" a.individual_proofs (fun p ->
      concat "/" p (fun pi ->
        Printf.sprintf "%a,%a,%a,%a"
          Z.sprint pi.dp_commitment.a
          Z.sprint pi.dp_commitment.b
          Z.sprint pi.dp_challenge
          Z.sprint pi.dp_response
      )
    ) ^
    "#" ^
    concat "/" a.overall_proof (fun pi ->
      Printf.sprintf "%a,%a,%a,%a"
        Z.sprint pi.dp_commitment.a
        Z.sprint pi.dp_commitment.b
        Z.sprint pi.dp_challenge
        Z.sprint pi.dp_response
    )
  ) ^
  "#" ^ v.election_hash ^
  "#" ^ (Uuidm.to_string v.election_uuid) |>
  hashB

let hash_user v =
  Helios_datatypes_j.string_of_user v |>
  hashB
