open Lwt
open Util
open Serializable_compat_t

let hashB x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
)

let if_legacy s =
  if String.length s = 38 && s.[0] = '{' && s.[37] = '}'
  then Uuidm.of_string ~pos:1 s
  else None

type 'a legacy_election = {
  uuid : uuid;
  raw_election : string;
  election : 'a election;
  trustee_public_keys : 'a trustee_public_key list;
  private_keys : 'a private_key list option;
  ballots : 'a ballot list option;
  encrypted_tally : 'a encrypted_tally option;
  partial_decryptions : 'a partial_decryption list option;
  result : raw_result option;
}

let ( / ) = Filename.concat

let atom t_of_string file =
  try_lwt
    lwt raw = Lwt_io.chars_of_file file |> Lwt_stream.to_string in
    return (Some (t_of_string raw))
  with Unix.Unix_error(Unix.ENOENT, "open", _) -> Lwt.return None

let list t_of_string file =
  try_lwt
    Lwt_io.lines_of_file file |>
    Lwt_stream.map t_of_string |>
    Lwt_stream.to_list >>=
    (fun x -> return (Some x))
  with Unix.Unix_error(Unix.ENOENT, "open", _) -> return None

let legacy_election dir uuid =
  let read = Serializable_builtin_j.read_number in
  lwt raw_election =
    Lwt_io.chars_of_file (dir/"election.json") |> Lwt_stream.to_string
  in
  let election = Serializable_compat_j.election_of_string read raw_election in
  lwt trustee_public_keys =
    match_lwt list (Serializable_compat_j.trustee_public_key_of_string read) (dir/"public_keys.jsons") with
      | Some xs -> return xs
      | None -> fail (Failure "cannot read public keys")
  in
  lwt private_keys = list (Serializable_compat_j.private_key_of_string read) (dir/"private_keys.jsons") in
  lwt ballots = list (Serializable_compat_j.ballot_of_string read) (dir/"ballots.jsons") in
  lwt encrypted_tally = atom (Serializable_compat_j.encrypted_tally_of_string read) (dir/"encrypted_tally.json") in
  lwt partial_decryptions = list (Serializable_compat_j.partial_decryption_of_string read) (dir/"partial_decryptions.jsons") in
  lwt result = atom Serializable_compat_j.raw_result_of_string (dir/"result.json") in
  return {
    uuid;
    raw_election;
    election;
    trustee_public_keys;
    private_keys;
    ballots;
    encrypted_tally;
    partial_decryptions;
    result;
  }

let check_legacy e =
  let read = Serializable_builtin_j.read_number in
  e.election = Serializable_compat_j.election_of_string read e.raw_election &&
  e.uuid = e.election.e_uuid &&
  let {g; p; q; _} = e.election.e_public_key in
  let module P = struct
    module G =
      (val Election.finite_field ~g ~p ~q : Signatures.GROUP with type t = Z.t)
    let params = Serializable_compat.election e.election
    let fingerprint = hashB e.raw_election
    let public_keys =
      e.trustee_public_keys |>
      List.map (fun x -> x.trustee_public_key.y) |>
      Array.of_list
  end in
  let module Compat = Serializable_compat.MakeCompat(P) in
  List.for_all (fun pk ->
    let {g = g'; p = p'; q = q'; _} = pk.trustee_public_key in
    g = g' && p = p' && q = q'
  ) e.trustee_public_keys &&
  (match e.private_keys with
    | Some sks ->
      List.for_all2 (fun pk sk ->
        pk.trustee_public_key = sk.public_key
      ) e.trustee_public_keys sks
    | None -> true
  ) &&
  (match e.ballots with
    | Some bs ->
      List.for_all (fun b ->
        let b' = Serializable_compat.ballot b in
        b = Compat.ballot b'
      ) bs
    | None -> true
  ) &&
  (match e.encrypted_tally, e.partial_decryptions with
    | Some et, Some pds ->
      List.for_all (fun pd ->
        let pd' = Serializable_compat.partial_decryption pd in
        pd = Compat.partial_decryption et.tally pd'
      ) pds
    | None, Some pds ->
      failwith "partial_decryptions with no encrypted_tally"
    | _, None -> true
  )

let verbose_assert msg it =
  Printf.eprintf "   %s...%!" msg;
  let r = Lazy.force it in
  Printf.eprintf " %s\n%!" (if r then "OK" else "failed!")

let verbose_verify_election_test_data e =
  Printf.eprintf "Verifying election %S:\n%!" e.election.e_short_name;
  let {g; p; q; y} = e.election.e_public_key in
  verbose_assert "group parameters" (lazy (
    Election.check_finite_field ~p ~q ~g
  ));
  verbose_assert "redundant information in legacy datastructrure" (lazy (
    check_legacy e
  ));
  let module P = struct
    module G = (val Election.finite_field ~p ~q ~g : Signatures.GROUP with type t = Z.t)
    let public_keys =
      List.map (fun x ->
        x.trustee_public_key.y
      ) e.trustee_public_keys |> Array.of_list
    let params = Serializable_compat.election e.election
    let fingerprint = hashB e.raw_election
  end in
  verbose_assert "election key" (lazy (
    Election.check_election (module P : Signatures.ELECTION_PARAMS)
  ));
  let module M = Election.MakeSimpleMonad(P.G) in
  let module E = Election.MakeElection(P)(M) in
  let encrypted_tally = lazy (
    M.fold (fun b tally ->
      M.return (E.combine_ciphertexts tally (E.extract_ciphertext b))
    ) E.neutral_ciphertext ()
  ) in
  (match e.ballots with
    | None ->
      Printf.eprintf "   no ballots available\n%!"
    | Some ballots ->
      verbose_assert "ballots" (lazy (
        List.for_all (fun x ->
          let b = Serializable_compat.ballot x in
          if E.check_ballot b then (
            M.cast b (); true
          ) else false
        ) ballots
      ));
      (match e.encrypted_tally with
        | Some et ->
          verbose_assert "encrypted tally" (lazy (
            et.tally = Lazy.force encrypted_tally
          ))
        | None -> ()
      );
  );
  (match e.encrypted_tally, e.result, e.partial_decryptions with
    | Some et, Some r, Some pds ->
      let pds = Array.of_list pds in
      verbose_assert "partial decryptions and result" (lazy (
        let result = E.combine_factors
          et.num_tallied
          (Lazy.force encrypted_tally)
          (Array.map Serializable_compat.partial_decryption pds)
        in
        E.check_result result &&
        E.extract_tally result = r
      ));
    | None, None, None -> Printf.eprintf "   no results available\n%!"
    | _ -> failwith "partial results, cannot check"
  );
  (match e.private_keys with
    | Some sks ->
      verbose_assert "private keys" (lazy (
        let open P.G in
        List.for_all (fun k ->
          let {g=g'; p=p'; q=q'; y} = k.public_key in
          g =~ g' && p =% p' && q =% q' && g **~ k.x =~ y
        ) sks
      ))
    | None -> Printf.eprintf "    no private keys available\n%!"
  );;

let iter_keep f xs = List.iter f xs; xs;;

let load_election_and_verify_it_all dirname =
  Lwt_unix.files_of_directory dirname |>
  Lwt_stream.filter_map_s (fun d ->
    match if_legacy d with
      | Some uuid -> legacy_election (dirname/d) uuid >>= (fun x -> return (Some x))
      | None -> return None
  ) |>
  Lwt_stream.to_list >>=
  wrap2 iter_keep verbose_verify_election_test_data;;

lwt all_data = load_election_and_verify_it_all "tests/data/legacy";;

let rec get_election name = function
  | [] -> raise Not_found
  | x::xs when x.election.e_short_name = name -> x
  | _::xs -> get_election name xs

let e = get_election "editor" all_data;;
let {g; p; q; y} = e.election.e_public_key

let random_exponent =
  let pseudo = lazy Cryptokit.Random.(pseudo_rng (string secure_rng 20)) in
  (* 20 is 160 bits of entropy, taken from secure source *)
  fun () ->
    let raw = Cryptokit.Random.(string (Lazy.force pseudo) 32) in
    (* 32 is 256 bits of entropy, taken from pseudo-random source *)
    let hex = Cryptokit.(transform_string (Hexa.encode ()) raw) in
    Z.(of_string_base 16 hex mod q)

module P = struct
  module G = (val Election.finite_field ~p ~q ~g : Signatures.GROUP with type t = Z.t)
  let public_keys =
    List.map (fun x ->
      x.trustee_public_key.y
    ) e.trustee_public_keys |> Array.of_list
  let params = Serializable_compat.election e.election
  let fingerprint = hashB e.raw_election
end

module M = Election.MakeSimpleMonad(P.G)
module E = Election.MakeElection(P)(M)
module Compat = Serializable_compat.MakeCompat(P)

let ballots = match e.ballots with Some ballots -> ballots | None -> assert false;;
let nballots = List.map Serializable_compat.ballot ballots;;
assert (List.for_all E.check_ballot nballots);;
assert (List.for_all2 (fun b b' -> b = Compat.ballot b') ballots nballots);;

let create_ballot b = E.(create_ballot (make_randomness () ()) b)

let test_ballot = create_ballot [| [| 1; 0; 0; 0 |] |] ();;
assert (E.check_ballot test_ballot);;

let result, encrypted_tally, partial_decryptions =
  match e.result, e.encrypted_tally, e.partial_decryptions with
    | Some a, Some b, Some c -> a, b, c
    | _ -> assert false

let tally = encrypted_tally.tally;;
let fs = List.map Serializable_compat.partial_decryption partial_decryptions;;
assert (List.for_all2 (fun f f' -> f = Compat.partial_decryption tally f') partial_decryptions fs);;
let ys = List.map (fun x -> x.trustee_public_key.y) e.trustee_public_keys;;
assert (List.for_all2 (E.check_factor tally) ys fs);;

let y = List.hd ys;;
let x = Z.of_string "45298523167338358817538343074024028933886309805828157085973885299032584889325";;
assert P.G.(g **~ x =% y);;

let test_factor = E.compute_factor tally x ();;
assert (E.check_factor tally y test_factor);;
assert (Serializable_t.(test_factor.decryption_factors) = (List.hd partial_decryptions).decryption_factors);;

let () =
  let partial_decryptions = List.map Serializable_compat.partial_decryption partial_decryptions in
  let open Serializable_t in
  let nresult' = E.combine_factors
    encrypted_tally.num_tallied encrypted_tally.tally (Array.of_list partial_decryptions)
  in
  assert (nresult'.result = result);
  assert (E.check_result nresult');
;;
