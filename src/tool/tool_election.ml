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

open Printf
open Platform
open Serializable_builtin_j
open Serializable_j
open Signatures
open Common

module type PARAMS = sig
  val election : string
  val get_public_keys : unit -> string array option
  val get_public_creds : unit -> string Stream.t option
  val get_ballots : unit -> string Stream.t option
  val get_result : unit -> string option
  val print_msg : string -> unit
end

module type S = sig
  val vote : string option -> int array array -> string
  val decrypt : string -> string
  val finalize : string array -> string
  val verify : unit -> unit
end

(* Helpers *)

let ( / ) = Filename.concat

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Printf.eprintf "I: loading %s...\n%!" (Filename.basename filename);
    let ic = open_in filename in
    let lines =
      let rec loop lines =
        match (try Some (input_line ic) with End_of_file -> None) with
        | Some "" -> loop lines
        | Some line -> loop (of_string line::lines)
        | None -> lines
      in loop []
    in
    close_in ic;
    Some (List.rev lines)
  ) else None

type action =
| Vote of string * string (* privcred, ballot *)
| Decrypt of string (* privkey *)
| Verify
| Finalize

module type PARSED_PARAMS = sig
  include PARAMS
  include ELECTION_PARAMS
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let params = Group.election_params_of_string P.election in
  let module R = struct
    include P
    include (val params : ELECTION_PARAMS)
  end in
  (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct

  open P
  module M = Election.MakeSimpleMonad(G)
  module E = Election.MakeElection(G)(M);;

  (* Load and check trustee keys, if present *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  let public_keys_with_pok =
    get_public_keys () |> option_map @@
    Array.map (trustee_public_key_of_string G.read)

  let () =
    match public_keys_with_pok with
    | Some pks ->
      assert (Array.forall KG.check pks);
      let y' = KG.combine pks in
      assert G.(params.e_public_key =~ y')
    | None -> ()

  let public_keys =
    option_map (
      Array.map (fun pk -> pk.trustee_public_key)
    ) public_keys_with_pok

  (* Finish setting up the election *)

  let pks = match public_keys with
    | Some pks -> pks
    | None -> failwith "missing public keys"

  let e = {
    e_params = params;
    e_pks = Some pks;
    e_fingerprint = fingerprint;
  }

  (* Load ballots, if present *)

  module GSet = Set.Make (G)

  let public_creds = lazy (
    get_public_creds () |> option_map (fun creds ->
      let res = ref GSet.empty in
      Stream.iter (fun x -> res := GSet.add (G.of_string x) !res) creds;
      !res
    )
  )

  let ballots = lazy (
    get_ballots () |> option_map (fun ballots ->
      let res = ref [] in
      Stream.iter (fun x ->
        res := (ballot_of_string G.read x, sha256_b64 x) :: !res
      ) ballots;
      List.rev !res
    )
  )

  let check_signature_present = lazy (
    match Lazy.force public_creds with
    | Some creds -> (fun b ->
      match b.signature with
      | Some s -> GSet.mem s.s_public_key creds
      | None -> false
    )
    | None -> (fun _ -> true)
  )

  let cast (b, hash) =
    if Lazy.force check_signature_present b && E.check_ballot e b
    then M.cast b ()
    else Printf.ksprintf failwith "ballot %s failed tests" hash

  let ballots_check = lazy (
    Lazy.force ballots |> option_map (List.iter cast)
  )

  let encrypted_tally = lazy (
    match Lazy.force ballots_check with
      | None -> failwith "ballots.jsons is missing"
      | Some () ->
        M.fold (fun () b t ->
          M.return (E.combine_ciphertexts (E.extract_ciphertext b) t)
        ) (E.neutral_ciphertext e) ()
  )

  let vote privcred ballot =
    let sk =
      privcred |> option_map (fun cred ->
        let hex = derive_cred e.e_params.e_uuid cred in
        Z.(of_string_base 16 hex mod G.q)
      )
    in
    let b = E.create_ballot e ?sk (E.make_randomness e ()) ballot () in
    assert (E.check_ballot e b);
    string_of_ballot G.write b

  let decrypt privkey =
    let sk = number_of_string privkey in
    let pk = G.(g **~ sk) in
    if Array.forall (fun x -> not G.(x =~ pk)) pks then (
      print_msg "W: your key is not present in public_keys.jsons";
    );
    let tally = Lazy.force encrypted_tally in
    let factor = E.compute_factor tally sk () in
    assert (E.check_factor tally pk factor);
    string_of_partial_decryption G.write factor

  let finalize factors =
    let factors = Array.map (partial_decryption_of_string G.read) factors in
    let tally = Lazy.force encrypted_tally in
    assert (Array.forall2 (E.check_factor tally) pks factors);
    let result = E.combine_factors (M.cardinal ()) tally factors in
    assert (E.check_result e result);
    string_of_result G.write result

  let verify () =
    (match Lazy.force ballots_check with
    | Some () -> ()
    | None -> print_msg "W: no ballots to check"
    );
    (match get_result () with
    | Some result ->
      assert (E.check_result e (result_of_string G.read result))
    | None -> print_msg "W: no result to check"
    );
    print_msg "I: all checks passed"

end

let stream_lines_of_file fname =
  let ic = open_in fname in
  Stream.from (fun _ ->
    try Some (input_line ic)
    with End_of_file -> close_in ic; None
  )

module MakeGetters (X : sig val dir : string end) = struct

  let get_public_keys () =
    load_from_file (fun x -> x) (X.dir/"public_keys.jsons") |>
    option_map Array.of_list

  let get_public_creds () =
    try Some (stream_lines_of_file (X.dir/"public_creds.txt"))
    with _ -> None

  let get_ballots () =
    try Some (stream_lines_of_file (X.dir/"ballots.jsons"))
    with _ -> None

  let get_result () =
    load_from_file (fun x -> x) (X.dir/"result.json") |> function
    | Some [r] -> Some r
    | _ -> failwith "invalid result"

  let print_msg = prerr_endline

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)

open Tool_common

let main dir action =
  wrap_main (fun () ->
    Printf.eprintf "I: using directory %s\n%!" dir;
    let module P : PARAMS = struct
      include MakeGetters (struct let dir = dir end)
      let election =
        let fname = dir/"election.json" in
        load_from_file (fun x -> x) fname |>
        function
        | Some [e] -> e
        | None -> failcmd "could not read %s" fname
        | _ -> Printf.ksprintf failwith "invalid election file: %s" fname
    end in
    let p = parse_params (module P : PARAMS) in
    let module X = Make ((val p : PARSED_PARAMS)) in
    match action with
    | Vote (privcred, ballot) ->
      let ballot =
        match load_from_file plaintext_of_string ballot with
        | Some [b] -> b
        | _ -> failwith "invalid plaintext ballot file"
      and privcred =
        match load_from_file (fun x -> x) privcred with
        | Some [cred] -> cred
        | _ -> failwith "invalid credential"
      in
      print_endline (X.vote (Some privcred) ballot)
    | Decrypt privkey ->
      let privkey =
        match load_from_file (fun x -> x) privkey with
        | Some [privkey] -> privkey
        | _ -> failwith "invalid private key"
      in
      print_endline (X.decrypt privkey)
    | Verify -> X.verify ()
    | Finalize ->
      let factors =
        let fname = dir/"partial_decryptions.jsons" in
        match load_from_file (fun x -> x) fname with
        | Some factors -> Array.of_list factors
        | None -> failwith "cannot load partial decryptions"
      in
      let oc = open_out (dir/"result.json") in
      output_string oc (X.finalize factors);
      output_char oc '\n';
      close_out oc
  )

open Cmdliner

let dir_t =
  let doc = "Path to election files." in
  let the_info = Arg.info ["dir"] ~docv:"DIR" ~doc in
  Arg.(value & opt dir Filename.current_dir_name the_info)

let privcred_t =
  let doc = "Read private credential from file $(docv)." in
  let the_info = Arg.info ["privcred"] ~docv:"PRIV_CRED" ~doc in
  Arg.(value & opt (some file) None the_info)

let privkey_t =
  let doc = "Read private key from file $(docv)." in
  let the_info = Arg.info ["privkey"] ~docv:"PRIV_KEY" ~doc in
  Arg.(value & opt (some file) None the_info)

let ballot_t =
  let doc = "Read ballot choices from file $(docv)." in
  let the_info = Arg.info ["ballot"] ~docv:"BALLOT" ~doc in
  Arg.(value & opt (some file) None the_info)

let vote_cmd =
  let doc = "create a ballot" in
  let man = [
    `S "DESCRIPTION";
    `P "This command creates a ballot and prints it on standard output.";
  ] @ common_man in
  let main = Term.pure (fun d p b ->
    let p = get_mandatory_opt "--privcred" p in
    let b = get_mandatory_opt "--ballot" b in
    main d (Vote (p, b))
  ) in
  Term.(ret (main $ dir_t $ privcred_t $ ballot_t)),
  Term.info "vote" ~doc ~man

let verify_cmd =
  let doc = "verify election data" in
  let man = [
    `S "DESCRIPTION";
    `P "This command performs all possible verifications.";
  ] @ common_man in
  Term.(ret (pure main $ dir_t $ pure Verify)),
  Term.info "verify" ~doc ~man

let decrypt_cmd =
  let doc = "perform partial decryption" in
  let man = [
    `S "DESCRIPTION";
    `P "This command is run by each trustee to perform a partial decryption.";
  ] @ common_man in
  let main = Term.pure (fun d p ->
    let p = get_mandatory_opt "--privkey" p in
    main d (Decrypt p)
  ) in
  Term.(ret (main $ dir_t $ privkey_t)),
  Term.info "decrypt" ~doc ~man

let finalize_cmd =
  let doc = "finalizes an election" in
  let man = [
    `S "DESCRIPTION";
    `P "This command reads partial decryptions done by trustees from file $(i,partial_decryptions.jsons), checks them, combines them into the final tally and prints the result to standard output.";
    `P "The result structure contains partial decryptions itself, so $(i,partial_decryptions.jsons) can be discarded afterwards.";
  ] @ common_man in
  Term.(ret (pure main $ dir_t $ pure Finalize)),
  Term.info "finalize" ~doc ~man

let cmds = [vote_cmd; verify_cmd; decrypt_cmd; finalize_cmd]
