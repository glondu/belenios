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
open Util
open Serializable_t

(* Helpers *)

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Printf.eprintf "Loading %s...\n%!" filename;
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

let read_number = Serializable_builtin_j.read_number


module type PARAMS = sig
  val sk_file : string option
  val do_finalize : bool
  val do_decrypt : bool
  val ballot_file : string option
  val params : ff_pubkey params
  val election_fingerprint : string
  val group :  ff_params
  val y : number
end


module GetParams (X : EMPTY) : PARAMS = struct

  (* Command-line arguments *)

  let initial_dir = Sys.getcwd ()
  let dir = ref initial_dir
  let sk_file = ref None
  let do_finalize = ref false
  let do_decrypt = ref false
  let ballot_file = ref None

  let speclist = Arg.([
    "--dir", String (fun s -> dir := s), "path to election files";
    "--privkey", String (fun s ->
      let fname =
        if Filename.is_relative s then Filename.concat initial_dir s else s
      in sk_file := Some fname
    ), "path to private key";
  ])

  let usage_msg =
    Printf.sprintf "Usage: %s election [--dir <dir>] [--privkey <privkey>] { vote <ballot> | verify | decrypt | finalize }" Sys.argv.(0)

  let usage () =
    Arg.usage speclist usage_msg;
    exit 1

  let anon_args = ref []

  let anon_fun x =
    anon_args := x :: !anon_args

  let () = Arg.parse speclist anon_fun usage_msg

  let () = match List.rev !anon_args with
    | [] -> usage ()
    | ["vote"; f] ->
      let f =
        if Filename.is_relative f then Filename.concat initial_dir f else f
      in ballot_file := Some f
    | ["vote"] ->
      Printf.eprintf "ballot file is missing\n";
      exit 1
    | ["verify"] -> ()
    | ["finalize"] -> do_finalize := true
    | ["decrypt"] ->
      (match !sk_file with
      | None ->
        Printf.eprintf "--privkey is missing\n";
        exit 1
      | Some _ -> do_decrypt := true)
    | x :: _ -> usage ()

  let () = Sys.chdir !dir

  (* Load and check election *)

  let params, election_fingerprint =
    match (load_from_file (fun l ->
      Serializable_j.(params_of_string read_ff_pubkey l),
      sha256_b64 l
    ) "election.json") with
    | Some [e] -> e
    | _ -> failwith "invalid election file"

  let {ffpk_g = g; ffpk_p = p; ffpk_q = q; ffpk_y = y} = params.e_public_key
  let group = {g; p; q}

  let sk_file = !sk_file
  let do_finalize = !do_finalize
  let do_decrypt = !do_decrypt
  let ballot_file = !ballot_file

end


module RunTool (G : Election.FF_GROUP) (P : PARAMS) = struct

  open P
  module M = Election.MakeSimpleMonad(G)
  module E = Election.MakeElection(G)(M);;

  (* Load and check trustee keys, if present *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  let public_keys_with_pok =
    load_from_file (
      Serializable_j.trustee_public_key_of_string read_number
    ) "public_keys.jsons" |> option_map Array.of_list

  let () =
    match public_keys_with_pok with
    | Some pks ->
      assert (Array.forall KG.check pks);
      let y' = KG.combine pks in
      assert (P.y =% y')
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
    e_params = { params with e_public_key = P.y };
    e_pks = Some pks;
    e_fingerprint = election_fingerprint;
  }

  (* Load ballots, if present *)

  module ZSet = Set.Make(Z)

  let public_creds =
    load_from_file Z.of_string "public_creds.txt" |>
    option_map (fun xs ->
      List.fold_left (fun accu x ->
        ZSet.add x accu
      ) ZSet.empty xs
    )

  let ballots =
    load_from_file (fun line ->
      Serializable_j.ballot_of_string read_number line,
      sha256_b64 line
    ) "ballots.jsons"

  let check_signature_present =
    match public_creds with
    | Some creds -> (fun b ->
      match b.signature with
      | Some s -> ZSet.mem s.s_public_key creds
      | None -> false
    )
    | None -> (fun _ -> true)

  let vote (b, hash) =
    if check_signature_present b && E.check_ballot e b
    then M.cast b ()
    else Printf.ksprintf failwith "ballot %s failed tests" hash

  let () = ballots |> option_map (List.iter vote) |> ignore

  let encrypted_tally = lazy (
    match ballots with
      | None -> failwith "ballots.jsons is missing"
      | Some _ ->
        M.fold (fun () b t ->
          M.return (E.combine_ciphertexts (E.extract_ciphertext b) t)
        ) (E.neutral_ciphertext e) ()
  )

  let () = match ballot_file with
    | None -> ()
    | Some fn ->
      (match load_from_file Serializable_j.plaintext_of_string fn with
      | Some [b] ->
        let sk =
          match sk_file with
          | Some fn ->
            (match load_from_file (fun x -> x) fn with
            | Some [cred] ->
              let hex = Credgen.derive e.e_params.e_uuid cred in
              Some Z.(of_string_base 16 hex mod G.q)
            | _ -> failwith "invalid credential file"
            )
          | None -> None
        in
        let b = E.create_ballot e ?sk (E.make_randomness e ()) b () in
        assert (E.check_ballot e b);
        print_endline (
          Serializable_j.string_of_ballot
            Serializable_builtin_j.write_number b
        )
      | _ -> failwith "invalid plaintext ballot file"
      )

  let () = if do_decrypt then
    match sk_file with
    | Some fn ->
      (match load_from_file (Serializable_builtin_j.number_of_string) fn with
        | Some [sk] ->
          let pk = G.(g **~ sk) in
          if Array.forall (fun x -> not (x =% pk)) pks then (
            Printf.eprintf "Warning: your key is not present in public_keys.jsons!\n";
          );
          let tally = Lazy.force encrypted_tally in
          let factor =
            E.compute_factor tally sk ()
          in
          assert (E.check_factor tally pk factor);
          print_endline (
            Serializable_j.string_of_partial_decryption
              Serializable_builtin_j.write_number
              factor
          )
        | _ -> failwith "invalid private key file"
      )
    | None -> ()

  (* Load or compute result, and check it *)

  let result =
    load_from_file (
      Serializable_j.result_of_string read_number
    ) "result.json"

  let () =
    match result with
    | Some [result] ->
      assert (E.check_result e result)
    | Some _ ->
      failwith "invalid result file"
    | None ->
      let factors = load_from_file (
        Serializable_j.partial_decryption_of_string read_number
      ) "partial_decryptions.jsons" |> option_map Array.of_list in
      match factors with
      | Some factors ->
        let tally = Lazy.force encrypted_tally in
        assert (Array.forall2 (E.check_factor tally) pks factors);
        let result = E.combine_factors (M.cardinal ()) tally factors in
        assert (E.check_result e result);
        if do_finalize then (
          save_to "result.json" (
            Serializable_j.write_result Serializable_builtin_j.write_number
          ) result;
          Printf.eprintf "result.json written\n%!"
        );
      | None -> ()

  (* The end *)

  let () = Printf.eprintf "All checks passed!\n%!"

end


let main () =
  let module P = GetParams(struct end) in
  let module G = (val Election.finite_field P.group : Election.FF_GROUP) in
  let module X = RunTool (G) (P) in
  ()
