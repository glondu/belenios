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

open Serializable_builtin_j
open Serializable_j
open Signatures
open Common

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


module type PARAMS = sig
  val sk_file : string option
  val do_finalize : bool
  val do_decrypt : bool
  val ballot_file : string option
  include ELECTION_PARAMS
end


let parse_args () = begin

  (* Command-line arguments *)

  let initial_dir = Sys.getcwd () in
  let dir = ref initial_dir in
  let sk_file = ref None in
  let do_finalize = ref false in
  let do_decrypt = ref false in
  let ballot_file = ref None in

  let speclist = Arg.([
    "--dir", String (fun s -> dir := s), "path to election files";
    "--privkey", String (fun s ->
      let fname =
        if Filename.is_relative s then Filename.concat initial_dir s else s
      in sk_file := Some fname
    ), "path to private key";
  ]) in

  let usage_msg =
    Printf.sprintf "Usage: %s election [--dir <dir>] [--privkey <privkey>] { vote <ballot> | verify | decrypt | finalize }" Sys.argv.(0)
  in

  let usage () =
    Arg.usage speclist usage_msg;
    exit 1
  in

  let anon_args = ref [] in

  let anon_fun x =
    anon_args := x :: !anon_args
  in

  let () = Arg.parse speclist anon_fun usage_msg in

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
  in

  let () = Sys.chdir !dir in

  (* Load and check election *)

  let params =
    match load_from_file Group.election_params_of_string "election.json" with
    | Some [e] -> e
    | _ -> failwith "invalid election file"
  in

  let module P = struct
    let sk_file = !sk_file
    let do_finalize = !do_finalize
    let do_decrypt = !do_decrypt
    let ballot_file = !ballot_file
    include (val params : ELECTION_PARAMS)
  end in

  (module P : PARAMS)

end

module Run (P : PARAMS) : EMPTY = struct

  open P
  module M = Election.MakeSimpleMonad(G)
  module E = Election.MakeElection(G)(M);;

  (* Load and check trustee keys, if present *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  let public_keys_with_pok =
    load_from_file (
      trustee_public_key_of_string G.read
    ) "public_keys.jsons" |> option_map Array.of_list

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

  let public_creds =
    load_from_file G.of_string "public_creds.txt" |>
    option_map (fun xs ->
      List.fold_left (fun accu x ->
        GSet.add x accu
      ) GSet.empty xs
    )

  let ballots =
    load_from_file (fun line ->
      ballot_of_string G.read line,
      sha256_b64 line
    ) "ballots.jsons"

  let check_signature_present =
    match public_creds with
    | Some creds -> (fun b ->
      match b.signature with
      | Some s -> GSet.mem s.s_public_key creds
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
      (match load_from_file plaintext_of_string fn with
      | Some [b] ->
        let sk =
          match sk_file with
          | Some fn ->
            (match load_from_file (fun x -> x) fn with
            | Some [cred] ->
              let hex = Tool_credgen.derive e.e_params.e_uuid cred in
              Some Z.(of_string_base 16 hex mod G.q)
            | _ -> failwith "invalid credential file"
            )
          | None -> None
        in
        let b = E.create_ballot e ?sk (E.make_randomness e ()) b () in
        assert (E.check_ballot e b);
        print_endline (string_of_ballot G.write b)
      | _ -> failwith "invalid plaintext ballot file"
      )

  let () = if do_decrypt then
    match sk_file with
    | Some fn ->
      (match load_from_file (number_of_string) fn with
        | Some [sk] ->
          let pk = G.(g **~ sk) in
          if Array.forall (fun x -> not G.(x =~ pk)) pks then (
            Printf.eprintf "Warning: your key is not present in public_keys.jsons!\n";
          );
          let tally = Lazy.force encrypted_tally in
          let factor =
            E.compute_factor tally sk ()
          in
          assert (E.check_factor tally pk factor);
          print_endline (string_of_partial_decryption G.write factor)
        | _ -> failwith "invalid private key file"
      )
    | None -> ()

  (* Load or compute result, and check it *)

  let result =
    load_from_file (
      result_of_string G.read
    ) "result.json"

  let () =
    match result with
    | Some [result] ->
      assert (E.check_result e result)
    | Some _ ->
      failwith "invalid result file"
    | None ->
      let factors = load_from_file (
        partial_decryption_of_string G.read
      ) "partial_decryptions.jsons" |> option_map Array.of_list in
      match factors with
      | Some factors ->
        let tally = Lazy.force encrypted_tally in
        assert (Array.forall2 (E.check_factor tally) pks factors);
        let result = E.combine_factors (M.cardinal ()) tally factors in
        assert (E.check_result e result);
        if do_finalize then (
          save_to "result.json" (
            write_result G.write
          ) result;
          Printf.eprintf "result.json written\n%!"
        );
      | None -> ()

  (* The end *)

  let () = Printf.eprintf "All checks passed!\n%!"

end


let main () =
  let module P = (val parse_args () : PARAMS) in
  let module X : EMPTY = Run (P) in
  ()
