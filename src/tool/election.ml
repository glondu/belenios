(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

open Belenios_core.Common
open Belenios_core.Serializable_j
open Common
open Tool_election
open Cmdliner

let main url dir action =
  let@ () = wrap_main in
  let dir, cleanup =
    match (url, dir) with
    | Some _, None ->
        let tmp = Filename.temp_file "belenios" "" in
        Unix.unlink tmp;
        Unix.mkdir tmp 0o700;
        (tmp, true)
    | None, None -> (Filename.current_dir_name, false)
    | _, Some d -> (d, false)
  in
  Printf.eprintf "I: using directory %s\n%!" dir;
  let file =
    match url with
    | None -> find_bel_in_dir dir
    | Some u -> (
        match download dir u with
        | Some x -> x
        | None -> failwith "error while downloading")
  in
  let module X =
    Make
      (struct
        let file = dir // file
      end)
      ()
  in
  (match action with
  | `Vote (privcred, choice) ->
      let choice =
        match load_from_file plaintext_of_string choice with
        | Some [ c ] -> c
        | _ -> failwith "invalid choice file"
      and privcred =
        match load_from_file (fun x -> x) privcred with
        | Some [ cred ] -> cred
        | _ -> failwith "invalid credential"
      in
      print_endline (X.vote (Some privcred) choice)
  | `Decrypt (i, privkey) ->
      let privkey =
        match load_from_file (fun x -> x) privkey with
        | Some [ privkey ] -> privkey
        | _ -> failwith "invalid private key"
      in
      print_endline2 (X.decrypt i privkey)
  | `TDecrypt (i, key, pdk) ->
      let key = string_of_file key in
      let pdk = string_of_file pdk in
      print_endline2 (X.tdecrypt i key pdk)
  | `VerifyBallot b ->
      let ballot =
        match load_from_file (fun x -> x) b with
        | Some [ ballot ] -> ballot
        | _ -> failwith "invalid ballot file"
      in
      X.verify_ballot ballot
  | `Verify -> X.verify ()
  | `ComputeResult -> print_endline (X.compute_result ())
  | `Shuffle trustee_id -> print_endline2 (X.shuffle_ciphertexts trustee_id)
  | `Checksums -> X.checksums () |> print_endline
  | `ComputeVoters privcreds ->
      X.compute_voters privcreds |> List.iter print_endline
  | `ComputeBallotSummary -> X.compute_ballot_summary () |> print_endline
  | `ComputeEncryptedTally -> print_endline2 (X.compute_encrypted_tally ()));
  if cleanup then rm_rf dir

let privcred_t =
  let doc = "Read private credential from file $(docv)." in
  let the_info = Arg.info [ "privcred" ] ~docv:"PRIV_CRED" ~doc in
  Arg.(value & opt (some file) None the_info)

let privkey_t =
  let doc = "Read private key from file $(docv)." in
  let the_info = Arg.info [ "privkey" ] ~docv:"PRIV_KEY" ~doc in
  Arg.(value & opt (some file) None the_info)

let choice_t =
  let doc = "Read voting choice from file $(docv)." in
  let the_info = Arg.info [ "choice" ] ~docv:"CHOICE" ~doc in
  Arg.(value & opt (some file) None the_info)

let ballot_t =
  let doc = "Read ballot from file $(docv)." in
  let the_info = Arg.info [ "ballot" ] ~docv:"BALLOT" ~doc in
  Arg.(value & opt (some file) None the_info)

let pdk_t =
  let doc = "Read (encrypted) decryption key from file $(docv)." in
  let the_info = Arg.info [ "decryption-key" ] ~docv:"KEY" ~doc in
  Arg.(value & opt (some file) None the_info)

let privcreds_t =
  let doc = "Read private credentials from file $(docv)." in
  let the_info = Arg.info [ "privcreds" ] ~docv:"PRIVCREDS" ~doc in
  Arg.(value & opt (some file) None the_info)

let trustee_id_t =
  let doc = "Trustee identifier (an integer)." in
  let the_info = Arg.info [ "trustee-id" ] ~docv:"TRUSTEE-ID" ~doc in
  Arg.(value & opt (some int) None the_info)

let vote_cmd =
  let doc = "create a ballot" in
  let man =
    [
      `S "DESCRIPTION";
      `P "This command creates a ballot and prints it on standard output.";
    ]
    @ common_man
  in
  let main =
    Term.const (fun u d p c ->
        let p = get_mandatory_opt "--privcred" p in
        let c = get_mandatory_opt "--choice" c in
        main u d (`Vote (p, c)))
  in
  Cmd.v
    (Cmd.info "generate-ballot" ~doc ~man)
    Term.(ret (main $ url_t $ optdir_t $ privcred_t $ choice_t))

let verify_ballot_cmd =
  let doc = "verify a single ballot" in
  let man =
    [
      `S "DESCRIPTION";
      `P "This command performs verifications on a single ballot.";
    ]
    @ common_man
  in
  let main =
    Term.const (fun u d b ->
        let b = get_mandatory_opt "--encrypted-ballot" b in
        main u d (`VerifyBallot b))
  in
  Cmd.v
    (Cmd.info "verify-ballot" ~doc ~man)
    Term.(ret (main $ url_t $ optdir_t $ ballot_t))

let verify_cmd =
  let doc = "verify election data" in
  let man =
    [ `S "DESCRIPTION"; `P "This command performs all possible verifications." ]
    @ common_man
  in
  Cmd.v
    (Cmd.info "verify" ~doc ~man)
    Term.(ret (const main $ url_t $ optdir_t $ const `Verify))

let decrypt_man =
  [
    `S "DESCRIPTION";
    `P "This command is run by each trustee to perform a partial decryption.";
  ]
  @ common_man

let decrypt_cmd =
  let doc = "perform partial decryption" in
  let main =
    Term.const (fun u d i p ->
        let i = get_mandatory_opt "--trustee-id" i in
        let p = get_mandatory_opt "--privkey" p in
        main u d (`Decrypt (i, p)))
  in
  Cmd.v
    (Cmd.info "decrypt" ~doc ~man:decrypt_man)
    Term.(ret (main $ url_t $ optdir_t $ trustee_id_t $ privkey_t))

let tdecrypt_cmd =
  let doc = "perform partial decryption (threshold version)" in
  let main =
    Term.const (fun u d i k pdk ->
        let i = get_mandatory_opt "--trustee-id" i in
        let k = get_mandatory_opt "--key" k in
        let pdk = get_mandatory_opt "--decryption-key" pdk in
        main u d (`TDecrypt (i, k, pdk)))
  in
  Cmd.v
    (Cmd.info "decrypt-threshold" ~doc ~man:decrypt_man)
    Term.(ret (main $ url_t $ optdir_t $ trustee_id_t $ key_t $ pdk_t))

let compute_result_cmd =
  let doc = "computes the result of an election" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command computes the result of an election. It assumes all \
         necessary partial decryptions have been done, checks them, combines \
         them into the final tally and prints the result to standard output.";
    ]
    @ common_man
  in
  Cmd.v
    (Cmd.info "compute-result" ~doc ~man)
    Term.(ret (const main $ url_t $ optdir_t $ const `ComputeResult))

let shuffle_cmd =
  let doc = "shuffle ciphertexts" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command shuffles non-homomorphic ciphertexts and prints on \
         standard output the shuffle proof and the shuffled ciphertexts.";
    ]
    @ common_man
  in
  let main =
    Term.const (fun u d i ->
        let i = get_mandatory_opt "--trustee-id" i in
        main u d (`Shuffle i))
  in
  Cmd.v
    (Cmd.info "shuffle" ~doc ~man)
    Term.(ret (main $ url_t $ optdir_t $ trustee_id_t))

let checksums_cmd =
  let doc = "compute checksums" in
  let man =
    [
      `S "DESCRIPTION";
      `P "This command computes checksums needed to audit an election.";
    ]
    @ common_man
  in
  Cmd.v
    (Cmd.info "compute-checksums" ~doc ~man)
    Term.(ret (const main $ url_t $ optdir_t $ const `Checksums))

let compute_voters_cmd =
  let doc = "compute actual voters" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command computes the list of voters that actually voted in an \
         election, from the list of ballots and private credentials.";
    ]
    @ common_man
  in
  let main =
    Term.const (fun u d privcreds ->
        let privcreds =
          get_mandatory_opt "--privcreds" privcreds
          |> string_of_file |> Yojson.Safe.from_string |> key_value_list_of_json
        in
        main u d (`ComputeVoters privcreds))
  in
  Cmd.v
    (Cmd.info "compute-voters" ~doc ~man)
    Term.(ret (main $ url_t $ optdir_t $ privcreds_t))

let compute_ballot_summary_cmd =
  let doc = "compute ballot summary" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command compute the hash (also known as smart ballot tracker) \
         and weight of all ballots.";
    ]
    @ common_man
  in
  Cmd.v
    (Cmd.info "compute-ballot-summary" ~doc ~man)
    Term.(ret (const main $ url_t $ optdir_t $ const `ComputeBallotSummary))

let compute_encrypted_tally_cmd =
  let doc = "compute encrypted tally" in
  let man =
    [
      `S "DESCRIPTION";
      `P "This command computes the encrypted tally of the election.";
    ]
    @ common_man
  in
  let main = Term.const (fun u d -> main u d `ComputeEncryptedTally) in
  Cmd.v
    (Cmd.info "compute-encrypted-tally" ~doc ~man)
    Term.(ret (main $ url_t $ optdir_t))

module Verifydiff : CMDLINER_MODULE = struct
  open Tool_verifydiff

  let main dir1 dir2 =
    let@ () = wrap_main in
    match (dir1, dir2) with
    | Some dir1, Some dir2 -> verifydiff dir1 dir2
    | _, _ -> failcmd "--dir1 or --dir2 is missing"

  let dir1_t =
    let doc = "First directory to compare." in
    Arg.(value & opt (some dir) None & info [ "dir1" ] ~docv:"DIR1" ~doc)

  let dir2_t =
    let doc = "Second directory to compare." in
    Arg.(value & opt (some dir) None & info [ "dir2" ] ~docv:"DIR2" ~doc)

  let cmd =
    let doc = "verify an election directory update" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command is run by an auditor on two directories $(i,DIR1) and \
           $(i,DIR2). It checks that $(i,DIR2) is a valid update of $(i,DIR1).";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "verify-diff" ~doc ~man)
      Term.(ret (const main $ dir1_t $ dir2_t))
end

let cmds =
  [
    vote_cmd;
    verify_ballot_cmd;
    verify_cmd;
    decrypt_cmd;
    tdecrypt_cmd;
    compute_result_cmd;
    shuffle_cmd;
    checksums_cmd;
    compute_voters_cmd;
    compute_ballot_summary_cmd;
    compute_encrypted_tally_cmd;
    Verifydiff.cmd;
  ]

let cmd =
  let doc = "election management commands" in
  let man = common_man in
  let info = Cmd.info "election" ~doc ~man in
  Cmd.group info cmds
