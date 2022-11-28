(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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

open Belenios
open Belenios_platform
open Belenios_core.Serializable_builtin_t
open Belenios_core.Serializable_j
open Belenios_core.Common
open Common
open Cmdliner

module Shasum : CMDLINER_MODULE = struct

  let main () =
    wrap_main (fun () -> chars_of_stdin () |> sha256_b64 |> print_endline)

  let sha256_b64_cmd =
    let doc = "compute SHA256 of standard input and encode it in Base64Compact" in
    let man = [
        `S "DESCRIPTION";
        `P "This command compute the SHA256 of standard input and encode it in Base64Compact. This computation is frequent when auditing an election. This single shell command is equivalent to the following shell pipeline:";
        `Pre "sha256sum | xxd -r -p | base64 | tr -d \"=\"";
        `P "but does not need each individual command to be available.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "sha256-b64" ~doc ~man)
      Term.(ret (const main $ const ()))

  let cmds = [sha256_b64_cmd]

end

module ElectionManagement : CMDLINER_MODULE = struct
  open Tool_election

  let main url dir action =
    let@ () = wrap_main in
    let dir, cleanup = match url, dir with
      | Some _, None ->
         let tmp = Filename.temp_file "belenios" "" in
         Unix.unlink tmp;
         Unix.mkdir tmp 0o700;
         tmp, true
      | None, None -> Filename.current_dir_name, false
      | _, Some d -> d, false
    in
    Printf.eprintf "I: using directory %s\n%!" dir;
    let file =
      match url with
      | None -> find_bel_in_dir dir
      | Some u ->
         match download dir u with
         | Some x -> x
         | None -> failwith "error while downloading"
    in
    let module X = Make (struct let file = dir // file end) () in
    begin match action with
    | `Vote (privcred, ballot) ->
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
    | `Decrypt (i, privkey) ->
       let privkey =
         match load_from_file (fun x -> x) privkey with
         | Some [privkey] -> privkey
         | _ -> failwith "invalid private key"
       in
       print_endline2 (X.decrypt i privkey)
    | `TDecrypt (i, key, pdk) ->
       let key = string_of_file key in
       let pdk = string_of_file pdk in
       print_endline2 (X.tdecrypt i key pdk)
    | `Verify -> X.verify ()
    | `ComputeResult -> print_endline (X.compute_result ())
    | `Shuffle trustee_id ->
       print_endline2 (X.shuffle_ciphertexts trustee_id)
    | `Checksums ->
       X.checksums () |> print_endline
    | `ComputeVoters privcreds ->
       X.compute_voters privcreds |> List.iter print_endline
    | `ComputeBallotSummary ->
       X.compute_ballot_summary () |> print_endline
    | `ComputeEncryptedTally ->
       print_endline2 (X.compute_encrypted_tally ())
    end;
    if cleanup then rm_rf dir

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

  let pdk_t =
    let doc = "Read (encrypted) decryption key from file $(docv)." in
    let the_info = Arg.info ["decryption-key"] ~docv:"KEY" ~doc in
    Arg.(value & opt (some file) None the_info)

  let privcreds_t =
    let doc = "Read private credentials from file $(docv)." in
    let the_info = Arg.info ["privcreds"] ~docv:"PRIVCREDS" ~doc in
    Arg.(value & opt (some file) None the_info)

  let trustee_id_t =
    let doc = "Trustee identifier (an integer)." in
    let the_info = Arg.info ["trustee-id"] ~docv:"TRUSTEE-ID" ~doc in
    Arg.(value & opt (some int) None the_info)

  let vote_cmd =
    let doc = "create a ballot" in
    let man = [
      `S "DESCRIPTION";
      `P "This command creates a ballot and prints it on standard output.";
    ] @ common_man in
    let main = Term.const (fun u d p b ->
      let p = get_mandatory_opt "--privcred" p in
      let b = get_mandatory_opt "--ballot" b in
      main u d (`Vote (p, b))
    ) in
    Cmd.v (Cmd.info "generate-ballot" ~doc ~man)
      Term.(ret (main $ url_t $ optdir_t $ privcred_t $ ballot_t))

  let verify_cmd =
    let doc = "verify election data" in
    let man = [
      `S "DESCRIPTION";
      `P "This command performs all possible verifications.";
    ] @ common_man in
    Cmd.v (Cmd.info "verify" ~doc ~man)
      Term.(ret (const main $ url_t $ optdir_t $ const `Verify))

  let decrypt_man = [
      `S "DESCRIPTION";
      `P "This command is run by each trustee to perform a partial decryption.";
    ] @ common_man

  let decrypt_cmd =
    let doc = "perform partial decryption" in
    let main = Term.const (fun u d i p ->
      let i = get_mandatory_opt "--trustee-id" i in
      let p = get_mandatory_opt "--privkey" p in
      main u d (`Decrypt (i, p))
    ) in
    Cmd.v (Cmd.info "decrypt" ~doc ~man:decrypt_man)
      Term.(ret (main $ url_t $ optdir_t $ trustee_id_t $ privkey_t))

  let tdecrypt_cmd =
    let doc = "perform partial decryption (threshold version)" in
    let main = Term.const (fun u d i k pdk ->
                   let i = get_mandatory_opt "--trustee-id" i in
                   let k = get_mandatory_opt "--key" k in
                   let pdk = get_mandatory_opt "--decryption-key" pdk in
                   main u d (`TDecrypt (i, k, pdk))
                 )
    in
    Cmd.v (Cmd.info "decrypt-threshold" ~doc ~man:decrypt_man)
      Term.(ret (main $ url_t $ optdir_t $ trustee_id_t $ key_t $ pdk_t))

  let compute_result_cmd =
    let doc = "computes the result of an election" in
    let man = [
      `S "DESCRIPTION";
      `P "This command computes the result of an election. It assumes all necessary partial decryptions have been done, checks them, combines them into the final tally and prints the result to standard output.";
    ] @ common_man in
    Cmd.v (Cmd.info "compute-result" ~doc ~man)
      Term.(ret (const main $ url_t $ optdir_t $ const `ComputeResult))

  let shuffle_cmd =
    let doc = "shuffle ciphertexts" in
    let man = [
        `S "DESCRIPTION";
        `P "This command shuffles non-homomorphic ciphertexts and prints on standard output the shuffle proof and the shuffled ciphertexts.";
      ] @ common_man
    in
    let main = Term.const (fun u d i ->
                   let i = get_mandatory_opt "--trustee-id" i in
                   main u d (`Shuffle i)
                 )
    in
    Cmd.v (Cmd.info "shuffle" ~doc ~man)
      Term.(ret (main $ url_t $ optdir_t $ trustee_id_t))

  let checksums_cmd =
    let doc = "compute checksums" in
    let man = [
        `S "DESCRIPTION";
        `P "This command computes checksums needed to audit an election.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "compute-checksums" ~doc ~man)
      Term.(ret (const main $ url_t $ optdir_t $ const `Checksums))

  let compute_voters_cmd =
    let doc = "compute actual voters" in
    let man = [
        `S "DESCRIPTION";
        `P "This command computes the list of voters that actually voted in an election, from the list of ballots and private credentials.";
      ] @ common_man
    in
    let main =
      Term.const
        (fun u d privcreds ->
          let privcreds =
            get_mandatory_opt "--privcreds" privcreds
            |> lines_of_file
          in
          main u d (`ComputeVoters privcreds)
        )
    in
    Cmd.v (Cmd.info "compute-voters" ~doc ~man)
      Term.(ret (main $ url_t $ optdir_t $ privcreds_t))

  let compute_ballot_summary_cmd =
    let doc = "compute ballot summary" in
    let man = [
        `S "DESCRIPTION";
        `P "This command compute the hash (also known as smart ballot tracker) and weight of all ballots.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "compute-ballot-summary" ~doc ~man)
      Term.(ret (const main $ url_t $ optdir_t $ const `ComputeBallotSummary))

  let compute_encrypted_tally_cmd =
    let doc = "compute encrypted tally" in
    let man = [
        `S "DESCRIPTION";
        `P "This command computes the encrypted tally of the election.";
      ] @ common_man
    in
    let main = Term.const (fun u d -> main u d `ComputeEncryptedTally) in
    Cmd.v (Cmd.info "compute-encrypted-tally" ~doc ~man)
      Term.(ret (main $ url_t $ optdir_t))

  let cmds =
    [
      vote_cmd;
      verify_cmd;
      decrypt_cmd;
      tdecrypt_cmd;
      compute_result_cmd;
      shuffle_cmd;
      checksums_cmd;
      compute_voters_cmd;
      compute_ballot_summary_cmd;
      compute_encrypted_tally_cmd;
    ]

end

module Verifydiff : CMDLINER_MODULE = struct
  open Tool_verifydiff

  let main dir1 dir2 =
    let@ () = wrap_main in
    match dir1, dir2 with
    | Some dir1, Some dir2 -> verifydiff dir1 dir2
    | _, _ -> failcmd "--dir1 or --dir2 is missing"

  let dir1_t =
    let doc = "First directory to compare." in
    Arg.(value & opt (some dir) None & info ["dir1"] ~docv:"DIR1" ~doc)

  let dir2_t =
    let doc = "Second directory to compare." in
    Arg.(value & opt (some dir) None & info ["dir2"] ~docv:"DIR2" ~doc)

  let verifydiff_cmd =
    let doc = "verify an election directory update" in
    let man = [
        `S "DESCRIPTION";
        `P "This command is run by an auditor on two directories $(i,DIR1) and $(i,DIR2). It checks that $(i,DIR2) is a valid update of $(i,DIR1).";
      ] @ common_man in
    Cmd.v (Cmd.info "verify-diff" ~doc ~man)
      Term.(ret (const main $ dir1_t $ dir2_t))

  let cmds = [verifydiff_cmd]

end

module Events : CMDLINER_MODULE = struct

  let init dir election trustees public_creds =
    let@ () = wrap_main in
    let election = string_of_file election in
    let trustees = string_of_file trustees in
    let public_creds =
      string_of_file public_creds
      |> public_credentials_of_string
      |> List.map strip_cred
      |> string_of_public_credentials
    in
    let file =
      let election = Election.of_string election in
      dir // raw_string_of_uuid election.e_uuid ^ ".bel"
    in
    ignore (Tool_events.init ~file ~election ~trustees ~public_creds)

  let add_event dir event_typ =
    let@ () = wrap_main in
    let file = dir // find_bel_in_dir dir in
    let index = Tool_events.get_index ~file in
    let event_typ =
      get_mandatory_opt "--type" event_typ
      |> Printf.sprintf "%S"
      |> event_type_of_string
    in
    let payloads = lines_of_stdin () in
    let payload =
      match List.rev payloads with
      | x :: _ -> Some (Hash.hash_string x)
      | _ -> None
    in
    let open Tool_events in
    List.map (fun x -> Data x) payloads @ [Event (event_typ, payload)]
    |> append index

  let election_t =
    let doc = "Read election parameters from file $(docv)." in
    Arg.(value & opt file "election.json" & info ["election"] ~docv:"ELECTION" ~doc)

  let trustees_t =
    let doc = "Read trustees from file $(docv)." in
    Arg.(value & opt file "trustees.json" & info ["trustees"] ~docv:"TRUSTEES" ~doc)

  let public_creds_t =
    let doc = "Read public credentials from file $(docv)." in
    Arg.(value & opt file "public_creds.json" & info ["public-creds"] ~docv:"PUBLIC-CREDS" ~doc)

  let event_typ_t =
    let doc = "Type of event." in
    Arg.(value & opt (some string) None & info ["type"] ~docv:"TYPE" ~doc)

  let init_cmd =
    let doc = "initialize events" in
    let man = [
        `S "DESCRIPTION";
        `P "This command creates $(i,UUID.bel) from election setup files.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "init" ~doc ~man)
      Term.(ret (const init $ dir_t $ election_t $ trustees_t $ public_creds_t))

  let add_event_cmd =
    let doc = "add an event" in
    let man = [
        `S "DESCRIPTION";
        `P "This command adds a new event to $(i,UUID.bel). If stdin is non-empty, each of its lines is added to $(i,UUID.bel) prior to the event, and the last line is added as payload of the event.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "add-event" ~doc ~man)
      Term.(ret (const add_event $ dir_t $ event_typ_t))

  let events_cmd =
    let doc = "manage archives" in
    let man = common_man in
    let info = Cmd.info "archive" ~doc ~man in
    Cmd.group info ([init_cmd; add_event_cmd] @ Tool_mkarchive.cmds)

  let cmds = [events_cmd]

end

module Methods : CMDLINER_MODULE = struct

  let schulze nchoices blank_allowed =
    let@ () = wrap_main in
    let ballots = chars_of_stdin () |> condorcet_ballots_of_string in
    let nchoices =
      if nchoices = 0 then
        if Array.length ballots > 0 then Array.length ballots.(0) else 0
      else nchoices
    in
    if nchoices <= 0 then
      failcmd "invalid --nchoices parameter (or could not infer it)"
    else
      let blank_allowed =
        match blank_allowed with
        | None -> failcmd "--blank-allowed is missing"
        | Some b -> b
      in
      ballots
      |> Belenios_core.Schulze.compute ~nchoices ~blank_allowed
      |> string_of_schulze_result
      |> print_endline

  let mj nchoices ngrades blank_allowed =
    let@ () = wrap_main in
    let ballots = chars_of_stdin () |> mj_ballots_of_string in
    let nchoices =
      if nchoices = 0 then
        if Array.length ballots > 0 then Array.length ballots.(0) else 0
      else nchoices
    in
    if nchoices <= 0 then
      failcmd "invalid --nchoices parameter (or could not infer it)"
    else
      let ngrades =
        match ngrades with
        | None -> failcmd "--ngrades is missing"
        | Some i -> if i > 0 then i else failcmd "invalid --ngrades parameter"
      in
      let blank_allowed =
        match blank_allowed with
        | None -> failcmd "--blank-allowed is missing"
        | Some b -> b
      in
      ballots
      |> Belenios_core.Majority_judgment.compute ~nchoices ~ngrades ~blank_allowed
      |> string_of_mj_result
      |> print_endline

  let stv nseats =
    let@ () = wrap_main in
    let nseats =
      match nseats with
      | None -> failcmd "--nseats is missing"
      | Some i -> if i > 0 then i else failcmd "invalid --nseats parameter"
    in
    chars_of_stdin ()
    |> stv_raw_ballots_of_string
    |> Belenios_core.Stv.compute ~nseats
    |> string_of_stv_result
    |> print_endline

  let nchoices_t =
    let doc = "Number of choices. If 0, try to infer it." in
    Arg.(value & opt int 0 & info ["nchoices"] ~docv:"N" ~doc)

  let ngrades_t =
    let doc = "Number of grades." in
    Arg.(value & opt (some int) None & info ["ngrades"] ~docv:"G" ~doc)

  let nseats_t =
    let doc = "Number of seats." in
    Arg.(value & opt (some int) None & info ["nseats"] ~docv:"N" ~doc)

  let blank_allowed_t =
    let doc = "Is blank allowed?" in
    Arg.(value & opt (some bool) None & info ["blank-allowed"] ~docv:"B" ~doc)

  let schulze_cmd =
    let doc = "compute Schulze result" in
    let man = [
        `S "DESCRIPTION";
        `P "This command reads on standard input JSON-formatted ballots and interprets them as Condorcet rankings on $(i,N) choices. It then computes the result according to the Schulze method and prints it on standard output.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "schulze" ~doc ~man)
      Term.(ret (const schulze $ nchoices_t $ blank_allowed_t))

  let mj_cmd =
    let doc = "compute Majority Judgment result" in
    let man = [
        `S "DESCRIPTION";
        `P "This command reads on standard input JSON-formatted ballots and interprets them as grades (ranging from 1 (best) to $(i,G) (worst)) given to $(i,N) choices. It then computes the result according to the Majority Judgment method and prints it on standard output.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "majority-judgment" ~doc ~man)
      Term.(ret (const mj $ nchoices_t $ ngrades_t $ blank_allowed_t))

  let stv_cmd =
    let doc = "compute Single Transferable Vote result" in
    let man = [
        `S "DESCRIPTION";
        `P "This command reads on standard input JSON-formatted ballots and interprets them as rankings of choices (ranging from 1 (best) to $(i,X) (worst)). It then computes the result according to the Single Transferable Vote method and prints it on standard output.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "stv" ~doc ~man)
      Term.(ret (const stv $ nseats_t))

  let method_cmd =
    let doc = "compute result with specific counting methods" in
    let man = common_man in
    let info = Cmd.info "method" ~doc ~man in
    Cmd.group info [schulze_cmd; mj_cmd; stv_cmd]

  let cmds = [method_cmd]

end

module Election : CMDLINER_MODULE = struct
  let cmd =
    let doc = "election management commands" in
    let man = common_man in
    let info = Cmd.info "election" ~doc ~man in
    let cmds =
      List.flatten
        [
          ElectionManagement.cmds;
          Verifydiff.cmds;
        ]
    in
    Cmd.group info cmds

  let cmds = [cmd]
end

let cmds =
  List.flatten
    [
      Shasum.cmds;
      Setup.cmds;
      Election.cmds;
      Events.cmds;
      Methods.cmds;
      Sealing.cmds;
    ]

let default_cmd =
  let open Version in
  let version = Printf.sprintf "%s (%s)" version build in
  let version = if debug then version ^ " [debug]" else version in
  let doc = "election management tool" in
  let man = common_man in
  Term.(ret (const (`Help (`Pager, None)))),
  Cmd.info "belenios-tool" ~version ~doc ~man

let root_cmd =
  let default, i = default_cmd in
  Cmd.(group ~default i cmds)

let () =
  exit (Cmd.eval root_cmd)
