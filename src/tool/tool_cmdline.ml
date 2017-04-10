(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Serializable_j
open Platform
open Common
open Cmdliner

let stream_to_list s =
  let res = ref [] in
  Stream.iter (fun x -> res := x :: !res) s;
  List.rev !res

let lines_of_file fname =
  let ic = open_in fname in
  Stream.from (fun _ ->
    try Some (input_line ic)
    with End_of_file -> close_in ic; None
  )

let lines_of_stdin () =
  Stream.from (fun _ ->
      try Some (input_line stdin)
      with End_of_file -> None
    )

let string_of_file f =
  lines_of_file f |> stream_to_list |> String.concat "\n"

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Printf.eprintf "I: loading %s...\n%!" (Filename.basename filename);
    Some (lines_of_file filename |> stream_to_list |> List.rev_map of_string)
  ) else None

let ( / ) = Filename.concat

let download dir url file =
  let url = if url.[String.length url - 1] = '/' then url else url ^ "/" in
  Printf.eprintf "I: downloading %s...\n%!" file;
  let target = dir / file in
  let command =
    Printf.sprintf "curl --silent --fail \"%s%s\" > \"%s\"" url file target
  in
  let r = Sys.command command in
  if r <> 0 then (Sys.remove target; false) else true

let rm_rf dir =
  let files = Sys.readdir dir in
  Array.iter (fun f -> Unix.unlink (dir / f)) files;
  Unix.rmdir dir

exception Cmdline_error of string

let failcmd fmt = Printf.ksprintf (fun x -> raise (Cmdline_error x)) fmt

let common_man = [
  `S "MORE INFORMATION";
  `P "This command is part of the Belenios command-line tool.";
  `P "To get more help on a specific subcommand, run:";
  `P "$(b,belenios-tool) $(i,COMMAND) $(b,--help)";
  `P "See $(i,http://www.belenios.org/).";
]

let get_mandatory_opt name = function
  | Some x -> x
  | None -> failcmd "%s is mandatory" name

let wrap_main f =
  try
    let () = f () in `Ok ()
  with
  | Cmdline_error e -> `Error (true, e)
  | Failure e -> `Error (false, e)
  | e -> `Error (false, Printexc.to_string e)

module type CMDLINER_MODULE = sig
  val cmds : (unit Cmdliner.Term.t * Cmdliner.Term.info) list
end

let group_t =
  let doc = "Take group parameters from file $(docv)." in
  Arg.(value & opt (some file) None & info ["group"] ~docv:"GROUP" ~doc)

let uuid_t =
  let doc = "UUID of the election." in
  Arg.(value & opt (some string) None & info ["uuid"] ~docv:"UUID" ~doc)

let dir_t, optdir_t =
  let doc = "Use directory $(docv) for reading and writing election files." in
  let the_info = Arg.info ["dir"] ~docv:"DIR" ~doc in
  Arg.(value & opt dir Filename.current_dir_name the_info),
  Arg.(value & opt (some dir) None the_info)

let url_t =
  let doc = "Download election files from $(docv)." in
  let the_info = Arg.info ["url"] ~docv:"URL" ~doc in
  Arg.(value & opt (some string) None the_info)

let key_t =
  let doc = "Read private key from file $(docv)." in
  let the_info = Arg.info ["key"] ~docv:"KEY" ~doc in
  Arg.(value & opt (some file) None the_info)

module Tkeygen : CMDLINER_MODULE = struct
  open Tool_tkeygen

  let main group =
    wrap_main (fun () ->
      let module P = struct
        let group = get_mandatory_opt "--group" group |> string_of_file
      end in
      let module R = (val make (module P : PARAMS) : S) in
      let kp = R.trustee_keygen () in
      Printf.printf "I: keypair %s has been generated\n%!" kp.R.id;
      let pubkey = "public", kp.R.id ^ ".pubkey", 0o444, kp.R.pub in
      let privkey = "private", kp.R.id ^ ".privkey", 0o400, kp.R.priv in
      let save (kind, filename, perm, thing) =
        let oc = open_out_gen [Open_wronly; Open_creat] perm filename in
        output_string oc thing;
        output_char oc '\n';
        close_out oc;
        Printf.printf "I: %s key saved to %s\n%!" kind filename;
        (* set permissions in the unlikely case where the file already existed *)
        Unix.chmod filename perm
      in
      save pubkey;
      save privkey
    )

  let tkeygen_cmd =
    let doc = "generate a trustee key" in
    let man = [
      `S "DESCRIPTION";
      `P "This command is run by a trustee to generate a share of an election key. Such a share consists of a private key and a public key with a certificate. Generated files are stored in the current directory with a name that starts with $(i,ID), where $(i,ID) is a short fingerprint of the public key. The private key is stored in $(i,ID.privkey) and must be secured by the trustee. The public key is stored in $(i,ID.pubkey) and must be sent to the election administrator.";
    ] @ common_man in
    Term.(ret (pure main $ group_t)),
    Term.info "trustee-keygen" ~doc ~man

  let cmds = [tkeygen_cmd]

end

module Ttkeygen : CMDLINER_MODULE = struct

  let main group step certs threshold key polynomials =
    wrap_main (fun () ->
        let get_certs () =
          let certs = get_mandatory_opt "--certs" certs in
          match load_from_file cert_of_string certs with
          | None -> Printf.ksprintf failwith "%s does not exist" certs
          | Some l -> { certs = Array.of_list (List.rev l) }
        in
        let get_polynomials () =
          let polynomials = get_mandatory_opt "--polynomials" polynomials in
          match load_from_file polynomial_of_string polynomials with
          | None -> Printf.ksprintf failwith "%s does not exist" polynomials
          | Some l -> Array.of_list (List.rev l)
        in
        let group = get_mandatory_opt "--group" group |> string_of_file in
        let module G = (val Group.of_string group : GROUP) in
        let module P = Trustees.MakePKI (G) (DirectRandom) in
        let module C = Trustees.MakeChannels (G) (DirectRandom) (P) in
        let module T = Trustees.MakePedersen (G) (DirectRandom) (P) (C) in
        match step with
        | 1 ->
           let key, cert = T.step1 () in
           let id = sha256_hex cert.s_message in
           Printf.eprintf "I: certificate %s has been generated\n%!" id;
           let pub = "certificate", id ^ ".cert", 0o444, string_of_cert cert in
           let prv = "private key", id ^ ".key", 0o400, key in
           let save (descr, filename, perm, thing) =
             let oc = open_out_gen [Open_wronly; Open_creat] perm filename in
             output_string oc thing;
             output_char oc '\n';
             close_out oc;
             Printf.eprintf "I: %s saved to %s\n%!" descr filename;
             (* set permissions in the unlikely case where the file already existed *)
             Unix.chmod filename perm
           in
           save pub;
           save prv
        | 2 ->
           let certs = get_certs () in
           let () = T.step2 certs in
           Printf.eprintf "I: certificates are valid\n%!"
        | 3 ->
           let certs = get_certs () in
           let threshold = get_mandatory_opt "--threshold" threshold in
           let key = get_mandatory_opt "--key" key |> string_of_file in
           let polynomial = T.step3 certs key threshold in
           Printf.printf "%s\n%!" (string_of_polynomial polynomial)
        | 4 ->
           let certs = get_certs () in
           let n = Array.length certs.certs in
           let polynomials = get_polynomials () in
           assert (n = Array.length polynomials);
           let vinputs = T.step4 certs polynomials in
           assert (n = Array.length vinputs);
           for i = 0 to n - 1 do
             let id = sha256_hex certs.certs.(i).s_message in
             let fn = id ^ ".vinput" in
             let oc = open_out_gen [Open_wronly; Open_creat] 0o444 fn in
             output_string oc (string_of_vinput vinputs.(i));
             output_char oc '\n';
             close_out oc;
             Printf.eprintf "I: wrote %s\n%!" fn
           done
        | 5 ->
           let certs = get_certs () in
           let key = get_mandatory_opt "--key" key |> string_of_file in
           let vinput = read_line () |> vinput_of_string in
           let voutput = T.step5 certs key vinput in
           Printf.printf "%s\n%!" (string_of_voutput G.write voutput)
        | 6 ->
           let certs = get_certs () in
           let n = Array.length certs.certs in
           let polynomials = get_polynomials () in
           assert (n = Array.length polynomials);
           let voutputs = lines_of_stdin ()
                          |> stream_to_list
                          |> List.map (voutput_of_string G.read)
                          |> Array.of_list
           in
           assert (n = Array.length voutputs);
           let tparams = T.step6 certs polynomials voutputs in
           for i = 0 to n - 1 do
             let id = sha256_hex certs.certs.(i).s_message in
             let fn = id ^ ".dkey" in
             let oc = open_out_gen [Open_wronly; Open_creat] 0o400 fn in
             output_string oc voutputs.(i).vo_private_key;
             output_char oc '\n';
             close_out oc;
             Printf.eprintf "I: wrote %s\n%!" fn
           done;
           Printf.printf "%s\n%!" (string_of_threshold_parameters G.write tparams)
        | _ -> failwith "invalid step"
      )

  let step_t =
    let doc = "Step to execute." in
    let the_info = Arg.info ["step"] ~docv:"STEP" ~doc in
    Arg.(value & opt int 0 the_info)

  let cert_t =
    let doc = "Read certificates from file $(docv)." in
    let the_info = Arg.info ["certs"] ~docv:"CERTS" ~doc in
    Arg.(value & opt (some file) None the_info)

  let threshold_t =
    let doc = "Threshold of trustees needed to decrypt." in
    let the_info = Arg.info ["threshold"] ~docv:"THRESHOLD" ~doc in
    Arg.(value & opt (some int) None the_info)

  let polynomials_t =
    let doc = "Read polynomials (output of step 3) from file $(docv)." in
    let the_info = Arg.info ["polynomials"] ~docv:"POLYNOMIALS" ~doc in
    Arg.(value & opt (some file) None the_info)

  let ttkeygen_cmd =
    let doc = "generate a trustee key usable with threshold decryption" in
    let man = [
        `S "DESCRIPTION";
        `P "This command is run by trustees and the administrator to generate an election key with threshold decryption.";
      ] @ common_man in
    Term.(ret (pure main $ group_t $ step_t $ cert_t $ threshold_t $ key_t $ polynomials_t)),
    Term.info "threshold-trustee-keygen" ~doc ~man

  let cmds = [ttkeygen_cmd]

end

module Election : CMDLINER_MODULE = struct
  open Tool_election

  module MakeGetters (X : sig val dir : string end) = struct

    let get_threshold () =
      let file = "threshold.json" in
      Printf.eprintf "I: loading %s...\n%!" file;
      try Some (string_of_file (X.dir / file))
      with _ -> None

    let get_public_keys () =
      load_from_file (fun x -> x) (X.dir/"public_keys.jsons") |>
      option_map Array.of_list

    let get_public_creds () =
      let file = "public_creds.txt" in
      Printf.eprintf "I: loading %s...\n%!" file;
      try Some (lines_of_file (X.dir / file))
      with _ -> None

    let get_ballots () =
      let file = "ballots.jsons" in
      Printf.eprintf "I: loading %s...\n%!" file;
      try Some (lines_of_file (X.dir / file))
      with _ -> None

    let get_result () =
      load_from_file (fun x -> x) (X.dir/"result.json") |> function
      | None -> None
      | Some [r] -> Some r
      | _ -> failwith "invalid result"

    let print_msg = prerr_endline

  end

  let main url dir action =
    wrap_main (fun () ->
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
      let () =
        match url with
        | None -> ()
        | Some u ->
           if not (
             download dir u "election.json" &&
             download dir u "public_keys.jsons" &&
             download dir u "public_creds.txt" &&
             download dir u "ballots.jsons" &&
             download dir u "result.json"
           ) then
             Printf.eprintf "W: some errors occurred while downloading\n%!";
      in
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
      let module X = (val make (module P : PARAMS) : S) in
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
      | `Decrypt privkey ->
        let privkey =
          match load_from_file (fun x -> x) privkey with
          | Some [privkey] -> privkey
          | _ -> failwith "invalid private key"
        in
        print_endline (X.decrypt privkey)
      | `TDecrypt (key, pdk) ->
         let key = string_of_file key in
         let pdk = string_of_file pdk in
         print_endline (X.tdecrypt key pdk)
      | `Verify -> X.verify ()
      | `Finalize ->
        let factors =
          let fname = dir/"partial_decryptions.jsons" in
          match load_from_file (fun x -> x) fname with
          | Some factors -> factors
          | None -> failwith "cannot load partial decryptions"
        in
        let oc = open_out (dir/"result.json") in
        output_string oc (X.finalize factors);
        output_char oc '\n';
        close_out oc
      end;
      if cleanup then rm_rf dir
    )

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

  let vote_cmd =
    let doc = "create a ballot" in
    let man = [
      `S "DESCRIPTION";
      `P "This command creates a ballot and prints it on standard output.";
    ] @ common_man in
    let main = Term.pure (fun u d p b ->
      let p = get_mandatory_opt "--privcred" p in
      let b = get_mandatory_opt "--ballot" b in
      main u d (`Vote (p, b))
    ) in
    Term.(ret (main $ url_t $ optdir_t $ privcred_t $ ballot_t)),
    Term.info "vote" ~doc ~man

  let verify_cmd =
    let doc = "verify election data" in
    let man = [
      `S "DESCRIPTION";
      `P "This command performs all possible verifications.";
    ] @ common_man in
    Term.(ret (pure main $ url_t $ optdir_t $ pure `Verify)),
    Term.info "verify" ~doc ~man

  let decrypt_man = [
      `S "DESCRIPTION";
      `P "This command is run by each trustee to perform a partial decryption.";
    ] @ common_man

  let decrypt_cmd =
    let doc = "perform partial decryption" in
    let main = Term.pure (fun u d p ->
      let p = get_mandatory_opt "--privkey" p in
      main u d (`Decrypt p)
    ) in
    Term.(ret (main $ url_t $ optdir_t $ privkey_t)),
    Term.info "decrypt" ~doc ~man:decrypt_man

  let tdecrypt_cmd =
    let doc = "perform partial decryption (threshold version)" in
    let main = Term.pure (fun u d k pdk ->
                   let k = get_mandatory_opt "--key" k in
                   let pdk = get_mandatory_opt "--decryption-key" pdk in
                   main u d (`TDecrypt (k, pdk))
                 )
    in
    Term.(ret (main $ url_t $ optdir_t $ key_t $ pdk_t)),
    Term.info "threshold-decrypt" ~doc ~man:decrypt_man

  let finalize_cmd =
    let doc = "finalizes an election" in
    let man = [
      `S "DESCRIPTION";
      `P "This command reads partial decryptions done by trustees from file $(i,partial_decryptions.jsons), checks them, combines them into the final tally and prints the result to standard output.";
      `P "The result structure contains partial decryptions itself, so $(i,partial_decryptions.jsons) can be discarded afterwards.";
    ] @ common_man in
    Term.(ret (pure main $ url_t $ optdir_t $ pure `Finalize)),
    Term.info "finalize" ~doc ~man

  let cmds = [vote_cmd; verify_cmd; decrypt_cmd; tdecrypt_cmd; finalize_cmd]

end

module Credgen : CMDLINER_MODULE = struct
  open Tool_credgen

  let params_priv = "private credentials with ids", ".privcreds", 0o400
  let params_pub = "public credentials", ".pubcreds", 0o444
  let params_hash = "hashed public credentials with ids", ".hashcreds", 0o400

  let save (info, ext, perm) basename things =
    let fname = basename ^ ext in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_excl] perm fname in
    let count = ref 0 in
    List.iter (fun x ->
      incr count;
      output_string oc x;
      output_string oc "\n";
    ) things;
    close_out oc;
    Printf.printf "%d %s saved to %s\n%!" !count info fname

  let main group dir uuid count file derive =
    wrap_main (fun () ->
      let module P = struct
        let group = get_mandatory_opt "--group" group |> string_of_file
        let uuid = get_mandatory_opt "--uuid" uuid
      end in
      let module R = (val make (module P : PARAMS) : S) in
      let action =
        match count, file, derive with
        | Some n, None, None ->
          if n < 1 then (
            failcmd "the argument of --count must be a positive number"
          ) else `Generate (generate_ids n)
        | None, Some f, None -> `Generate (lines_of_file f |> stream_to_list)
        | None, None, Some c -> `Derive c
        | _, _, _ ->
          failcmd "--count, --file and --derive are mutually exclusive"
      in
      match action with
      | `Derive c ->
        print_endline (R.derive c)
      | `Generate ids ->
        let privs, pubs, hashs =
          List.fold_left (fun (privs, pubs, hashs) id ->
            let priv, pub, hash = R.generate () in
            let priv = id ^ " " ^ priv and hash = id ^ " " ^ hash in
            priv::privs, pub::pubs, hash::hashs
          ) ([], [], []) ids
        in
        let timestamp = Printf.sprintf "%.0f" (Unix.time ()) in
        let base = dir / timestamp in
        save params_priv base (List.rev privs);
        save params_pub base (List.sort compare pubs);
        save params_hash base (List.rev hashs)
    )

  let count_t =
    let doc = "Generate $(docv) credentials." in
    let the_info = Arg.info ["count"] ~docv:"N" ~doc in
    Arg.(value & opt (some int) None the_info)

  let file_t =
    let doc = "Read identities from $(docv) and generate an additional $(i,T.hashcreds) with identities associated with hashed public credentials. These hashed public credentials are used by the hotline to update a public credential on the web server. One credential will be generated for each line of $(docv)." in
    let the_info = Arg.info ["file"] ~docv:"FILE" ~doc in
    Arg.(value & opt (some file) None the_info)

  let derive_t =
    let doc = "Derive the public key associated to a specific $(docv)." in
    let the_info = Arg.info ["derive"] ~docv:"PRIVATE_CRED" ~doc in
    Arg.(value & opt (some string) None the_info)

  let credgen_cmd =
    let doc = "generate credentials" in
    let man = [
      `S "DESCRIPTION";
      `P "This command is run by a credential authority to generate credentials for a specific election. The generated private credentials are stored in $(i,T.privcreds), where $(i,T) is a timestamp. $(i,T.privcreds) contains one credential per line. Each voter must be sent a credential, and $(i,T.privcreds) must be destroyed after dispatching is done. The associated public keys are stored in $(i,T.pubcreds) and must be sent to the election administrator.";
    ] @ common_man in
    Term.(ret (pure main $ group_t $ dir_t $ uuid_t $ count_t $ file_t $ derive_t)),
    Term.info "credgen" ~doc ~man

  let cmds = [credgen_cmd]

end

module Mkelection : CMDLINER_MODULE = struct
  open Tool_mkelection

  let main dir group uuid template =
    wrap_main (fun () ->
      let module P = struct
        let group = get_mandatory_opt "--group" group |> string_of_file
        let uuid = get_mandatory_opt "--uuid" uuid
        let template = get_mandatory_opt "--template" template |> string_of_file
        let get_public_keys () =
          Some (lines_of_file (dir / "public_keys.jsons") |> stream_to_list |> Array.of_list)
        let get_threshold () =
          let fn = dir / "threshold.json" in
          if Sys.file_exists fn then Some (string_of_file fn) else None
      end in
      let module R = (val make (module P : PARAMS) : S) in
      let params = R.mkelection () in
      let oc = open_out (dir / "election.json") in
      output_string oc params;
      output_char oc '\n';
      close_out oc
    )

  let template_t =
    let doc = "Read election template from file $(docv)." in
    Arg.(value & opt (some file) None & info ["template"] ~docv:"TEMPLATE" ~doc)

  let mkelection_cmd =
    let doc = "create an election public parameter file" in
    let man = [
      `S "DESCRIPTION";
      `P "This command reads and checks $(i,public_keys.jsons) (or $(i,threshold.json) if it exists). It then computes the global election public key and generates an $(i,election.json) file.";
    ] @ common_man in
    Term.(ret (pure main $ dir_t $ group_t $ uuid_t $ template_t)),
    Term.info "mkelection" ~doc ~man

  let cmds = [mkelection_cmd]

end

module Verifydiff : CMDLINER_MODULE = struct
  open Tool_verifydiff

  let main dir1 dir2 =
    wrap_main (fun () ->
        match dir1, dir2 with
        | Some dir1, Some dir2 -> verifydiff dir1 dir2
        | _, _ -> failcmd "--dir1 or --dir2 is missing"
      )

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
    Term.(ret (pure main $ dir1_t $ dir2_t)),
    Term.info "verify-diff" ~doc ~man

  let cmds = [verifydiff_cmd]

end

let cmds = Tkeygen.cmds @ Ttkeygen.cmds @ Election.cmds @ Credgen.cmds @ Mkelection.cmds @ Verifydiff.cmds

let default_cmd =
  let open Belenios_version in
  let version = Printf.sprintf "%s (%s)" version build in
  let version = if debug then version ^ " [debug]" else version in
  let doc = "election management tool" in
  let man = common_man in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "belenios-tool" ~version ~doc ~man

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
