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

open Serializable_j
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

let string_of_file f =
  lines_of_file f |> stream_to_list |> String.concat "\n"

let load_from_file of_string filename =
  if Sys.file_exists filename then (
    Printf.eprintf "I: loading %s...\n%!" (Filename.basename filename);
    Some (lines_of_file filename |> stream_to_list |> List.rev_map of_string)
  ) else None

let ( / ) = Filename.concat

exception Cmdline_error of string

let failcmd fmt = Printf.ksprintf (fun x -> raise (Cmdline_error x)) fmt

let common_man = [
  `S "MORE INFORMATION";
  `P "This command is part of the Belenios command-line tool.";
  `P "To get more help on a specific subcommand, run:";
  `P "$(b,belenios-tool) $(i,COMMAND) $(b,--help)";
  `P "See $(i,http://belenios.gforge.inria.fr/).";
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

let dir_t =
  let doc = "Use directory $(docv) for reading of writing election files." in
  let the_info = Arg.info ["dir"] ~docv:"DIR" ~doc in
  Arg.(value & opt dir Filename.current_dir_name the_info)

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

module Election : CMDLINER_MODULE = struct
  open Tool_election

  module MakeGetters (X : sig val dir : string end) = struct

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
      | Some [r] -> Some r
      | _ -> failwith "invalid result"

    let print_msg = prerr_endline

  end

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
      let module X = (val make (module P : PARAMS) : S) in
      match action with
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
      | `Verify -> X.verify ()
      | `Finalize ->
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
      main d (`Vote (p, b))
    ) in
    Term.(ret (main $ dir_t $ privcred_t $ ballot_t)),
    Term.info "vote" ~doc ~man

  let verify_cmd =
    let doc = "verify election data" in
    let man = [
      `S "DESCRIPTION";
      `P "This command performs all possible verifications.";
    ] @ common_man in
    Term.(ret (pure main $ dir_t $ pure `Verify)),
    Term.info "verify" ~doc ~man

  let decrypt_cmd =
    let doc = "perform partial decryption" in
    let man = [
      `S "DESCRIPTION";
      `P "This command is run by each trustee to perform a partial decryption.";
    ] @ common_man in
    let main = Term.pure (fun d p ->
      let p = get_mandatory_opt "--privkey" p in
      main d (`Decrypt p)
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
    Term.(ret (pure main $ dir_t $ pure `Finalize)),
    Term.info "finalize" ~doc ~man

  let cmds = [vote_cmd; verify_cmd; decrypt_cmd; finalize_cmd]

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
      `P "This command reads and checks $(i,public_keys.jsons). It then computes the global election public key and generates an $(i,election.json) file.";
    ] @ common_man in
    Term.(ret (pure main $ dir_t $ group_t $ uuid_t $ template_t)),
    Term.info "mkelection" ~doc ~man

  let cmds = [mkelection_cmd]

end

let cmds = Tkeygen.cmds @ Election.cmds @ Credgen.cmds @ Mkelection.cmds

let default_cmd =
  let version = Belenios_version.(Printf.sprintf "%s (%s)" version build) in
  let doc = "election management tool" in
  let man = common_man in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "belenios-tool" ~version ~doc ~man

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
