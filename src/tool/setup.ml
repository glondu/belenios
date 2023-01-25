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

open Belenios
open Belenios_platform.Platform
open Belenios_core.Common
open Belenios_core.Signatures
open Belenios_core.Serializable_j
open Belenios_tool_common
open Common
open Cmdliner

let group_t =
  let doc = "Use group $(docv)." in
  Arg.(value & opt (some string) None & info ["group"] ~docv:"GROUP" ~doc)

let version_t =
  let doc = "Use protocol version $(docv)." in
  Arg.(value & opt int (List.hd supported_crypto_versions) & info ["protocol-version"] ~docv:"VERSION" ~doc)

let uuid_t =
  let doc = "UUID of the election." in
  Arg.(value & opt (some string) None & info ["uuid"] ~docv:"UUID" ~doc)

module Tkeygen : CMDLINER_MODULE = struct
  open Tool_tkeygen

  let main group version =
    let@ () = wrap_main in
    let module P = struct
        let group = get_mandatory_opt "--group" group
        let version = version
      end in
    let module R = Make (P) (Random) () in
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

  let cmd =
    let doc = "generate a trustee key" in
    let man = [
      `S "DESCRIPTION";
      `P "This command is run by a trustee to generate a share of an election key. Such a share consists of a private key and a public key with a certificate. Generated files are stored in the current directory with a name that starts with $(i,ID), where $(i,ID) is a short fingerprint of the public key. The private key is stored in $(i,ID.privkey) and must be secured by the trustee. The public key is stored in $(i,ID.pubkey) and must be sent to the election administrator.";
    ] @ common_man in
    Cmd.v (Cmd.info "generate-trustee-key" ~doc ~man)
      Term.(ret (const main $ group_t $ version_t))

end

module Ttkeygen : CMDLINER_MODULE = struct

  let main group version step certs threshold key polynomials =
    let@ () = wrap_main in
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
    let group = get_mandatory_opt "--group" group in
    let module G = (val Group.of_string ~version group : GROUP) in
    let module Trustees = (val Trustees.get_by_version version) in
    let module P = Trustees.MakePKI (G) (Random) in
    let module C = Trustees.MakeChannels (G) (Random) (P) in
    let module T = Trustees.MakePedersen (G) (Random) (P) (C) in
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
       Printf.printf "%s\n%!" (string_of_voutput (swrite G.to_string) voutput)
    | 6 ->
       let certs = get_certs () in
       let n = Array.length certs.certs in
       let polynomials = get_polynomials () in
       assert (n = Array.length polynomials);
       let voutputs = lines_of_stdin ()
                      |> List.map (voutput_of_string (sread G.of_string))
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
       Printf.printf "%s\n%!" (string_of_threshold_parameters (swrite G.to_string) tparams)
    | _ -> failwith "invalid step"

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

  let cmd =
    let doc = "generate a trustee key usable with threshold decryption" in
    let man = [
        `S "DESCRIPTION";
        `P "This command is run by trustees and the administrator to generate an election key with threshold decryption.";
      ] @ common_man in
    Cmd.v (Cmd.info "generate-trustee-key-threshold" ~doc ~man)
      Term.(ret (const main $ group_t $ version_t $ step_t $ cert_t $ threshold_t $ key_t $ polynomials_t))

end

module Credgen : CMDLINER_MODULE = struct
  open Tool_credgen

  let params_priv = "private credentials with ids", ".privcreds", 0o400
  let params_pub = "public credentials", ".pubcreds", 0o444

  let save (info, ext, perm) basename f =
    let fname = basename ^ ext in
    let oc = open_out_gen [Open_wronly; Open_creat; Open_excl] perm fname in
    let count = f oc in
    close_out oc;
    Printf.printf "%d %s saved to %s\n%!" count info fname

  let as_lines things oc =
    let count = ref 0 in
    List.iter
      (fun x ->
        incr count;
        output_string oc x;
        output_string oc "\n";
      ) things;
    !count

  let as_public_credentials things oc =
    output_string oc (string_of_public_credentials things);
    List.length things

  let main version group dir uuid count file derive =
    let@ () = wrap_main in
    let module P = struct
        let version = version
        let group = get_mandatory_opt "--group" group
        let uuid = get_mandatory_opt "--uuid" uuid
      end in
    let module R = Make (P) (Random) () in
    let action =
      match count, file, derive with
      | Some n, None, None ->
         if n < 1 then (
           failcmd "the argument of --count must be a positive number"
         ) else `Generate (generate_ids n)
      | None, Some f, None -> `Generate (string_of_file f |> Voter.list_of_string)
      | None, None, Some c -> `Derive c
      | _, _, _ ->
         failcmd "--count, --file and --derive are mutually exclusive"
    in
    match action with
    | `Derive c ->
       print_endline (R.derive c)
    | `Generate ids ->
       let c = R.generate ids in
       let timestamp = Printf.sprintf "%.0f" (Unix.time ()) in
       let base = dir // timestamp in
       save params_priv base (as_lines c.priv);
       save params_pub base (as_public_credentials c.public_with_ids);
       let h = sha256_b64 (string_of_public_credentials c.public) in
       Printf.printf "The fingerprint of public credentials is %s\n%!" h

  let count_t =
    let doc = "Generate $(docv) credentials." in
    let the_info = Arg.info ["count"] ~docv:"N" ~doc in
    Arg.(value & opt (some int) None the_info)

  let file_t =
    let doc = "Read identities from $(docv). One credential will be generated for each line of $(docv)." in
    let the_info = Arg.info ["file"] ~docv:"FILE" ~doc in
    Arg.(value & opt (some file) None the_info)

  let derive_t =
    let doc = "Derive the public key associated to a specific $(docv)." in
    let the_info = Arg.info ["derive"] ~docv:"PRIVATE_CRED" ~doc in
    Arg.(value & opt (some string) None the_info)

  let cmd =
    let doc = "generate credentials" in
    let man = [
      `S "DESCRIPTION";
      `P "This command is run by a credential authority to generate credentials for a specific election. The generated private credentials are stored in $(i,T.privcreds), where $(i,T) is a timestamp. $(i,T.privcreds) contains one credential per line. Each voter must be sent a credential, and $(i,T.privcreds) must be destroyed after dispatching is done. The associated public keys are stored in $(i,T.pubcreds) and must be sent to the election administrator.";
    ] @ common_man in
    Cmd.v (Cmd.info "generate-credentials" ~doc ~man)
      Term.(ret (const main $ version_t $ group_t $ dir_t $ uuid_t $ count_t $ file_t $ derive_t))

end

module Mktrustees : CMDLINER_MODULE = struct
  let main dir =
    let@ () = wrap_main in
    let get_public_keys () =
      Some (lines_of_file (dir // "public_keys.jsons"))
    in
    let get_threshold () =
      let fn = dir // "threshold.json" in
      if Sys.file_exists fn then Some (string_of_file fn) else None
    in
    let get_trustees () =
      let singles =
        match get_public_keys () with
        | None -> []
        | Some t ->
           t
           |> List.map (trustee_public_key_of_string Yojson.Safe.read_json)
           |> List.map (fun x -> `Single x)
      in
      let pedersens =
        match get_threshold () with
        | None -> []
        | Some t ->
           t
           |> threshold_parameters_of_string Yojson.Safe.read_json
           |> (fun x -> [`Pedersen x])
      in
      match singles @ pedersens with
      | [] -> failwith "trustees are missing"
      | trustees -> string_of_trustees Yojson.Safe.write_json trustees
    in
    let trustees = get_trustees () in
    let oc = open_out (dir // "trustees.json") in
    output_string oc trustees;
    output_char oc '\n';
    close_out oc

  let cmd =
    let doc = "create a trustee parameter file" in
    let man = [
      `S "DESCRIPTION";
      `P "This command reads $(i,public_keys.jsons) and $(i,threshold.json) (if any). It then generates an $(i,trustees.json) file.";
    ] @ common_man in
    Cmd.v (Cmd.info "make-trustees" ~doc ~man)
      Term.(ret (const main $ dir_t))

end

module Mkelection : CMDLINER_MODULE = struct
  open Tool_mkelection

  let main dir group version uuid template =
    let@ () = wrap_main in
    let module P = struct
        let version = version
        let group = get_mandatory_opt "--group" group
        let uuid = get_mandatory_opt "--uuid" uuid
        let template = get_mandatory_opt "--template" template |> string_of_file
        let get_trustees () =
          let fn = dir // "trustees.json" in
          if Sys.file_exists fn then
            string_of_file fn
          else
            failwith "trustees are missing"
      end in
    let module R = (val make (module P : PARAMS) : S) in
    let params = R.mkelection () in
    let oc = open_out (dir // "election.json") in
    output_string oc params;
    output_char oc '\n';
    close_out oc

  let template_t =
    let doc = "Read election template from file $(docv)." in
    Arg.(value & opt (some file) None & info ["template"] ~docv:"TEMPLATE" ~doc)

  let cmd =
    let doc = "create an election public parameter file" in
    let man = [
      `S "DESCRIPTION";
      `P "This command reads and checks $(i,public_keys.jsons) (or $(i,threshold.json) if it exists). It then computes the global election public key and generates an $(i,election.json) file.";
    ] @ common_man in
    Cmd.v (Cmd.info "make-election" ~doc ~man)
      Term.(ret (const main $ dir_t $ group_t $ version_t $ uuid_t $ template_t))

end

module GenerateToken : CMDLINER_MODULE = struct

  let main length =
    let@ () = wrap_main in
    let module X = MakeGenerateToken (Random) in
    X.generate_token ~length ()
    |> print_endline

  let length_t =
    let doc = "Token length." in
    Arg.(value & opt int 14 & info ["length"] ~docv:"L" ~doc)

  let cmd =
    let doc = "generate a token" in
    let man = [
        `S "DESCRIPTION";
        `P "This command generates a random token suitable for an election identifier.";
      ] @ common_man
    in
    Cmd.v (Cmd.info "generate-token" ~doc ~man)
      Term.(ret (const main $ length_t))

end

let cmd =
  let doc = "election setup commands" in
  let man = common_man in
  let info = Cmd.info "setup" ~doc ~man in
  let cmds =
    [
      Tkeygen.cmd;
      Ttkeygen.cmd;
      Credgen.cmd;
      Mktrustees.cmd;
      Mkelection.cmd;
      GenerateToken.cmd;
    ]
  in
  Cmd.group info cmds
