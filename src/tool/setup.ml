(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Lwt.Syntax
open Belenios
open Common
open Cmdliner

let group_t =
  let doc = "Use group $(docv)." in
  Arg.(value & opt (some string) None & info [ "group" ] ~docv:"GROUP" ~doc)

let version_t =
  let doc = "Use protocol version $(docv)." in
  Arg.(
    value & opt int default_version
    & info [ "protocol-version" ] ~docv:"VERSION" ~doc)

let uuid_t =
  let doc = "UUID of the election." in
  Arg.(value & opt (some string) None & info [ "uuid" ] ~docv:"UUID" ~doc)

let save (descr, filename, perm, thing) =
  let* () =
    let open Lwt_io in
    let@ oc =
      with_file ~flags:[ O_WRONLY; O_CREAT ] ~perm ~mode:Output filename
    in
    write_line oc thing
  in
  let* () = Lwt_io.eprintlf "I: %s saved to %s" descr filename in
  (* set permissions in the unlikely case where the file already existed *)
  Lwt_unix.chmod filename perm

module Tkeygen : CMDLINER_MODULE = struct
  let main group version =
    let@ () = wrap_main in
    let group = get_mandatory_opt "--group" group in
    let module G = (val Group.of_string ~version group) in
    let module Trustees = (val Trustees.get_by_version version) in
    let module KG = Trustees.MakeSimple (G) (Random) in
    let private_key = KG.generate () in
    let public_key = KG.prove private_key in
    let id =
      String.sub (sha256_hex (G.to_string public_key.trustee_public_key)) 0 8
      |> String.uppercase_ascii
    in
    let priv = string_of_number @@ G.Zq.to_Z private_key in
    let pub =
      string_of_trustee_public_key (swrite G.to_string) (swrite G.Zq.to_string)
        public_key
    in
    Printf.printf "I: keypair %s has been generated\n%!" id;
    let pubkey = ("public key", id ^ ".pubkey", 0o444, pub) in
    let privkey = ("private key", id ^ ".privkey", 0o400, priv) in
    let* () = save pubkey in
    let* () = save privkey in
    Lwt.return_unit

  let cmd =
    let doc = "generate a trustee key" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command is run by a trustee to generate a share of an election \
           key. Such a share consists of a private key and a public key with a \
           certificate. Generated files are stored in the current directory \
           with a name that starts with $(i,ID), where $(i,ID) is a short \
           fingerprint of the public key. The private key is stored in \
           $(i,ID.privkey) and must be secured by the trustee. The public key \
           is stored in $(i,ID.pubkey) and must be sent to the election \
           administrator.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "generate-trustee-key" ~doc ~man)
      Term.(ret (const main $ group_t $ version_t))
end

module Ttkeygen : CMDLINER_MODULE = struct
  let main group version step certs context key polynomials =
    let@ () = wrap_main in
    let group = get_mandatory_opt "--group" group in
    let get_context () =
      let x = get_mandatory_opt "--threshold-context" context in
      try
        match String.split_on_char '/' x with
        | [ index; threshold; size ] ->
            let index = int_of_string index in
            let threshold = int_of_string threshold in
            let size = int_of_string size in
            if
              0 < size && 1 <= index && index <= size && 0 < threshold
              && threshold < size
            then { group; index; threshold; size }
            else raise Exit
        | _ -> raise Exit
      with _ -> failcmd "threshold context is invalid"
    in
    let module G = (val Group.of_string ~version group : GROUP) in
    let module Trustees = (val Trustees.get_by_version version) in
    let module P = Pki.Make (G) (Random) in
    let module C = Pki.MakeChannels (P) in
    let module T = Trustees.MakePedersen (C) in
    let get_certs () =
      let certs = get_mandatory_opt "--certs" certs in
      let* x = load_from_file (cert_of_string (sread G.Zq.of_string)) certs in
      match x with
      | None -> Printf.ksprintf failwith "%s does not exist" certs
      | Some l -> Lwt.return @@ Array.of_list l
    in
    let get_polynomials () =
      let polynomials = get_mandatory_opt "--polynomials" polynomials in
      let* x =
        load_from_file (polynomial_of_string (sread G.Zq.of_string)) polynomials
      in
      match x with
      | None -> Printf.ksprintf failwith "%s does not exist" polynomials
      | Some l -> Lwt.return @@ Array.of_list l
    in
    match step with
    | 1 ->
        let key, cert = T.step1 (get_context ()) in
        let id = sha256_hex cert.s_message in
        Printf.eprintf "I: certificate %s has been generated\n%!" id;
        let pub =
          ( "certificate",
            id ^ ".cert",
            0o444,
            string_of_cert (swrite G.Zq.to_string) cert )
        in
        let prv = ("private key", id ^ ".key", 0o400, key) in
        let* () = save pub in
        let* () = save prv in
        Lwt_io.printl id
    | 2 ->
        let* certs = get_certs () in
        let _ = T.step2 certs in
        Lwt_io.eprintl "I: certificates are valid"
    | 3 ->
        let* certs = get_certs () in
        let* key = get_mandatory_opt "--key" key |> string_of_file in
        let* polynomial = T.step3 certs key in
        Lwt_io.printl (string_of_polynomial (swrite G.Zq.to_string) polynomial)
    | 4 ->
        let* certs = get_certs () in
        let n = Array.length certs in
        let* polynomials = get_polynomials () in
        assert (n = Array.length polynomials);
        let vinputs = T.step4 certs polynomials in
        assert (n = Array.length vinputs);
        let rec loop i =
          if i < n then
            let id = sha256_hex certs.(i).s_message in
            let fn = id ^ ".vinput" in
            let* () =
              let open Lwt_io in
              let@ oc =
                with_file ~flags:[ O_WRONLY; O_CREAT ] ~perm:0o444 ~mode:Output
                  fn
              in
              write_line oc
                (string_of_vinput (swrite G.Zq.to_string) vinputs.(i))
            in
            let* () = Lwt_io.eprintlf "I: wrote %s" fn in
            loop (i + 1)
          else Lwt.return_unit
        in
        loop 0
    | 5 ->
        let* certs = get_certs () in
        let* key = get_mandatory_opt "--key" key |> string_of_file in
        let vinput = read_line () |> vinput_of_string (sread G.Zq.of_string) in
        let* voutput = T.step5 certs key vinput in
        Lwt_io.printl
          (string_of_voutput (swrite G.to_string) (swrite G.Zq.to_string)
             voutput)
    | 6 ->
        let* certs = get_certs () in
        let n = Array.length certs in
        let* polynomials = get_polynomials () in
        assert (n = Array.length polynomials);
        let* lines = lines_of_stdin () in
        let voutputs =
          lines
          |> List.map
               (voutput_of_string (sread G.of_string) (sread G.Zq.of_string))
          |> Array.of_list
        in
        assert (n = Array.length voutputs);
        let tparams = T.step6 certs polynomials voutputs in
        let rec loop i =
          if i < n then
            let id = sha256_hex certs.(i).s_message in
            let fn = id ^ ".dkey" in
            let* () =
              let open Lwt_io in
              let@ oc =
                with_file ~flags:[ O_WRONLY; O_CREAT ] ~perm:0o400 ~mode:Output
                  fn
              in
              write_line oc voutputs.(i).vo_private_key
            in
            let* () = Lwt_io.eprintlf "I: wrote %s" fn in
            loop (i + 1)
          else Lwt.return_unit
        in
        let* () = loop 0 in
        Lwt_io.printl
          (string_of_threshold_parameters (swrite G.to_string)
             (swrite G.Zq.to_string) tparams)
    | _ -> failwith "invalid step"

  let step_t =
    let doc = "Step to execute." in
    let the_info = Arg.info [ "step" ] ~docv:"STEP" ~doc in
    Arg.(value & opt int 0 the_info)

  let cert_t =
    let doc = "Read certificates from file $(docv)." in
    let the_info = Arg.info [ "certs" ] ~docv:"CERTS" ~doc in
    Arg.(value & opt (some file) None the_info)

  let context_t =
    let doc = "Context for threshold protocol." in
    let the_info = Arg.info [ "threshold-context" ] ~docv:"i/k/n" ~doc in
    Arg.(value & opt (some string) None the_info)

  let polynomials_t =
    let doc = "Read polynomials (output of step 3) from file $(docv)." in
    let the_info = Arg.info [ "polynomials" ] ~docv:"POLYNOMIALS" ~doc in
    Arg.(value & opt (some file) None the_info)

  let cmd =
    let doc = "generate a trustee key usable with threshold decryption" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command is run by trustees and the administrator to generate \
           an election key with threshold decryption.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "generate-trustee-key-threshold" ~doc ~man)
      Term.(
        ret
          (const main $ group_t $ version_t $ step_t $ cert_t $ context_t
         $ key_t $ polynomials_t))
end

module Credgen : CMDLINER_MODULE = struct
  let params_priv = ("private credentials with ids", ".privcreds", 0o400)
  let params_pub = ("public credentials", ".pubcreds", 0o444)

  let save (info, ext, perm) basename f =
    let fname = basename ^ ext in
    let* count =
      Lwt_io.with_file
        ~flags:[ O_WRONLY; O_CREAT; O_EXCL ]
        ~perm ~mode:Output fname f
    in
    Lwt_io.printlf "%d %s saved to %s" count info fname

  let as_json to_string things oc =
    let* () = Lwt_io.write oc (to_string things) in
    Lwt.return @@ List.length things

  let main version group dir uuid count jobs file derive =
    let@ () = wrap_main in
    let group = get_mandatory_opt "--group" group in
    let uuid = get_mandatory_opt "--uuid" uuid |> Uuid.wrap in
    let () = if jobs <= 0 then failcmd "--jobs must be positive" in
    let* action =
      match (count, file, derive) with
      | Some n, None, None ->
          if n < 1 then
            failcmd "the argument of --count must be a positive number"
          else Lwt.return @@ `Generate (Voter.generate n)
      | None, Some f, None ->
          let* x = string_of_file f in
          Lwt.return @@ `Generate (x |> Voter.list_of_string)
      | None, None, Some c -> Lwt.return @@ `Derive c
      | _, _, _ -> failcmd "--count, --file and --derive are mutually exclusive"
    in
    let module G = (val Group.of_string ~version group : GROUP) in
    let module Cred =
      Credential.Make
        (G)
        (struct
          type 'a t = 'a

          let return = Fun.id
          let bind x f = f x
          let pause () = ()
          let uuid = uuid
        end)
    in
    let save (c : Credential.batch) =
      let timestamp = Printf.sprintf "%.0f" (Unix.time ()) in
      let base = dir // timestamp in
      let* () =
        save params_priv base
          (as_json string_of_private_credentials c.private_creds)
      in
      let* () =
        save params_pub base
          (as_json string_of_public_credentials c.public_with_ids)
      in
      let h = sha256_b64 (string_of_public_credentials c.public_creds) in
      Lwt_io.printlf "The fingerprint of public credentials is %s" h
    in
    match action with
    | `Derive c -> (
        match Cred.derive c with
        | Ok x -> Lwt_io.printl G.(g **~ x |> to_string)
        | Error _ -> failcmd "invalid credential")
    | `Generate ids when jobs = 1 -> save (Cred.generate ids)
    | `Generate ids ->
        let n = (List.length ids / jobs) + 1 in
        let args =
          [|
            Sys.argv.(0);
            "setup";
            "generate-sub-credentials";
            Printf.sprintf "--protocol-version=%d" version;
            Printf.sprintf "--group=%s" group;
            Printf.sprintf "--uuid=%s" (Uuid.unwrap uuid);
            Printf.sprintf "--count=%d" n;
          |]
        in
        let rec open_processes jobs accu =
          if jobs > 0 then
            let ic = Unix.open_process_args_in Sys.executable_name args in
            open_processes (jobs - 1) (ic :: accu)
          else accu
        in
        let rec collect_processes accu = function
          | [] -> accu
          | ic :: ics -> (
              let line = input_line ic in
              match Unix.close_process_in ic with
              | Unix.WEXITED 0 ->
                  let sub = sub_batch_of_string line in
                  collect_processes (List.rev_append sub accu) ics
              | _ -> failcmd "generate-sub-credentials failed")
        in
        open_processes jobs [] |> collect_processes [] |> Cred.merge_sub ids
        |> save

  let count_t =
    let doc = "Generate $(docv) credentials." in
    let the_info = Arg.info [ "count" ] ~docv:"N" ~doc in
    Arg.(value & opt (some int) None the_info)

  let jobs_t =
    let doc = "Use $(docv) parallel jobs." in
    let the_info = Arg.info [ "jobs" ] ~docv:"JOBS" ~doc in
    Arg.(value & opt int 1 the_info)

  let file_t =
    let doc =
      "Read identities from $(docv). One credential will be generated for each \
       line of $(docv)."
    in
    let the_info = Arg.info [ "file" ] ~docv:"FILE" ~doc in
    Arg.(value & opt (some file) None the_info)

  let derive_t =
    let doc = "Derive the public key associated to a specific $(docv)." in
    let the_info = Arg.info [ "derive" ] ~docv:"PRIVATE_CRED" ~doc in
    Arg.(value & opt (some string) None the_info)

  let cmd =
    let doc = "generate credentials" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command is run by a credential authority to generate \
           credentials for a specific election. The generated private \
           credentials are stored in $(i,T.privcreds), where $(i,T) is a \
           timestamp. $(i,T.privcreds) contains one credential per line. Each \
           voter must be sent a credential, and $(i,T.privcreds) must be \
           destroyed after dispatching is done. The associated public keys are \
           stored in $(i,T.pubcreds) and must be sent to the election \
           administrator.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "generate-credentials" ~doc ~man)
      Term.(
        ret
          (const main $ version_t $ group_t $ dir_t $ uuid_t $ count_t $ jobs_t
         $ file_t $ derive_t))
end

module SubCredgen : CMDLINER_MODULE = struct
  let main version group uuid count =
    let@ () = wrap_main in
    let group = get_mandatory_opt "--group" group in
    let uuid = get_mandatory_opt "--uuid" uuid |> Uuid.wrap in
    let count = get_mandatory_opt "--count" count in
    let module G = (val Group.of_string ~version group : GROUP) in
    let module Cred =
      Credential.Make
        (G)
        (struct
          type 'a t = 'a

          let return = Fun.id
          let bind x f = f x
          let pause () = ()
          let uuid = uuid
        end)
    in
    let x, _ = Cred.generate_sub count in
    Lwt_io.printl (string_of_sub_batch x)

  let count_t =
    let doc = "Generate $(docv) credentials." in
    let the_info = Arg.info [ "count" ] ~docv:"N" ~doc in
    Arg.(value & opt (some int) None the_info)

  let cmd =
    let doc = "generate sub credentials" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command is an implementation detail and should not be run \
           directly.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "generate-sub-credentials" ~doc ~man)
      Term.(ret (const main $ version_t $ group_t $ uuid_t $ count_t))
end

module Mktrustees : CMDLINER_MODULE = struct
  let main dir =
    let@ () = wrap_main in
    let get_public_keys () =
      let* lines = lines_of_file (dir // "public_keys.jsons") in
      Lwt.return_some lines
    in
    let get_threshold () =
      let fn = dir // "threshold.json" in
      let* b = Lwt_unix.file_exists fn in
      if b then
        let* x = string_of_file fn in
        Lwt.return_some x
      else Lwt.return_none
    in
    let get_trustees () =
      let* singles =
        let* x = get_public_keys () in
        match x with
        | None -> Lwt.return_nil
        | Some t ->
            t
            |> List.map
                 (trustee_public_key_of_string Yojson.Safe.read_json
                    Yojson.Safe.read_json)
            |> List.map (fun x -> `Single x)
            |> Lwt.return
      in
      let* pedersens =
        let* x = get_threshold () in
        match x with
        | None -> Lwt.return_nil
        | Some t ->
            t
            |> threshold_parameters_of_string Yojson.Safe.read_json
                 Yojson.Safe.read_json
            |> fun x -> Lwt.return [ `Pedersen x ]
      in
      match singles @ pedersens with
      | [] -> failwith "trustees are missing"
      | trustees ->
          string_of_trustees Yojson.Safe.write_json Yojson.Safe.write_json
            trustees
          |> Lwt.return
    in
    let* trustees = get_trustees () in
    let open Lwt_io in
    let@ oc = with_file ~mode:Output (dir // "trustees.json") in
    write_line oc trustees

  let cmd =
    let doc = "create a trustee parameter file" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command reads $(i,public_keys.jsons) and $(i,threshold.json) \
           (if any). It then generates an $(i,trustees.json) file.";
      ]
      @ common_man
    in
    Cmd.v (Cmd.info "make-trustees" ~doc ~man) Term.(ret (const main $ dir_t))
end

module Mkelection : CMDLINER_MODULE = struct
  let main dir group version uuid template =
    let@ () = wrap_main in
    let* template = get_mandatory_opt "--template" template |> string_of_file in
    let group = get_mandatory_opt "--group" group in
    let uuid = get_mandatory_opt "--uuid" uuid |> Uuid.wrap in
    let* trustees = dir // "trustees.json" |> string_of_file in
    let module G = (val Group.of_string ~version group) in
    let module Trustees = (val Trustees.get_by_version version) in
    let module K = Trustees.MakeCombinator (G) in
    let template =
      let (Version v) = Election.version_of_int version in
      let open (val Election.get_serializers v) in
      Election.Template (v, template_of_string read_question template)
    in
    let trustees =
      trustees_of_string (sread G.of_string) (sread G.Zq.of_string) trustees
    in
    let y = K.combine_keys trustees in
    let public_key = G.to_string y in
    let params =
      Election.make_raw_election ~version template ~uuid ~group ~public_key
    in
    let open Lwt_io in
    let@ oc = with_file ~mode:Output (dir // "election.json") in
    write_line oc params

  let template_t =
    let doc = "Read election template from file $(docv)." in
    Arg.(
      value & opt (some file) None & info [ "template" ] ~docv:"TEMPLATE" ~doc)

  let cmd =
    let doc = "create an election public parameter file" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command reads and checks $(i,public_keys.jsons) (or \
           $(i,threshold.json) if it exists). It then computes the global \
           election public key and generates an $(i,election.json) file.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "make-election" ~doc ~man)
      Term.(
        ret (const main $ dir_t $ group_t $ version_t $ uuid_t $ template_t))
end

module GenerateToken : CMDLINER_MODULE = struct
  let main length =
    let@ () = wrap_main in
    let module X = MakeGenerateToken (Random) in
    X.generate_token ~length () |> Lwt_io.printl

  let length_t =
    let doc = "Token length." in
    Arg.(value & opt int 14 & info [ "length" ] ~docv:"L" ~doc)

  let cmd =
    let doc = "generate a token" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command generates a random token suitable for an election \
           identifier.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "generate-token" ~doc ~man)
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
      SubCredgen.cmd;
      Mktrustees.cmd;
      Mkelection.cmd;
      GenerateToken.cmd;
    ]
  in
  Cmd.group info cmds
