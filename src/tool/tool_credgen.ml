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

open Platform
open Signatures
open Common

type generate_kind = Count of int | File of string
type action =  Derive of string | Generate of generate_kind

module type PARAMS = sig
  val uuid : Uuidm.t
  val action : action
  val dir : string
  module G : GROUP
end

module Run (P : PARAMS) : EMPTY = struct
  open P

  (* Some helpers *)

  (* Beware: the following must be changed in accordance with the booth! *)
  let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  let token_length = 14
  let n58 = Z.of_int 58
  let n53 = Z.of_int 53

  let public_key_of_token uuid x =
    let hex = derive_cred uuid x in
    let x = Z.(of_string_base 16 hex mod G.q) in
    let y = G.(g **~ x) in
    G.to_string y

  let generate kind = begin

    let count, ids = match kind with
      | File f ->
        let ic = open_in f in
        let rec loop accu =
          match (try Some (input_line ic) with End_of_file -> None) with
          | Some "" -> loop accu
          | Some x -> loop (x::accu)
          | None -> List.rev accu
        in
        let res = loop [] in
        close_in ic;
        List.length res, Some res
      | Count i -> i, None
    in

    (* The generation itself *)

    let prng = pseudo_rng (random_string secure_rng 16) in
    let random_char () = int_of_char (random_string prng 1).[0] in

    let generate_raw_token () =
      let res = String.create token_length in
      let rec loop i accu =
        if i < token_length then (
          let digit = random_char () mod 58 in
          res.[i] <- digits.[digit];
          loop (i+1) Z.(n58 * accu + of_int digit)
        ) else (res, accu)
      in loop 0 Z.zero
    in

    let generate_token () =
      let (raw, value) = generate_raw_token () in
      let checksum = 53 - Z.(to_int (value mod n53)) in
      raw ^ String.make 1 digits.[checksum]
    in

    let private_credentials =
      let rec loop i accu =
        if i > 0 then loop (i-1) (generate_token () :: accu)
        else accu
      in loop count []
    in

    let public_credentials =
      List.map (public_key_of_token uuid) private_credentials
    in

    let hashed_credentials = option_map (fun ids ->
      List.map2 (fun id cred ->
        Printf.sprintf "%s %s" (sha256_hex cred) id
      ) ids public_credentials
    ) ids in

    (* Save to files *)

    let timestamp = Printf.sprintf "%.0f" (Unix.time ()) in

    let pub =
      "public credentials",
      timestamp ^ ".pubcreds",
      0o444,
      List.sort compare public_credentials
    in

    let priv =
      let kind, creds = match ids with
        | None -> "private credentials", private_credentials
        | Some ids -> "private credentials with ids",
          List.map2 (fun id cred ->
            Printf.sprintf "%s %s" cred id
          ) ids private_credentials
      in
      kind,
      timestamp ^ ".privcreds",
      0o400,
      List.sort compare creds
    in

    let hashed = option_map (fun h ->
      "hashed credentials with ids",
      timestamp ^ ".hashcreds",
      0o400,
      List.sort compare h
    ) hashed_credentials in

    let output_endline oc x =
      output_string oc x;
      output_char oc '\n'
    in

    let save (kind, filename, perm, thing) =
      let full_filename = Filename.concat dir filename in
      let oc = open_out_gen [
        Open_wronly; Open_creat; Open_excl
      ] perm full_filename in
      List.iter (output_endline oc) thing;
      close_out oc;
      Printf.printf "%d %s saved to %s\n%!" count kind full_filename
    in

    save pub;
    save priv;
    ignore (option_map save hashed)

  end

  let () = match action with
    | Derive d -> print_endline (public_key_of_token uuid d)
    | Generate kind -> generate kind

end

open Tool_common

let main group dir uuid count file derive =
  wrap_main (fun () ->
    let _, group = get_mandatory_opt "--group" group in
    let module P : PARAMS = struct
      module G = (val group : GROUP)
      let uuid = get_mandatory_opt "--uuid" uuid
      let dir = dir
      let action =
        match count, file, derive with
        | Some n, None, None ->
          if n < 1 then (
            failcmd "the argument of --count must be a positive number"
          ) else Generate (Count n)
        | None, Some f, None -> Generate (File f)
        | None, None, Some c -> Derive c
        | _, _, _ -> failcmd "--count, --file and --derive are mutually exclusive"
    end in
    let module X : EMPTY = Run (P) in ()
  )

open Cmdliner

let dir_t =
  let doc = "Save output files to $(docv)." in
  let the_info = Arg.info ["dir"] ~docv:"DIR" ~doc in
  Arg.(value & opt dir Filename.current_dir_name the_info)

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
