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

module type PARAMS = sig
  val uuid : string
  val group : string
end

module type S = sig
  val derive : string -> string
  val generate : unit -> string * string * string
end

module type PARSED_PARAMS = sig
  val uuid : Uuidm.t
  module G : GROUP
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module R = struct
    let uuid =
      match Uuidm.of_string P.uuid with
      | Some u -> u
      | None -> Printf.ksprintf failwith "%s is not a valid UUID" P.uuid
    module G = (val Group.of_string P.group : GROUP)
  end
  in (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct
  open P

  (* Some helpers *)

  (* Beware: the following must be changed in accordance with the booth! *)
  let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  let token_length = 14
  let n58 = Z.of_int 58
  let n53 = Z.of_int 53

  let derive x =
    let hex = derive_cred uuid x in
    let x = Z.(of_string_base 16 hex mod G.q) in
    let y = G.(g **~ x) in
    G.to_string y

  let prng = lazy (pseudo_rng (random_string secure_rng 16))

  let random_char () =
    int_of_char (random_string (Lazy.force prng) 1).[0]

  let generate_raw_token () =
    let res = String.create token_length in
    let rec loop i accu =
      if i < token_length then (
        let digit = random_char () mod 58 in
        res.[i] <- digits.[digit];
        loop (i+1) Z.(n58 * accu + of_int digit)
      ) else (res, accu)
    in loop 0 Z.zero

  let add_checksum (raw, value) =
    let checksum = 53 - Z.(to_int (value mod n53)) in
    raw ^ String.make 1 digits.[checksum]

  let compute_pub_and_hash priv =
    let pub = derive priv in
    let hashed = sha256_hex pub in
    priv, pub, hashed

  let generate () =
    generate_raw_token () |> add_checksum |> compute_pub_and_hash

end

let make params =
  let module P = (val parse_params params : PARSED_PARAMS) in
  let module R = Make (P) in
  (module R : S)

let lines_of_file f =
  let ic = open_in f in
  let rec loop accu =
    match (try Some (input_line ic) with End_of_file -> None) with
    | Some x -> loop (x::accu)
    | None -> List.rev accu
  in
  let res = loop [] in
  close_in ic;
  res

let string_of_file f =
  lines_of_file f |> String.concat "\n"

let int_length n =
  string_of_int n |> String.length

let rec find_first n first =
  if int_length first = int_length (first + n) then first
  else find_first n (10 * first)

let generate_ids n =
  (* choose the first id so that they all have the same length *)
  let first = find_first n 1 in
  let last = first + n - 1 in
  let rec loop last accu =
    if last < first then accu
    else loop (last-1) (string_of_int last :: accu)
  in loop last []

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

let ( / ) = Filename.concat

open Tool_common

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
      | None, Some f, None -> `Generate (lines_of_file f)
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

open Cmdliner

let group_t =
  let doc = "Take group parameters from file $(docv)." in
  Arg.(value & opt (some file) None & info ["group"] ~docv:"GROUP" ~doc)

let uuid_t =
  let doc = "UUID of the election." in
  Arg.(value & opt (some string) None & info ["uuid"] ~docv:"UUID" ~doc)

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
