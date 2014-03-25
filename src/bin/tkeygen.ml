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

module type PARAMS = sig
  module G : GROUP
end

let parse_args () = begin

  let group = ref None in

  let speclist = Arg.([
    "--group", String (fun s -> group := Some s), "file with group parameters";
  ]) in

  let usage_msg =
    Printf.sprintf "Usage: %s trustee-keygen --group <file>" Sys.argv.(0)
  in

  let usage () =
    Arg.usage speclist usage_msg;
    exit 1
  in

  let anon_fun x =
    Printf.eprintf "I do not know what to do with %s\n" x;
    usage ()
  in

  let () = Arg.parse speclist anon_fun usage_msg in

  let group = match !group with
    | None ->
      Printf.eprintf "--group is missing!\n";
      usage ()
    | Some fname ->
      let ic = open_in fname in
      let ls = Yojson.init_lexer () in
      let lb = Lexing.from_channel ic in
      let r = read_ff_params ls lb in
      close_in ic;
      r
  in

  let module P = struct
    module G = (val Group_field.make group : Group_field.GROUP)
  end in

  (module P : PARAMS)

end

module Run (P : PARAMS) : EMPTY = struct
  open P

  let write_elt = make_write G.to_string

  (* Setup group *)

  module M = Election.MakeSimpleMonad(G);;

  (* Generate key *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;
  let private_key, public_key = KG.generate_and_prove () ();;
  assert (KG.check public_key);;

  (* Save to file *)

  let id = String.sub
    (sha256_hex (G.to_string public_key.trustee_public_key))
    0 8 |> String.uppercase
  ;;

  Printf.printf "Keypair %s has been generated\n%!" id;;

  let pubkey =
    "public",
    id ^ ".pubkey",
    0o444,
    public_key,
    write_trustee_public_key write_elt

  let privkey =
    "private",
    id ^ ".privkey",
    0o400,
    private_key,
    write_number

  let save (kind, filename, perm, thing, writer) =
    let oc = open_out_gen [Open_wronly; Open_creat] perm filename in
    let ob = Bi_outbuf.create_channel_writer oc in
    writer ob thing;
    Bi_outbuf.add_char ob '\n';
    Bi_outbuf.flush_channel_writer ob;
    close_out oc;
    Printf.printf "%s key saved to %s\n%!" (String.capitalize kind) filename;
    (* set permissions in the unlikely case where the file already existed *)
    Unix.chmod filename perm;;

  save pubkey;;
  save privkey;;

end


let main () =
  let module P = (val parse_args () : PARAMS) in
  let module X : EMPTY = Run (P) in
  ()
