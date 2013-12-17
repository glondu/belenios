(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2013 Inria                                           *)
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

open Util
open Serializable_t

module type PARAMS = sig
  val group : (module Election.FF_GROUP)
end

module GetParams (X : sig end) : PARAMS = struct
  let group = ref None

  let speclist = Arg.([
    "--group", String (fun s -> group := Some s), "file with group parameters";
  ])

  let usage_msg =
    Printf.sprintf "Usage: %s trustee-keygen [--group <file>]" Sys.argv.(0)

  let usage () =
    Arg.usage speclist usage_msg;
    exit 1

  let anon_fun x =
    Printf.eprintf "I do not know what to do with %s\n" x;
    usage ()

  let () = Arg.parse speclist anon_fun usage_msg

  let group = match !group with
    | None -> Election.((module DefaultGroup : FF_GROUP))
    | Some fname ->
      let ic = open_in fname in
      let ls = Yojson.init_lexer () in
      let lb = Lexing.from_channel ic in
      let r = Serializable_j.read_ff_params ls lb in
      close_in ic;
      Election.finite_field r
end

module RunTrusteeKeygen (G : Election.FF_GROUP) = struct

  (* Setup group *)

  assert (Election.check_finite_field G.group);;
  module M = Election.MakeSimpleMonad(G);;

  (* Generate key *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;
  let private_key, public_key = KG.generate_and_prove () ();;
  assert (KG.check public_key);;

  (* Save to file *)

  let id = String.sub
    (sha256_hex (Z.to_string public_key.trustee_public_key))
    0 8 |> String.uppercase
  ;;

  Printf.printf "Keypair %s has been generated\n%!" id;;

  let pubkey =
    "public",
    id ^ ".public",
    0o444,
    public_key,
    Serializable_j.write_trustee_public_key Serializable_builtin_j.write_number

  let privkey =
    "private",
    id ^ ".private",
    0o400,
    private_key,
    Serializable_builtin_j.write_number

  let save (kind, filename, perm, thing, writer) =
    let oc = open_out_gen [Open_wronly; Open_creat] perm filename in
    let ob = Bi_outbuf.create_channel_writer oc in
    writer ob thing;
    Bi_outbuf.flush_channel_writer ob;
    close_out oc;
    Printf.printf "%s key saved to %s\n%!" (String.capitalize kind) filename;
    (* set permissions in the unlikely case where the file already existed *)
    Unix.chmod filename perm;;

  save pubkey;;
  save privkey;;

end


let main () =
  let module P = GetParams (struct end) in
  let module G = (val P.group : Election.FF_GROUP) in
  let module X = RunTrusteeKeygen (G) in
  ()
