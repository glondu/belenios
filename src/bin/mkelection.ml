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

open Util
open Serializable_t
open Signatures

module type PARAMS = sig
  val group : (module Group_field.GROUP)
  val uuid : Uuidm.t
  val template : template
end

module GetParams (X : EMPTY) : PARAMS = struct
  let group = ref None
  let uuid = ref None
  let template = ref None

  let speclist = Arg.([
    "--group", String (fun s -> group := Some s), "file with group parameters";
    "--uuid", String (fun s -> uuid := Some s), "UUID of the election";
    "--template", String (fun s -> template := Some s), "file with election template";
  ])

  let usage_msg =
    Printf.sprintf "Usage: %s mkelection --group <file> --uuid <uuid> --template <file>" Sys.argv.(0)

  let usage () =
    Arg.usage speclist usage_msg;
    exit 1

  let die s = prerr_endline s; usage ()

  let anon_fun x =
    Printf.eprintf "I do not know what to do with %s\n" x;
    usage ()

  let () = Arg.parse speclist anon_fun usage_msg

  let group = match !group with
    | None -> die "--group is missing"
    | Some fname ->
      let ic = open_in fname in
      let ls = Yojson.init_lexer () in
      let lb = Lexing.from_channel ic in
      let r = Serializable_j.read_ff_params ls lb in
      close_in ic;
      Group_field.make r

  let uuid = match !uuid with
    | None -> die "--uuid is missing"
    | Some uuid -> match Uuidm.of_string uuid with
      | None -> die "invalid UUID"
      | Some u -> u

  let template = match !template with
    | None -> die "--template is missing"
    | Some fname ->
      let ic = open_in fname in
      let ls = Yojson.init_lexer () in
      let lb = Lexing.from_channel ic in
      let r = Serializable_j.read_template ls lb in
      close_in ic;
      r
end

module MakeElection (G : Group_field.GROUP) (P : PARAMS) = struct

  (* Setup group *)

  module M = Election.MakeSimpleMonad(G);;

  (* Setup trustees *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  let public_keys =
    let ic = open_in "public_keys.jsons" in
    let raw_keys =
      let rec loop xs =
        match (try Some (input_line ic) with End_of_file -> None) with
        | Some x -> loop (x::xs)
        | None -> xs
      in loop []
    in
    close_in ic;
    let keys = List.map (fun x ->
      Serializable_j.trustee_public_key_of_string Serializable_builtin_j.read_number x
    ) raw_keys |> Array.of_list in
    assert (Array.forall KG.check keys);
    keys

  let y = KG.combine public_keys

  (* Setup election *)

  let {g; p; q} = G.group

  let params = {
    e_description = P.template.t_description;
    e_name = P.template.t_name;
    e_public_key = {ffpk_g = g; ffpk_p = p; ffpk_q = q; ffpk_y = y};
    e_questions = P.template.t_questions;
    e_uuid = P.uuid;
    e_short_name = P.template.t_short_name;
  }

  (* Save to disk *)

  open Serializable_j
  let () = save_to "election.json" (write_params write_ff_pubkey) params

end

let main () =
  let module P = GetParams (struct end) in
  let module G = (val P.group : Group_field.GROUP) in
  let module X = MakeElection (G) (P) in
  ()
