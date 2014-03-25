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
  val uuid : Uuidm.t
  val template : template
  module G : Group_field.GROUP
end

let parse_args () = begin

  let group = ref None in
  let uuid = ref None in
  let template = ref None in

  let speclist = Arg.([
    "--group", String (fun s -> group := Some s), "file with group parameters";
    "--uuid", String (fun s -> uuid := Some s), "UUID of the election";
    "--template", String (fun s -> template := Some s), "file with election template";
  ]) in

  let usage_msg =
    Printf.sprintf "Usage: %s mkelection --group <file> --uuid <uuid> --template <file>" Sys.argv.(0)
  in

  let usage () =
    Arg.usage speclist usage_msg;
    exit 1
  in

  let die s = prerr_endline s; usage () in

  let anon_fun x =
    Printf.eprintf "I do not know what to do with %s\n" x;
    usage ()
  in

  let () = Arg.parse speclist anon_fun usage_msg in

  let group = match !group with
    | None -> die "--group is missing"
    | Some fname ->
      let ic = open_in fname in
      let ls = Yojson.init_lexer () in
      let lb = Lexing.from_channel ic in
      let r = read_ff_params ls lb in
      r
  in

  let module P = struct

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
        let r = read_template ls lb in
        close_in ic;
        r

    module G = (val Group_field.make group : Group_field.GROUP)

  end in

  (module P : PARAMS)

end

module Run (P : PARAMS) : EMPTY = struct
  open P

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
      trustee_public_key_of_string read_number x
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

  let () = save_to "election.json" (write_params write_ff_pubkey) params

end

let main () =
  let module P = (val parse_args () : PARAMS) in
  let module X : EMPTY = Run (P) in
  ()
