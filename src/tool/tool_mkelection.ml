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
  val dir : string
  val uuid : Uuidm.t
  val template : template
  module G : GROUP
end

let ( / ) = Filename.concat

module Run (P : PARAMS) : EMPTY = struct
  open P

  (* Setup group *)

  module M = Election.MakeSimpleMonad(G);;

  (* Setup trustees *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  let public_keys =
    let ic = open_in (dir / "public_keys.jsons") in
    let raw_keys =
      let rec loop xs =
        match (try Some (input_line ic) with End_of_file -> None) with
        | Some x -> loop (x::xs)
        | None -> xs
      in loop []
    in
    close_in ic;
    let keys = List.map (fun x ->
      trustee_public_key_of_string G.read x
    ) raw_keys |> Array.of_list in
    assert (Array.forall KG.check keys);
    keys

  let y = KG.combine public_keys

  (* Setup election *)

  let params = {
    e_description = template.t_description;
    e_name = template.t_name;
    e_public_key = G.wrap_pubkey y;
    e_questions = template.t_questions;
    e_uuid = uuid;
    e_short_name = template.t_short_name;
  }

  (* Save to disk *)

  let write_params = write_params G.write_wrapped_pubkey
  let () = save_to (dir / "election.json") write_params params

end

open Tool_common

let main dir group uuid template =
  wrap_main (fun () ->
    let _, group = get_mandatory_opt "--group" group in
    let _, template = get_mandatory_opt "--template" template in
    let module P : PARAMS = struct
      module G = (val group : GROUP)
      let uuid = get_mandatory_opt "--uuid" uuid
      let template = template
      let dir = dir
    end in
    let module X : EMPTY = Run (P) in ()
  )

open Cmdliner

let dir_t =
  let doc = "Path to election files." in
  let the_info = Arg.info ["dir"] ~docv:"DIR" ~doc in
  Arg.(value & opt dir Filename.current_dir_name the_info)

let template_c =
  (fun fname ->
    if Sys.file_exists fname then (
      try
        let ic = open_in fname in
        let ls = Yojson.init_lexer () in
        let lb = Lexing.from_channel ic in
        let r = read_template ls lb in
        close_in ic;
        `Ok (fname, r)
      with e ->
        let e = Printexc.to_string e and s = Printf.sprintf in
        `Error (s "could not read template from %s (%s)" fname e)
    ) else `Error (Printf.sprintf "file %s does not exist" fname)
  ), (fun fmt (fname, _) -> Format.pp_print_string fmt fname)

let template_t =
  let doc = "Read election template from file $(docv)." in
  let the_info = Arg.info ["template"] ~docv:"TEMPLATE" ~doc in
  Arg.(value & opt (some template_c) None the_info)

let mkelection_cmd =
  let doc = "create an election public parameter file" in
  let man = [
    `S "DESCRIPTION";
    `P "This command reads and checks $(i,public_keys.jsons). It then computes the global election public key and generates an $(i,election.json) file.";
  ] @ common_man in
  Term.(ret (pure main $ dir_t $ group_t $ uuid_t $ template_t)),
  Term.info "mkelection" ~doc ~man

let cmds = [mkelection_cmd]
