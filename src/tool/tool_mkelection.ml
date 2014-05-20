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
  val uuid : string
  val group : string
  val template : string
  val get_public_keys : unit -> string array option
end

module type S = sig
  val mkelection : unit -> string
end

module type PARSED_PARAMS = sig
  val uuid : Uuidm.t
  val template : template
  module G : GROUP
  val get_public_keys : unit -> G.t trustee_public_key array option
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module R = struct
    let uuid =
      match Uuidm.of_string P.uuid with
      | Some u -> u
      | None -> Printf.ksprintf failwith "%s is not a valid UUID" P.uuid
    let template = template_of_string P.template
    module G = (val Group.of_string P.group : GROUP)
    let get_public_keys () =
      match P.get_public_keys () with
      | None -> None
      | Some xs -> Some (Array.map (trustee_public_key_of_string G.read) xs)
  end
  in (module R : PARSED_PARAMS)

let ( / ) = Filename.concat

module Make (P : PARSED_PARAMS) : S = struct
  open P

  (* Setup group *)

  module M = Election.MakeSimpleMonad(G);;

  (* Setup trustees *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  let public_keys =
    match get_public_keys () with
    | Some keys -> keys
    | None -> failwith "trustee keys are missing"

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

  (* Generate and serialize election.json *)

  let mkelection () =
    string_of_params G.write_wrapped_pubkey params

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

open Tool_common

let main dir group uuid template =
  wrap_main (fun () ->
    let module P = struct
      let group = get_mandatory_opt "--group" group |> string_of_file
      let uuid = get_mandatory_opt "--uuid" uuid
      let template = get_mandatory_opt "--template" template |> string_of_file
      let get_public_keys () =
        Some (lines_of_file (dir / "public_keys.jsons") |> Array.of_list)
    end in
    let module R = (val make (module P : PARAMS) : S) in
    let params = R.mkelection () in
    let oc = open_out (dir / "election.json") in
    output_string oc params;
    output_char oc '\n';
    close_out oc
  )

open Cmdliner

let group_t =
  let doc = "Take group parameters from file $(docv)." in
  Arg.(value & opt (some file) None & info ["group"] ~docv:"GROUP" ~doc)

let uuid_t =
  let doc = "UUID of the election." in
  Arg.(value & opt (some string) None & info ["uuid"] ~docv:"UUID" ~doc)

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
