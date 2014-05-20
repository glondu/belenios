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
open Serializable_builtin_j
open Serializable_j
open Signatures
open Common

module type PARAMS = sig
  val group : string
end

module type S = sig
  type keypair = { id : string; priv : string; pub : string }
  val trustee_keygen : unit -> keypair
end

module type PARSED_PARAMS = sig
  module G : GROUP
end

let parse_params p =
  let module P = (val p : PARAMS) in
  let module R = struct
    module G = (val Group.of_string P.group : GROUP)
  end
  in (module R : PARSED_PARAMS)

module Make (P : PARSED_PARAMS) : S = struct
  open P

  (* Setup group *)

  module M = Election.MakeSimpleMonad(G);;

  (* Generate key *)

  module KG = Election.MakeSimpleDistKeyGen(G)(M);;

  type keypair = { id : string; priv : string; pub : string }

  let trustee_keygen () =
    let private_key, public_key = KG.generate_and_prove () () in
    assert (KG.check public_key);
    let id = String.sub
      (sha256_hex (G.to_string public_key.trustee_public_key))
      0 8 |> String.uppercase
    in
    Printf.printf "I: keypair %s has been generated\n%!" id;
    let priv = string_of_number private_key in
    let pub = string_of_trustee_public_key G.write public_key in
    {id; priv; pub}

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

let main group =
  wrap_main (fun () ->
    let module P = struct
      let group = get_mandatory_opt "--group" group |> string_of_file
    end in
    let module R = (val make (module P : PARAMS) : S) in
    let kp = R.trustee_keygen () in
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
  )

open Cmdliner

let group_t =
  let doc = "Take group parameters from file $(docv)." in
  Arg.(value & opt (some file) None & info ["group"] ~docv:"GROUP" ~doc)

let tkeygen_cmd =
  let doc = "generate a trustee key" in
  let man = [
    `S "DESCRIPTION";
    `P "This command is run by a trustee to generate a share of an election key. Such a share consists of a private key and a public key with a certificate. Generated files are stored in the current directory with a name that starts with $(i,ID), where $(i,ID) is a short fingerprint of the public key. The private key is stored in $(i,ID.privkey) and must be secured by the trustee. The public key is stored in $(i,ID.pubkey) and must be sent to the election administrator.";
  ] @ common_man in
  Term.(ret (pure main $ group_t)),
  Term.info "trustee-keygen" ~doc ~man

let cmds = [tkeygen_cmd]
