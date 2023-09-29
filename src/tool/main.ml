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

module B = Belenios
open Belenios_platform.Platform
open Belenios_core.Serializable_j
open Belenios_core.Common
open Common
open Cmdliner

module Bench : CMDLINER_MODULE = struct
  let gen n i =
    let j = n * i in
    let xs = Array.init n (fun i -> sha256_hex (string_of_int (j + i))) in
    Z.of_hex (xs |> Array.to_list |> String.concat "")

  let bench_group version group n =
    let@ () = wrap_main in
    let group = get_mandatory_opt "--group" group in
    let module G = (val B.Group.of_string ~version group) in
    let byte_length = bytes_to_sample G.Zq.q in
    let xs = Array.init n (fun i -> gen byte_length i |> G.Zq.reduce) in
    let start = Unix.gettimeofday () in
    let ys = Array.map (fun x -> G.(g **~ x)) xs in
    let stop = Unix.gettimeofday () in
    let delta_exp = stop -. start in
    let start = Unix.gettimeofday () in
    ignore (Array.fold_left G.( *~ ) G.one ys);
    let stop = Unix.gettimeofday () in
    let delta_mul = stop -. start in
    Printf.printf "Bench result (size %d): %.3f s (exp), %.3f s (mul)!\n" n
      delta_exp delta_mul

  let group_t =
    let doc = "Use group $(docv)." in
    Arg.(value & opt (some string) None & info [ "group" ] ~docv:"GROUP" ~doc)

  let version_t =
    let doc = "Use protocol version $(docv)." in
    Arg.(
      value
      & opt int (List.hd supported_crypto_versions)
      & info [ "protocol-version" ] ~docv:"VERSION" ~doc)

  let count_t =
    let doc = "Do $(docv) iterations." in
    Arg.(value & opt int 1000 & info [ "count" ] ~docv:"COUNT" ~doc)

  let group_cmd =
    let doc = "bench group operations" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command performs a benchmark of group exponentiation and \
           multiplication.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "group" ~doc ~man)
      Term.(ret (const bench_group $ version_t $ group_t $ count_t))

  let cmd =
    let doc = "benchmarking commands" in
    let man = common_man in
    let info = Cmd.info "bench" ~doc ~man in
    let cmds = [ group_cmd ] in
    Cmd.group info cmds
end

module Shasum : CMDLINER_MODULE = struct
  let main () =
    wrap_main (fun () -> chars_of_stdin () |> sha256_b64 |> print_endline)

  let cmd =
    let doc =
      "compute SHA256 of standard input and encode it in Base64Compact"
    in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command compute the SHA256 of standard input and encode it in \
           Base64Compact. This computation is frequent when auditing an \
           election. This single shell command is equivalent to the following \
           shell pipeline:";
        `Pre "sha256sum | xxd -r -p | base64 | tr -d \"=\"";
        `P "but does not need each individual command to be available.";
      ]
      @ common_man
    in
    Cmd.v (Cmd.info "sha256-b64" ~doc ~man) Term.(ret (const main $ const ()))
end

module Events : CMDLINER_MODULE = struct
  let init dir election trustees public_creds =
    let@ () = wrap_main in
    let election = string_of_file election in
    let trustees = string_of_file trustees in
    let public_creds =
      string_of_file public_creds
      |> public_credentials_of_string |> List.map strip_cred
      |> string_of_public_credentials
    in
    let file =
      let election = B.Election.of_string election in
      (dir // Uuid.unwrap election.e_uuid) ^ ".bel"
    in
    ignore (Tool_events.init ~file ~election ~trustees ~public_creds)

  let add_event dir event_typ =
    let@ () = wrap_main in
    let file = dir // find_bel_in_dir dir in
    let index = Tool_events.get_index ~file in
    let event_typ =
      get_mandatory_opt "--type" event_typ
      |> Printf.sprintf "%S" |> event_type_of_string
    in
    let payloads = lines_of_stdin () in
    let payload =
      match List.rev payloads with
      | x :: _ -> Some (Hash.hash_string x)
      | _ -> None
    in
    let open Tool_events in
    List.map (fun x -> Data x) payloads @ [ Event (event_typ, payload) ]
    |> append index

  let election_t =
    let doc = "Read election parameters from file $(docv)." in
    Arg.(
      value & opt file "election.json"
      & info [ "election" ] ~docv:"ELECTION" ~doc)

  let trustees_t =
    let doc = "Read trustees from file $(docv)." in
    Arg.(
      value & opt file "trustees.json"
      & info [ "trustees" ] ~docv:"TRUSTEES" ~doc)

  let public_creds_t =
    let doc = "Read public credentials from file $(docv)." in
    Arg.(
      value
      & opt file "public_creds.json"
      & info [ "public-creds" ] ~docv:"PUBLIC-CREDS" ~doc)

  let event_typ_t =
    let doc = "Type of event." in
    Arg.(value & opt (some string) None & info [ "type" ] ~docv:"TYPE" ~doc)

  let init_cmd =
    let doc = "initialize events" in
    let man =
      [
        `S "DESCRIPTION";
        `P "This command creates $(i,UUID.bel) from election setup files.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "init" ~doc ~man)
      Term.(ret (const init $ dir_t $ election_t $ trustees_t $ public_creds_t))

  let add_event_cmd =
    let doc = "add an event" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command adds a new event to $(i,UUID.bel). If stdin is \
           non-empty, each of its lines is added to $(i,UUID.bel) prior to the \
           event, and the last line is added as payload of the event.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "add-event" ~doc ~man)
      Term.(ret (const add_event $ dir_t $ event_typ_t))

  let cmd =
    let doc = "manage archives" in
    let man = common_man in
    let info = Cmd.info "archive" ~doc ~man in
    Cmd.group info [ init_cmd; add_event_cmd; Tool_mkarchive.cmd ]
end

module Methods : CMDLINER_MODULE = struct
  let schulze nchoices blank_allowed =
    let@ () = wrap_main in
    let ballots = chars_of_stdin () |> condorcet_ballots_of_string in
    let nchoices =
      if nchoices = 0 then
        if Array.length ballots > 0 then Array.length ballots.(0) else 0
      else nchoices
    in
    if nchoices <= 0 then
      failcmd "invalid --nchoices parameter (or could not infer it)"
    else
      let blank_allowed =
        match blank_allowed with
        | None -> failcmd "--blank-allowed is missing"
        | Some b -> b
      in
      ballots
      |> Belenios_core.Schulze.compute ~nchoices ~blank_allowed
      |> string_of_schulze_result |> print_endline

  let mj nchoices ngrades blank_allowed =
    let@ () = wrap_main in
    let ballots = chars_of_stdin () |> mj_ballots_of_string in
    let nchoices =
      if nchoices = 0 then
        if Array.length ballots > 0 then Array.length ballots.(0) else 0
      else nchoices
    in
    if nchoices <= 0 then
      failcmd "invalid --nchoices parameter (or could not infer it)"
    else
      let ngrades =
        match ngrades with
        | None -> failcmd "--ngrades is missing"
        | Some i -> if i > 0 then i else failcmd "invalid --ngrades parameter"
      in
      let blank_allowed =
        match blank_allowed with
        | None -> failcmd "--blank-allowed is missing"
        | Some b -> b
      in
      ballots
      |> Belenios_core.Majority_judgment.compute ~nchoices ~ngrades
           ~blank_allowed
      |> string_of_mj_result |> print_endline

  let stv nseats =
    let@ () = wrap_main in
    let nseats =
      match nseats with
      | None -> failcmd "--nseats is missing"
      | Some i -> if i > 0 then i else failcmd "invalid --nseats parameter"
    in
    chars_of_stdin () |> stv_raw_ballots_of_string
    |> Belenios_core.Stv.compute ~nseats
    |> string_of_stv_result |> print_endline

  let nchoices_t =
    let doc = "Number of choices. If 0, try to infer it." in
    Arg.(value & opt int 0 & info [ "nchoices" ] ~docv:"N" ~doc)

  let ngrades_t =
    let doc = "Number of grades." in
    Arg.(value & opt (some int) None & info [ "ngrades" ] ~docv:"G" ~doc)

  let nseats_t =
    let doc = "Number of seats." in
    Arg.(value & opt (some int) None & info [ "nseats" ] ~docv:"N" ~doc)

  let blank_allowed_t =
    let doc = "Is blank allowed?" in
    Arg.(value & opt (some bool) None & info [ "blank-allowed" ] ~docv:"B" ~doc)

  let schulze_cmd =
    let doc = "compute Schulze result" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command reads on standard input JSON-formatted ballots and \
           interprets them as Condorcet rankings on $(i,N) choices. It then \
           computes the result according to the Schulze method and prints it \
           on standard output.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "schulze" ~doc ~man)
      Term.(ret (const schulze $ nchoices_t $ blank_allowed_t))

  let mj_cmd =
    let doc = "compute Majority Judgment result" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command reads on standard input JSON-formatted ballots and \
           interprets them as grades (ranging from 1 (best) to $(i,G) (worst)) \
           given to $(i,N) choices. It then computes the result according to \
           the Majority Judgment method and prints it on standard output.";
      ]
      @ common_man
    in
    Cmd.v
      (Cmd.info "majority-judgment" ~doc ~man)
      Term.(ret (const mj $ nchoices_t $ ngrades_t $ blank_allowed_t))

  let stv_cmd =
    let doc = "compute Single Transferable Vote result" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This command reads on standard input JSON-formatted ballots and \
           interprets them as rankings of choices (ranging from 1 (best) to \
           $(i,X) (worst)). It then computes the result according to the \
           Single Transferable Vote method and prints it on standard output.";
      ]
      @ common_man
    in
    Cmd.v (Cmd.info "stv" ~doc ~man) Term.(ret (const stv $ nseats_t))

  let cmd =
    let doc = "compute result with specific counting methods" in
    let man = common_man in
    let info = Cmd.info "method" ~doc ~man in
    Cmd.group info [ schulze_cmd; mj_cmd; stv_cmd ]
end

let cmds =
  [
    Bench.cmd;
    Shasum.cmd;
    Setup.cmd;
    Election.cmd;
    Events.cmd;
    Methods.cmd;
    Sealing.cmd;
  ]

let default_cmd =
  let open Belenios_platform.Version in
  let version = Printf.sprintf "%s (%s)" version build in
  let version = if debug then version ^ " [debug]" else version in
  let doc = "election management tool" in
  let man = common_man in
  ( Term.(ret (const (`Help (`Pager, None)))),
    Cmd.info "belenios-tool" ~version ~doc ~man )

let root_cmd =
  let default, i = default_cmd in
  Cmd.(group ~default i cmds)

let () = exit (Cmd.eval root_cmd)
