(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026 VCAST                                                *)
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

open Lwt.Syntax
open Cmdliner
open Belenios

let self = Sys.argv.(0)

let nb_voters =
  match int_of_string @@ Sys.getenv "BELENIOS_SCALING_NB_VOTERS" with
  | n -> n
  | exception _ -> 1000

let main () =
  let@ () = Common.wrap_main in
  let@ () = fun cont -> Lwt_main.run @@ cont () in
  let* () = Lwt_io.eprintlf "Running scenario with %d voters..." nb_voters in
  let start = Unix.gettimeofday () in
  let@ server =
    Lwt_process.with_process_none
      ( "sh",
        [|
          "sh";
          "-c";
          "BELENIOS_SENDMAIL=tests/sendmail_fake.sh demo/run-server.sh";
        |] )
  in
  let* () = Lwt_unix.sleep 1. in
  let* () =
    match server#state with
    | Running -> Lwt.return_unit
    | Exited status ->
        let r = match status with WEXITED r -> r | _ -> 127 in
        let* () = Lwt_io.eprintlf "Starting server exited with %d" r in
        exit 1
  in
  let* uuid =
    Lwt_process.pread
      ( self,
        [|
          self;
          "setup";
          "--admin-login=admin";
          "--url=http://127.0.0.1:8001/";
          Printf.sprintf "--voters=%d" nb_voters;
          "--validate";
        |] )
  in
  let uuid = String.trim uuid in
  let* status =
    let credentials =
      Printf.sprintf "--credentials=./scaling.%s.privcreds.json" uuid
    in
    let uuid_flag = Printf.sprintf "--uuid=%s" uuid in
    Lwt_process.exec
      ( self,
        [|
          self;
          "vote";
          credentials;
          "--url=http://127.0.0.1:8001/";
          uuid_flag;
          Printf.sprintf "--requests=%d" nb_voters;
          "--concurrency=10";
        |] )
  in
  let* _ =
    let cmd = "demo/stop-server.sh" in
    Lwt_process.exec (cmd, [| cmd |])
  in
  let delta = Unix.gettimeofday () -. start in
  let* () =
    match status with
    | WEXITED 0 -> Lwt_io.eprintlf "Scenario succeeded in %f s" delta
    | _ -> Lwt_io.eprintlf "Scenario failed in %f s" delta
  in
  match status with
  | WEXITED 0 -> Lwt.return_unit
  | WEXITED r -> exit r
  | _ -> exit 127

let cmd =
  let doc = "run a scaling test scenario" in
  let man = [] in
  Cmd.v (Cmd.info "scenario" ~doc ~man) Term.(ret (const main $ const ()))
