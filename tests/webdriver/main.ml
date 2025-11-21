(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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
open Common

let getenv_int ~default x =
  match Sys.getenv_opt x with None -> default | Some x -> int_of_string x

module Config = struct
  let headless = Sys.getenv_opt "BELENIOS_HEADLESS" <> None
  let width = getenv_int ~default:1000 "BELENIOS_WINDOW_WIDTH"
  let height = getenv_int ~default:1000 "BELENIOS_WINDOW_HEIGHT"
  let webdriver = "http://127.0.0.1:4444"
  let belenios = "http://127.0.0.1:8001"
  let email_file = "/tmp/sendmail_fake"
  let tmpdir = ref None
end

let admin_of_string = function
  | "local" -> Admin.Local { username = "user1"; password = "RP91JMQkL6Lz" }
  | "demo" -> Admin.Demo { username = "admin" }
  | _ -> invalid_arg "admin_of_string"

let default_question =
  Admin.Select
    {
      question = "Question 1?";
      min = 1;
      max = 2;
      blank = true;
      answers = [ "Answer 1-1"; "Answer 1-2"; "Answer 1-3" ];
    }

let questions_of_string = function
  | "one" -> [ default_question ]
  | "two" ->
      [
        default_question;
        Admin.Select
          {
            question = "Question 2?";
            min = 0;
            max = 1;
            blank = false;
            answers = [ "Answer 2-1"; "Answer 2-2" ];
          };
      ]
  | _ -> invalid_arg "questions_of_string"

let trustees : Admin.trustee list =
  [
    { name = "Alice Trustee"; email = "alice@example.org" };
    { name = "Bob Trustee"; email = "bob@example.org" };
  ]

let trustees_of_string = function
  | "none" -> Admin.{ mode = Basic; trustees = [] }
  | "basic" -> { mode = Basic; trustees }
  | "threshold" -> { mode = Threshold 1; trustees }
  | _ -> invalid_arg "trustees_of_string"

let registrar_of_string : _ -> Admin.registrar option = function
  | "none" -> None
  | "charlie" -> Some { name = "Charlie Registrar"; ext = None }
  | "david" ->
      Some
        {
          name = "David Registrar";
          ext =
            Some
              {
                server = Config.belenios ^ "/api/credentials/server";
                operator = "david@example.org";
              };
        }
  | _ -> invalid_arg "registrar_of_string"

let auth_of_string = function
  | "password" -> Admin.Password
  | "email" -> Admin.Email
  | _ -> invalid_arg "auth_of_string"

let monitor x =
  match !Config.tmpdir with
  | None -> Lwt.return_unit
  | Some tmpdir -> (
      let wdir = Filename.concat tmpdir "belenios-monitoring" in
      let* () =
        let* b = Lwt_unix.file_exists wdir in
        if not b then Lwt_unix.mkdir wdir 0o755 else Lwt.return_unit
      in
      let cmd = "contrib/monitor_elections.py" in
      let args =
        match x with
        | `Make_reference ->
            [|
              cmd;
              Printf.sprintf "--url=%s" Config.belenios;
              Printf.sprintf "--wdir=%s" wdir;
              "--checkhash=yes";
              "--hashref=contrib/reference_template.json";
              Printf.sprintf "--outputref=%s/reference.json" wdir;
            |]
        | `Monitor uuid ->
            [|
              cmd;
              Printf.sprintf "--url=%s" Config.belenios;
              Printf.sprintf "--wdir=%s" wdir;
              "--checkhash=yes";
              Printf.sprintf "--hashref=%s/reference.json" wdir;
              Printf.sprintf "--uuid=%s" uuid;
            |]
      in
      let* x = Lwt_process.exec (cmd, args) in
      match x with
      | WEXITED 0 -> Lwt.return_unit
      | _ -> failwith "monitoring failed")

let scenario admin questions nvoters trustees registrar auth =
  let voters = make_voters nvoters in
  let module Config = struct
    include Config

    let config = Admin.{ questions; voters; trustees; registrar; auth }
    let admin = admin
    let emails = open_in_gen [ Open_creat ] 0o644 email_file
  end in
  let module Admin = Admin.Make (Config) in
  let* e = Admin.setup_election () in
  let check () = monitor (`Monitor e.id) in
  let* () = check () in
  Printf.printf "  Page of the election: %s/election#%s\n" Config.belenios e.id;
  let emails = Emails.parse Config.emails in
  let module Config = struct
    include Config

    let election_id = e.id
  end in
  let module Vote = Vote.Make (Config) in
  let* () =
    Lwt_list.iter_s
      (fun voter ->
        let credential =
          match e.private_creds with
          | None | Some (Credop _) -> (
              match Emails.extract_credential emails voter with
              | Some x -> x
              | _ -> assert false)
          | Some (Creds (`Assoc o)) -> (
              match List.assoc_opt voter o with
              | Some (`String x) -> x
              | _ -> assert false)
          | _ -> assert false
        in
        let auth =
          match auth with
          | Password ->
              let password = Emails.extract_password emails voter in
              let password = Option.value ~default:"N/A" password in
              Vote.auth_password ~username:voter ~password
          | Email -> Vote.auth_email ~username:voter
        in
        let* () = Vote.vote ~voter ~credential ~auth in
        check ())
      voters
  in
  let* () =
    match auth with
    | Email -> Lwt.return_unit
    | Password ->
        let voter = List.hd voters in
        let credential = Emails.extract_credential emails voter in
        let credential = Option.value ~default:"N/A" credential in
        let* password = Admin.regen_password ~id:e.id ~username:voter in
        let auth = Vote.auth_password ~username:voter ~password in
        Vote.vote ~voter ~credential ~auth
  in
  let* () = Admin.tally_election check e in
  close_in Config.emails;
  Lwt.return_unit

let with_cmd cmd f =
  Printf.printf "Running: %s\n%!" cmd;
  let p =
    let open Lwt_process in
    open_process_none ~stdin:`Dev_null ~stdout:`Dev_null ~stderr:`Dev_null
      (shell cmd)
  in
  let* () = Lwt_unix.sleep 1. in
  match p#state with
  | Running ->
      Lwt.finalize f (fun () ->
          p#terminate;
          let* _ = p#status in
          Lwt.return_unit)
  | Exited s ->
      let msg =
        match s with
        | WEXITED s -> Printf.sprintf "process exited with code %d" s
        | WSIGNALED s -> Printf.sprintf "process was killed by signal %d" s
        | WSTOPPED s -> Printf.sprintf "process was stopped by signal %d" s
      in
      Lwt.fail @@ Failure msg

let rec main = function
  | "tmpdir" :: dir :: xs ->
      Printf.printf "Setting tmpdir to %s\n%!" dir;
      Config.tmpdir := Some dir;
      let* () = monitor `Make_reference in
      main xs
  | "run" :: cmd :: xs -> with_cmd cmd (fun () -> main xs)
  | "scenario" :: admin :: questions :: nvoters :: trustees :: registrar :: auth
    :: xs ->
      Printf.printf "Running: scenario %s %s %s %s %s %s\n%!" admin questions
        nvoters trustees registrar auth;
      let admin = admin_of_string admin in
      let questions = questions_of_string questions in
      let nvoters = int_of_string nvoters in
      let trustees = trustees_of_string trustees in
      let registrar = registrar_of_string registrar in
      let auth = auth_of_string auth in
      let t1 = Unix.gettimeofday () in
      let* () = scenario admin questions nvoters trustees registrar auth in
      let t2 = Unix.gettimeofday () in
      Printf.printf "End of scenario in %f s\n%!" (t2 -. t1);
      main xs
  | [] -> Lwt.return_unit
  | _ -> Lwt.fail @@ Failure "bad syntax"

let () = Lwt_main.run @@ main (Sys.argv |> Array.to_list |> List.tl)
