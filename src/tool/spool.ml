(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026-2026 Inria                                           *)
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
open Belenios
open Common
open Cmdliner
module P = Belenios_spool.Passwords

let import spool db force =
  let@ () = wrap_main in
  let* csv = chars_of_stdin () in
  let records = P.records_of_csv csv in
  let* () = P.import ~spool ~db ~force records in
  Lwt_io.eprintlf "Imported %d records into %s" (List.length records)
    (P.dir ~spool db)

let export spool db =
  let@ () = wrap_main in
  let* records = P.export ~spool ~db in
  Lwt_io.print (P.csv_of_records records)

let spool_t =
  let doc = "Use spool directory $(docv) (of the filesystem backend)." in
  Arg.(required & opt (some dir) None & info [ "spool" ] ~docv:"DIR" ~doc)

let db_t =
  let doc =
    "Use password database $(docv) (the $(b,db) attribute of $(b,<password>) \
     in the server configuration)."
  in
  Arg.(required & opt (some string) None & info [ "db" ] ~docv:"NAME" ~doc)

let force_t =
  let doc = "Replace the database if it already exists." in
  Arg.(value & flag & info [ "force" ] ~doc)

let csv_man =
  [
    `P
      "In the CSV format, each line consists of a username, a salt, a hashed \
       password (as given by $(b,belenios-tool hash-password)) and an optional \
       e-mail address, separated by commas. Additional fields are ignored.";
    `P
      "These commands work offline: the server must not use the database at \
       the same time.";
  ]

let import_cmd =
  let doc = "create a password database from a CSV file" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command reads a CSV file on standard input and creates the \
         corresponding password database in a spool. Usernames and addresses \
         must be unique (case-insensitively).";
    ]
    @ csv_man @ common_man
  in
  Cmd.v
    (Cmd.info "import" ~doc ~man)
    Term.(ret (const import $ spool_t $ db_t $ force_t))

let export_cmd =
  let doc = "export a password database to a CSV file" in
  let man =
    [
      `S "DESCRIPTION";
      `P
        "This command writes a password database of a spool, sorted by \
         username, in CSV format on standard output.";
    ]
    @ csv_man @ common_man
  in
  Cmd.v (Cmd.info "export" ~doc ~man) Term.(ret (const export $ spool_t $ db_t))

let passwords_cmd =
  let doc = "administrator password database commands" in
  let man = common_man in
  Cmd.group (Cmd.info "passwords" ~doc ~man) [ import_cmd; export_cmd ]

let filesystem_cmd =
  let doc = "commands for the spool of the filesystem storage backend" in
  let man = common_man in
  Cmd.group (Cmd.info "filesystem" ~doc ~man) [ passwords_cmd ]

let cmd =
  let doc = "server storage commands" in
  let man = common_man in
  Cmd.group (Cmd.info "spool" ~doc ~man) [ filesystem_cmd ]
