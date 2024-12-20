(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2024 Inria                                           *)
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
open Belenios_server_core
open Storage
open Types

let () = Stdlib.Random.self_init ()

module type BACKEND0 = sig
  include BACKEND

  val list_accounts : unit -> int list Lwt.t
  val list_elections : unit -> uuid list Lwt.t
end

module type S = sig
  val mutexes : uuid option Indexed_mutex.t
  val make : uuid option Mutex_set.t -> (module BACKEND0)
end

module MakeBackend
    (Config : CONFIG)
    (Accounts_cache : CLEAR)
    (Elections_cache : CLEAR) : S = struct
  (** {1 Mutexes} *)

  let mutexes = Indexed_mutex.create ()

  (** {1 Abstract election-specific file operations} *)

  type 'key abstract_file_ops = {
    mutable get : uuid -> 'key -> string option Lwt.t;
    mutable set : uuid -> 'key -> string -> unit Lwt.t;
  }

  let make_uninitialized_ops what =
    let e = Lwt.fail @@ Not_implemented what in
    { get = (fun _ _ -> e); set = (fun _ _ _ -> e) }

  (** {1 Forward references} *)

  let dates_ops = make_uninitialized_ops "dates_ops"
  let extended_records_ops = make_uninitialized_ops "extended_records_ops"
  let credential_mappings_ops = make_uninitialized_ops "credential_mappings_ops"
  let data_ops = make_uninitialized_ops "data_ops"
  let roots_ops = make_uninitialized_ops "roots_ops"
  let voters_config_ops = make_uninitialized_ops "voters_config_ops"
  let voters_ops = make_uninitialized_ops "voters_ops"
  let credential_weights_ops = make_uninitialized_ops "credential_weights_ops"
  let credential_users_ops = make_uninitialized_ops "credential_users_ops"
  let password_records_ops = make_uninitialized_ops "password_records_ops"

  let get_password_file =
    ref (fun _ -> Lwt.fail @@ Not_implemented "get_password_file")

  let set_password_file =
    ref (fun _ _ -> Lwt.fail @@ Not_implemented "set_password_file")

  (** {1 Password file operations} *)

  module PasswordRecordsCacheTypes = struct
    type key = Admin of string | Election of uuid
    type value = password_record SMap.t * password_record SMap.t
  end

  module PasswordRecordsCache = Ocsigen_cache.Make (PasswordRecordsCacheTypes)

  exception Password_db_not_found

  let raw_get_password_records (key : PasswordRecordsCacheTypes.key) =
    let* csv =
      match key with
      | Admin file -> Filesystem.read_file file
      | Election uuid -> !get_password_file uuid
    in
    let* csv =
      match csv with
      | None -> Lwt.fail Password_db_not_found
      | Some x ->
          Lwt_preemptive.detach (fun x -> Csv.(input_all (of_string x))) x
    in
    Lwt_list.fold_left_s
      (fun ((username_indexed, address_indexed) as accu) r ->
        let@ r cont =
          match r with
          | username :: salt :: hashed :: address :: _ ->
              cont { username; salt; hashed; address = Some address }
          | username :: salt :: hashed :: _ ->
              cont { username; salt; hashed; address = None }
          | _ -> Lwt.return accu
        in
        let username_indexed =
          SMap.add (String.lowercase_ascii r.username) r username_indexed
        in
        let address_indexed =
          match r.address with
          | None -> address_indexed
          | Some a -> SMap.add (String.lowercase_ascii a) r address_indexed
        in
        Lwt.return (username_indexed, address_indexed))
      (SMap.empty, SMap.empty) csv

  let password_records_cache =
    new PasswordRecordsCache.cache raw_get_password_records ~timer:3600. 100

  let get_password_record_generic where who =
    Lwt.try_bind
      (fun () -> password_records_cache#find where)
      (fun (u, a) ->
        let key, map =
          match who with Username u' -> (u', u) | Address a' -> (a', a)
        in
        let r = SMap.find_opt (String.lowercase_ascii key) map in
        Lwt.return @@ Option.map string_of_password_record r)
      (function Password_db_not_found -> Lwt.return_none | e -> Lwt.reraise e)

  let get_password_record_admin file who =
    get_password_record_generic (Admin file) who

  let get_password_record_election uuid who =
    get_password_record_generic (Election uuid) (Username who)

  let set_password_record_generic read write key data =
    let { username; salt; hashed; address } = password_record_of_string data in
    let update r =
      match address with
      | None ->
          let xs = match r with _ :: _ :: _ :: xs -> xs | _ -> [] in
          username :: salt :: hashed :: xs
      | Some address ->
          let xs = match r with _ :: _ :: _ :: _ :: xs -> xs | _ -> [] in
          username :: salt :: hashed :: address :: xs
    in
    let@ csv cont =
      let* x = read () in
      match x with
      | None -> Lwt.fail (Failure "missing password database")
      | Some x -> cont x
    in
    let* csv =
      Lwt_preemptive.detach (fun csv -> Csv.(input_all (of_string csv))) csv
    in
    let rec update_by_username x accu = function
      | (u :: _ as r) :: rs when String.lowercase_ascii u = x ->
          List.rev_append (update r :: accu) rs
      | r :: rs -> update_by_username x (r :: accu) rs
      | [] -> List.rev_append (update [] :: accu) []
    in
    let rec update_by_address x accu = function
      | (_ :: _ :: _ :: u :: _ as r) :: rs when String.lowercase_ascii u = x ->
          List.rev_append (update r :: accu) rs
      | r :: rs -> update_by_address x (r :: accu) rs
      | [] -> List.rev_append (update [] :: accu) []
    in
    let csv =
      match key with
      | Username u -> update_by_username (String.lowercase_ascii u) [] csv
      | Address u -> update_by_address (String.lowercase_ascii u) [] csv
    in
    let* csv =
      Lwt_preemptive.detach
        (fun csv ->
          let b = Buffer.create 1024 in
          Csv.(output_all (to_buffer b) csv);
          Buffer.contents b)
        csv
    in
    write csv

  let set_password_record_admin file key data =
    let* () =
      set_password_record_generic
        (fun () -> Filesystem.read_file file)
        (fun x -> Filesystem.write_file file x)
        key data
    in
    password_records_cache#remove (Admin file);
    Lwt.return_unit

  let set_password_record_election uuid who data =
    let* () =
      set_password_record_generic
        (fun () -> !get_password_file uuid)
        (fun x -> !set_password_file uuid x)
        (Username who) data
    in
    password_records_cache#remove (Election uuid);
    Lwt.return_unit

  let () =
    password_records_ops.get <- get_password_record_election;
    password_records_ops.set <- set_password_record_election

  (** {1 Generic operations} *)

  let ( !! ) x = Config.spool_dir // x
  let ( /// ) uuid x = !!(Uuid.unwrap uuid // x)

  type kind = Raw | Trim

  let files_of_directory d = Lwt_unix.files_of_directory d |> Lwt_stream.to_list

  let list_accounts () =
    let* xs = files_of_directory Config.accounts_dir in
    Lwt.return
    @@ List.fold_left
         (fun accu x ->
           match Filename.chop_suffix_opt ~suffix:".json" x with
           | None -> accu
           | Some x -> (
               match int_of_string_opt x with
               | None -> accu
               | Some x -> x :: accu))
         [] xs

  type election_file_props =
    | Concrete : string * kind -> election_file_props
    | Abstract : 'key abstract_file_ops * 'key -> election_file_props

  let get_election_file_props uuid = function
    | Draft -> Concrete ("draft.json", Trim)
    | State -> Concrete ("state.json", Trim)
    | Public_creds -> Concrete ("public_creds.json", Trim)
    | Private_creds -> Concrete ("private_creds.txt", Raw)
    | Dates_full -> Abstract (dates_ops, ())
    | Decryption_tokens -> Concrete ("decryption_tokens.json", Trim)
    | Metadata -> Concrete ("metadata.json", Trim)
    | Private_key -> Concrete ("private_key.json", Trim)
    | Private_keys -> Concrete ("private_keys.jsons", Raw)
    | Skipped_shufflers -> Concrete ("skipped_shufflers.json", Trim)
    | Shuffle_token -> Concrete ("shuffle_token.json", Trim)
    | Audit_cache -> Concrete ("audit_cache.json", Trim)
    | Last_event -> Concrete ("last_event.json", Trim)
    | Deleted -> Concrete ("deleted.json", Trim)
    | Public_archive -> Concrete (Uuid.unwrap uuid ^ ".bel", Raw)
    | Passwords -> Concrete ("passwords.csv", Raw)
    | Records -> Concrete ("records", Raw)
    | Voters -> Concrete ("voters.txt", Raw)
    | Confidential_archive -> Concrete ("archive.zip", Raw)
    | Private_creds_downloaded -> Concrete ("private_creds.downloaded", Raw)
    | Extended_record key -> Abstract (extended_records_ops, key)
    | Credential_mapping key -> Abstract (credential_mappings_ops, key)
    | Data key -> Abstract (data_ops, key)
    | Roots -> Abstract (roots_ops, ())
    | Voters_config -> Abstract (voters_config_ops, ())
    | Voter key -> Abstract (voters_ops, key)
    | Credential_weight key -> Abstract (credential_weights_ops, key)
    | Credential_user key -> Abstract (credential_users_ops, key)
    | Password key -> Abstract (password_records_ops, key)

  let clear_caches = function
    | Election (_, (Draft | State)) -> Elections_cache.clear ()
    | Account _ -> Accounts_cache.clear ()
    | _ -> ()

  let extended_records_filename = "extended_records.jsons"
  let credential_mappings_filename = "credential_mappings.jsons"

  type file_props =
    | Concrete : string * kind -> file_props
    | Abstract : 'key abstract_file_ops * uuid * 'key -> file_props
    | Admin_password : string * admin_password_file -> file_props

  let get_props = function
    | Spool_version -> Concrete (!!"version", Trim)
    | Account_counter -> Concrete (Config.accounts_dir // "counter", Trim)
    | Account id ->
        Concrete (Config.accounts_dir // Printf.sprintf "%d.json" id, Trim)
    | Election (uuid, f) -> (
        match get_election_file_props uuid f with
        | Concrete (fname, kind) -> Concrete (uuid /// fname, kind)
        | Abstract (ops, key) -> Abstract (ops, uuid, key))
    | Auth_db f -> Concrete (SMap.find f Config.maps, Raw)
    | Admin_password (file, key) ->
        Admin_password (SMap.find file Config.maps, key)

  let file_exists x =
    match get_props x with
    | Concrete (path, _) -> Filesystem.file_exists path
    | Abstract _ | Admin_password _ -> Lwt.fail @@ Not_implemented "file_exists"

  let list_elections () =
    let* xs = files_of_directory Config.spool_dir in
    Lwt_list.fold_left_s
      (fun accu x ->
        match Uuid.wrap x with
        | exception _ -> Lwt.return accu
        | uuid ->
            let* b = file_exists (Election (uuid, Deleted)) in
            if b then Lwt.return accu else Lwt.return (uuid :: accu))
      [] xs

  let get f =
    match get_props f with
    | Concrete (path, kind) -> (
        let* x = Filesystem.read_file path in
        match kind with
        | Raw -> Lwt.return x
        | Trim -> Lwt.return @@ Option.map String.trim x)
    | Abstract (ops, uuid, key) -> ops.get uuid key
    | Admin_password (file, key) -> get_password_record_admin file key

  let set f data =
    match get_props f with
    | Concrete (fname, kind) ->
        let data = match kind with Raw -> data | Trim -> data ^ "\n" in
        let* () = Filesystem.write_file fname data in
        let () = clear_caches f in
        Lwt.return_unit
    | Abstract (ops, uuid, key) -> ops.set uuid key data
    | Admin_password (file, key) -> set_password_record_admin file key data

  let () = get_password_file := fun uuid -> get (Election (uuid, Passwords))
  let () = set_password_file := fun uuid x -> set (Election (uuid, Passwords)) x

  module HashedFile = struct
    type t = file

    let equal = ( = )
    let hash = Hashtbl.hash
  end

  module TxId = Ephemeron.K1.Make (HashedFile)

  let txids_cells = TxId.create 1000

  let gen_txid =
    let counter = ref 0L in
    fun () ->
      let x = !counter in
      counter := Int64.add x 1L;
      x

  let get_txid_cell f =
    match TxId.find_opt txids_cells f with
    | Some x -> x
    | None ->
        let x = ref (-1L) in
        TxId.add txids_cells f x;
        x

  let update f =
    let* x = get f in
    let&* x = x in
    let txid_cell = get_txid_cell f in
    let txid = gen_txid () in
    txid_cell := txid;
    let set x =
      if !txid_cell = txid then set f x else Lwt.fail Race_condition
    in
    Lwt.return_some (x, set)

  let create = set
  let ensure = set

  let append_to_file fname lines =
    let open Lwt_io in
    let@ oc =
      with_file ~mode:Output ~flags:[ O_WRONLY; O_APPEND; O_CREAT ] fname
    in
    Lwt_list.iter_s (write_line oc) lines

  let del f =
    match get_props f with
    | Concrete (f, _) -> Filesystem.cleanup_file f
    | Abstract _ | Admin_password _ -> Lwt.fail @@ Not_implemented "del"

  let rmdir dir =
    let command = ("rm", [| "rm"; "-rf"; dir |]) in
    let* _ = Lwt_process.exec command in
    Lwt.return_unit

  let new_election () =
    let length = Config.uuid_length in
    let rec loop trials =
      if trials > 0 then
        let uuid = generate_token ~length () in
        Lwt.try_bind
          (fun () -> Lwt_unix.mkdir !!uuid 0o700)
          (fun () -> Lwt.return_some @@ Uuid.wrap uuid)
          (fun _ -> loop (trials - 1))
      else Lwt.return_none
    in
    loop 10

  let copy_file src dst =
    let open Lwt_io in
    chars_of_file src |> chars_to_file dst

  let try_copy_file src dst =
    let* b = Filesystem.file_exists src in
    if b then copy_file src dst else Lwt.return_unit

  let make_archive uuid =
    let uuid_s = Uuid.unwrap uuid in
    let* temp_dir =
      Lwt_preemptive.detach
        (fun () ->
          let temp_dir = Filename.temp_file "belenios" "archive" in
          Sys.remove temp_dir;
          Unix.mkdir temp_dir 0o700;
          Unix.mkdir (temp_dir // "public") 0o755;
          Unix.mkdir (temp_dir // "restricted") 0o700;
          temp_dir)
        ()
    in
    let* () =
      Lwt_list.iter_p
        (fun x -> try_copy_file (uuid /// x) (temp_dir // "public" // x))
        [ Uuid.unwrap uuid ^ ".bel" ]
    in
    let* () =
      Lwt_list.iter_p
        (fun x -> try_copy_file (uuid /// x) (temp_dir // "restricted" // x))
        [ "voters.txt"; "records" ]
    in
    let command =
      Printf.ksprintf Lwt_process.shell
        "cd \"%s\" && zip -r archive public restricted" temp_dir
    in
    let* r = Lwt_process.exec command in
    match r with
    | Unix.WEXITED 0 ->
        let fname = uuid /// "archive.zip" in
        let fname_new = fname ^ ".new" in
        let* () = copy_file (temp_dir // "archive.zip") fname_new in
        let* () = Lwt_unix.rename fname_new fname in
        rmdir temp_dir
    | _ ->
        Printf.ksprintf Ocsigen_messages.errlog
          "Error while creating archive.zip for election %s, temporary \
           directory left in %s"
          uuid_s temp_dir;
        Lwt.return_unit

  let get_archive uuid =
    let* state = get (Election (uuid, State)) in
    let final =
      match state with
      | None -> true
      | Some x -> (
          match election_state_of_string x with
          | `Tallied | `Archived -> true
          | _ -> false)
    in
    if final then
      let archive_name = Election (uuid, Confidential_archive) in
      let* b = file_exists archive_name in
      let* () = if not b then make_archive uuid else Lwt.return_unit in
      match get_props archive_name with
      | Concrete (f, _) -> Lwt.return f
      | _ -> Lwt.fail @@ Not_implemented "get_archive"
    else Lwt.fail Not_found

  let get_as_file = function
    | Election (_, (Public_archive | Private_creds)) as x -> (
        match get_props x with
        | Concrete (f, _) -> Lwt.return f
        | _ -> Lwt.fail @@ Not_implemented "get_as_file")
    | Election (uuid, Confidential_archive) -> get_archive uuid
    | _ -> Lwt.fail Not_found

  let account_id_promise = ref Lwt.return_unit

  let new_account_id () =
    let min = Config.account_id_min in
    let max = Config.account_id_max in
    let delta = max - min in
    let* () = !account_id_promise in
    let t, u = Lwt.task () in
    account_id_promise := t;
    let rec loop trials =
      if trials > 0 then
        let id = min + Stdlib.Random.int delta in
        let* b = file_exists (Account id) in
        if b then loop (trials - 1) else Lwt.return_some (id, u)
      else Lwt.fail Exit
    in
    Lwt.catch
      (fun () -> loop 10)
      (fun _ ->
        Lwt.wakeup_later u ();
        Lwt.return_none)

  (** {1 Views} *)

  let raw_dates_filename = "dates.json"
  let hide_result_filename = "hide_result"

  let get_dates uuid () =
    let* raw_dates = Filesystem.read_file (uuid /// raw_dates_filename) in
    let* hide_result = Filesystem.read_file (uuid /// hide_result_filename) in
    let hide_result =
      Option.map
        (String.trim >> datetime_of_string >> Datetime.to_unixfloat)
        hide_result
    in
    let dates =
      match (raw_dates, hide_result) with
      | None, None -> None
      | None, Some t ->
          Some
            {
              Belenios_storage_api.default_election_dates with
              e_date_publish = Some t;
            }
      | Some d, e_date_publish ->
          let d = election_dates_of_string (String.trim d) in
          Some
            {
              e_date_creation = Option.map Datetime.to_unixfloat d.e_creation;
              e_date_finalization =
                Option.map Datetime.to_unixfloat d.e_finalization;
              e_date_tally = Option.map Datetime.to_unixfloat d.e_tally;
              e_date_archive = Option.map Datetime.to_unixfloat d.e_archive;
              e_date_last_mail = Option.map Datetime.to_unixfloat d.e_last_mail;
              e_date_auto_open = Option.map Datetime.to_unixfloat d.e_auto_open;
              e_date_auto_close =
                Option.map Datetime.to_unixfloat d.e_auto_close;
              e_date_publish;
            }
    in
    Lwt.return @@ Option.map Belenios_storage_api.string_of_election_dates dates

  let set_dates uuid () dates =
    let d = Belenios_storage_api.election_dates_of_string dates in
    let* () =
      let filename = uuid /// hide_result_filename in
      match d.e_date_publish with
      | None -> Filesystem.cleanup_file filename
      | Some t ->
          let t = t |> Datetime.from_unixfloat |> string_of_datetime in
          Filesystem.write_file filename (t ^ "\n")
    in
    let filename = uuid /// raw_dates_filename in
    let dates =
      {
        e_creation = Option.map Datetime.from_unixfloat d.e_date_creation;
        e_finalization =
          Option.map Datetime.from_unixfloat d.e_date_finalization;
        e_tally = Option.map Datetime.from_unixfloat d.e_date_tally;
        e_archive = Option.map Datetime.from_unixfloat d.e_date_archive;
        e_last_mail = Option.map Datetime.from_unixfloat d.e_date_last_mail;
        e_auto_open = Option.map Datetime.from_unixfloat d.e_date_auto_open;
        e_auto_close = Option.map Datetime.from_unixfloat d.e_date_auto_close;
      }
    in
    Filesystem.write_file filename (string_of_election_dates dates ^ "\n")

  let () =
    dates_ops.get <- get_dates;
    dates_ops.set <- set_dates

  module ExtendedRecordsCacheTypes = struct
    type key = uuid
    type value = (datetime * string) SMap.t
  end

  module ExtendedRecordsCache = Ocsigen_cache.Make (ExtendedRecordsCacheTypes)

  let raw_get_extended_records uuid =
    let* x = Filesystem.read_file (uuid /// extended_records_filename) in
    let x = match x with None -> [] | Some x -> split_lines x in
    Lwt_list.fold_left_s
      (fun accu x ->
        let x = extended_record_of_string x in
        Lwt.return @@ SMap.add x.r_username (x.r_date, x.r_credential) accu)
      SMap.empty x

  let dump_extended_records uuid rs =
    let rs = SMap.bindings rs in
    let extended_records =
      List.map
        (fun (r_username, (r_date, r_credential)) ->
          { r_username; r_date; r_credential } |> string_of_extended_record)
        rs
      |> join_lines
    in
    let records =
      List.map
        (fun (u, (d, _)) -> Printf.sprintf "%s %S" (string_of_datetime d) u)
        rs
      |> join_lines
    in
    let* () =
      Filesystem.write_file
        (uuid /// extended_records_filename)
        extended_records
    in
    set (Election (uuid, Records)) records

  let extended_records_cache =
    new ExtendedRecordsCache.cache raw_get_extended_records ~timer:3600. 10

  let extended_records_deferrer =
    Indexed_defer.create mutexes (function
      | None -> Lwt.return_unit
      | Some uuid ->
          let* x = extended_records_cache#find uuid in
          dump_extended_records uuid x)

  let find_extended_record uuid username =
    let* rs = extended_records_cache#find uuid in
    Lwt.return (SMap.find_opt username rs)

  let add_extended_record uuid username r =
    let* rs = extended_records_cache#find uuid in
    let rs = SMap.add username r rs in
    extended_records_cache#add uuid rs;
    let* () =
      let r_date, r_credential = r in
      { r_username = username; r_date; r_credential }
      |> string_of_extended_record
      |> (fun x -> [ x ])
      |> append_to_file (uuid /// extended_records_filename)
    in
    Indexed_defer.defer extended_records_deferrer (Some uuid);
    Lwt.return_unit

  let () =
    extended_records_ops.get <-
      (fun uuid r_username ->
        let*& r_date, r_credential = find_extended_record uuid r_username in
        Lwt.return_some
        @@ string_of_extended_record { r_username; r_date; r_credential });
    extended_records_ops.set <-
      (fun uuid username data ->
        let { r_username; r_date; r_credential } =
          extended_record_of_string data
        in
        if username = r_username then
          add_extended_record uuid username (r_date, r_credential)
        else Lwt.fail @@ Not_implemented "extended_records_ops.set")

  module CredMappingsCacheTypes = struct
    type key = uuid
    type value = string option SMap.t
  end

  module CredMappingsCache = Ocsigen_cache.Make (CredMappingsCacheTypes)

  let raw_get_credential_mappings uuid =
    let* x = Filesystem.read_file (uuid /// credential_mappings_filename) in
    let x = match x with None -> [] | Some x -> split_lines x in
    Lwt_list.fold_left_s
      (fun accu x ->
        let x = credential_mapping_of_string x in
        Lwt.return @@ SMap.add x.c_credential x.c_ballot accu)
      SMap.empty x

  let dump_credential_mappings uuid xs =
    SMap.fold
      (fun c_credential c_ballot accu -> { c_credential; c_ballot } :: accu)
      xs []
    |> List.rev_map string_of_credential_mapping
    |> join_lines
    |> Filesystem.write_file (uuid /// credential_mappings_filename)

  let credential_mappings_cache =
    new CredMappingsCache.cache raw_get_credential_mappings ~timer:3600. 10

  let credential_mappings_deferrer =
    Indexed_defer.create mutexes (function
      | None -> Lwt.return_unit
      | Some uuid ->
          let* x = credential_mappings_cache#find uuid in
          dump_credential_mappings uuid x)

  let init_credential_mapping uuid =
    let* file = get (Election (uuid, Public_creds)) in
    match file with
    | Some x ->
        let public_credentials =
          public_credentials_of_string x |> List.map strip_public_credential
        in
        let xs =
          List.fold_left
            (fun accu x ->
              let x = (parse_public_credential Fun.id x).credential in
              if SMap.mem x accu then
                failwith "trying to add duplicate credential"
              else SMap.add x None accu)
            SMap.empty public_credentials
        in
        credential_mappings_cache#add uuid xs;
        let* () = dump_credential_mappings uuid xs in
        Lwt.return public_credentials
    | None -> Lwt.fail @@ Election_not_found (uuid, "init_credential_mapping")

  let find_credential_mapping uuid cred =
    let* xs = credential_mappings_cache#find uuid in
    Lwt.return @@ SMap.find_opt cred xs

  let add_credential_mapping uuid cred mapping =
    let* xs = credential_mappings_cache#find uuid in
    let xs = SMap.add cred mapping xs in
    credential_mappings_cache#add uuid xs;
    let* () =
      { c_credential = cred; c_ballot = mapping }
      |> string_of_credential_mapping
      |> (fun x -> [ x ])
      |> append_to_file (uuid /// credential_mappings_filename)
    in
    Indexed_defer.defer credential_mappings_deferrer (Some uuid);
    Lwt.return_unit

  let () =
    credential_mappings_ops.get <-
      (fun uuid cred ->
        let* x = find_credential_mapping uuid cred in
        let&* x = x in
        Lwt.return_some @@ Option.value ~default:"" x);
    credential_mappings_ops.set <-
      (fun uuid cred data ->
        let mapping = if data = "" then None else Some data in
        add_credential_mapping uuid cred mapping)

  type voters = {
    has_explicit_weights : bool;
    username_or_address : [ `Username | `Address ];
    voter_map : Voter.t SMap.t;
  }

  module VoterCacheTypes = struct
    type key = uuid
    type value = voters
  end

  module VoterCache = Ocsigen_cache.Make (VoterCacheTypes)

  let get_all_voters uuid =
    let* x = get (Election (uuid, Voters)) in
    match x with
    | None -> Lwt.return []
    | Some x -> Lwt.return (Voter.list_of_string x)

  let raw_get_voter_cache uuid =
    let* voters = get_all_voters uuid in
    let voter_map =
      List.fold_left
        (fun accu x ->
          let _, login, _ = Voter.get x in
          SMap.add (String.lowercase_ascii login) x accu)
        SMap.empty voters
    in
    let has_explicit_weights = Voter.has_explicit_weights voters in
    let username_or_address =
      match voters with
      | [] -> `Username
      | (_, { login; _ }) :: _ -> (
          match login with None -> `Address | Some _ -> `Username)
    in
    Lwt.return { has_explicit_weights; username_or_address; voter_map }

  let voter_cache = new VoterCache.cache raw_get_voter_cache ~timer:3600. 10

  let get_voters uuid =
    Lwt.catch
      (fun () ->
        let* x = voter_cache#find uuid in
        Lwt.return_some x)
      (fun _ -> Lwt.return_none)

  let get_voters_config uuid () =
    let* x = get_voters uuid in
    let&* { has_explicit_weights; username_or_address; voter_map } = x in
    let nb_voters = SMap.cardinal voter_map in
    let x : voters_config =
      { has_explicit_weights; username_or_address; nb_voters }
    in
    Lwt.return_some @@ string_of_voters_config x

  let get_voter uuid id =
    let* x = get_voters uuid in
    let&* { voter_map; _ } = x in
    let&* x = SMap.find_opt (String.lowercase_ascii id) voter_map in
    Lwt.return_some @@ Voter.to_string x

  let () =
    voters_config_ops.get <- get_voters_config;
    voters_ops.get <- get_voter

  module CredCacheTypes = struct
    type key = uuid
    type value = { cred_map : (string option * Weight.t) SMap.t }
  end

  module CredCache = Ocsigen_cache.Make (CredCacheTypes)

  let raw_get_credential_cache uuid =
    let@ public_creds cont =
      let* x = get (Election (uuid, Public_creds)) in
      match x with
      | None ->
          (* public credentials mapping is no longer available, use
             the public view from the archive *)
          let fail () =
            Lwt.fail (Election_not_found (uuid, "raw_get_credential_cache"))
          in
          let ( let& ) x f = match x with None -> fail () | Some x -> f x in
          let* x = get (Election (uuid, Roots)) in
          let& x = x in
          let roots = roots_of_string x in
          let& h = roots.roots_setup_data in
          let* x = get (Election (uuid, Data h)) in
          let& x = x in
          let setup_data = setup_data_of_string x in
          let* x = get (Election (uuid, Data setup_data.setup_credentials)) in
          let& x = x in
          cont x
      | Some x -> cont x
    in
    let public_creds = public_credentials_of_string public_creds in
    let cred_map =
      List.fold_left
        (fun cred_map x ->
          let p = parse_public_credential Fun.id x in
          SMap.add p.credential
            (p.username, Option.value ~default:Weight.one p.weight)
            cred_map)
        SMap.empty public_creds
    in
    Lwt.return CredCacheTypes.{ cred_map }

  let credential_cache =
    new CredCache.cache raw_get_credential_cache ~timer:3600. 10

  let get_credential_user uuid cred =
    Lwt.catch
      (fun () ->
        let* x = credential_cache#find uuid in
        let&* x, _ = SMap.find_opt cred x.cred_map in
        Lwt.return x)
      (fun _ -> Lwt.return_none)

  let get_credential_weight uuid cred =
    Lwt.catch
      (fun () ->
        let* x = credential_cache#find uuid in
        let&* _, x = SMap.find_opt cred x.cred_map in
        Lwt.return_some @@ Weight.to_string x)
      (fun _ -> Lwt.return_none)

  let () =
    credential_users_ops.get <- get_credential_user;
    credential_weights_ops.get <- get_credential_weight

  (** {1 Cleaning operations} *)

  let delete_election uuid =
    let* () = rmdir !!(Uuid.unwrap uuid) in
    Elections_cache.clear ();
    Lwt.return_unit

  let delete_sensitive_data uuid =
    let* () =
      Lwt_list.iter_p
        (fun x -> Filesystem.cleanup_file (uuid /// x))
        [ extended_records_filename; credential_mappings_filename ]
    in
    let* () =
      Lwt_list.iter_p
        (fun x -> del (Election (uuid, x)))
        [ State; Private_key; Private_keys; Decryption_tokens; Public_creds ]
    in
    Elections_cache.clear ();
    Lwt.return_unit

  let delete_live_data uuid =
    let* () =
      Lwt_list.iter_p
        (fun x -> del (Election (uuid, x)))
        [
          Last_event;
          Metadata;
          Audit_cache;
          Shuffle_token;
          Skipped_shufflers;
          Public_archive;
          Passwords;
          Records;
          Voters;
          Confidential_archive;
        ]
    in
    let* () =
      Lwt_list.iter_p
        (fun x -> Filesystem.cleanup_file (uuid /// x))
        [ raw_dates_filename; hide_result_filename ]
    in
    Elections_cache.clear ();
    Lwt.return_unit

  (** {1 Public archive operations} *)

  let get_last_event uuid =
    let*& x = get (Election (uuid, Last_event)) in
    Lwt.return_some @@ last_event_of_string x

  let set_last_event uuid x =
    set (Election (uuid, Last_event)) (string_of_last_event x)

  let block_size = Archive.block_size
  let block_sizeL = Int64.of_int block_size

  type index = {
    timeout : Lwt_timeout.t;
    map : (hash, location) Hashtbl.t;
    mutable roots : roots;
    timestamp : int64;
  }

  module IoReader = struct
    include Lwt

    let yield = Lwt.pause

    open Lwt_io

    type file = input_channel

    let get_pos ic = Lwt.return @@ position ic
    let set_pos = set_position
    let read_block ic buffer = read_into_exactly ic buffer 0 block_size
  end

  module Reader = Archive.MakeReader (IoReader)

  module IoWriter = struct
    include Lwt

    let yield = Lwt.pause

    open Lwt_io

    (* `Lwt_io`'s position does not work with files opened in append
       mode, so we implement it here *)

    type file = { channel : output_channel; mutable position : int64 }

    let get_pos oc = Lwt.return oc.position

    let write_block oc buffer =
      let* () = write_from_exactly oc.channel buffer 0 block_size in
      oc.position <- Int64.add oc.position block_sizeL;
      Lwt.return_unit
  end

  module Writer = Archive.MakeWriter (IoWriter)

  let indexes = Hashtbl.create 100

  let write_header ~filename ~header =
    let open Lwt_unix in
    let* fd = openfile filename [ O_WRONLY; O_APPEND; O_CREAT ] 0o644 in
    Lwt.finalize
      (fun () ->
        let* () = LargeFile.ftruncate fd 0L in
        let oc =
          { IoWriter.channel = Lwt_io.of_fd ~mode:Output fd; position = 0L }
        in
        let* () = Writer.write_header oc header in
        Lwt_io.flush oc.channel)
      (fun () -> close fd)

  let build_roots ~size ~pos filename =
    let r = Hashtbl.create size in
    let@ () =
     fun cont ->
      if pos > 0L then cont ()
      else
        let header = Archive.new_header () in
        let* () = write_header ~filename ~header in
        Lwt.return_some (r, Events.empty_roots, Archive.get_timestamp header)
    in
    let@ fd cont =
      Lwt.try_bind
        (fun () -> Lwt_unix.openfile filename [ Unix.O_RDONLY ] 0o644)
        cont
        (function
          | Unix.Unix_error (ENOENT, _, _) -> Lwt.return_none
          | e -> Lwt.reraise e)
    in
    let open Lwt_io in
    let ic = of_fd ~mode:Input fd in
    let* header = Reader.read_header ic in
    let rec loop accu =
      let location_offset = position ic in
      if location_offset < pos then (
        let* record = Reader.read_record ic in
        let accu =
          match record.typ with
          | Data -> accu
          | Event event -> Events.update_roots record.hash event accu
        in
        Hashtbl.add r record.hash record.location;
        loop accu)
      else Lwt.return_some (r, accu, Archive.get_timestamp header)
    in
    Lwt.finalize (fun () -> loop Events.empty_roots) (fun () -> close ic)

  exception Creation_not_requested

  let do_get_index ~creat ~uuid =
    let* last = get_last_event uuid in
    let size, pos =
      match last with
      | None when creat -> (100, 0L)
      | None -> raise Creation_not_requested
      | Some x -> (x.last_height + 100, x.last_pos)
    in
    let* filename = get_as_file (Election (uuid, Public_archive)) in
    let*& map, roots, timestamp = build_roots ~size ~pos filename in
    let remove () = Hashtbl.remove indexes uuid in
    let timeout = Lwt_timeout.create 3600 remove in
    let r = { timeout; map; roots; timestamp } in
    Hashtbl.add indexes uuid r;
    Lwt.return_some r

  let get_index ~creat uuid =
    let*& r =
      match Hashtbl.find_opt indexes uuid with
      | Some r -> Lwt.return_some r
      | None -> do_get_index ~creat ~uuid
    in
    Lwt_timeout.start r.timeout;
    Lwt.return_some r

  let raw_append ~filename ~timestamp offset xs =
    let open Lwt_unix in
    let* fd = openfile filename [ O_WRONLY; O_APPEND ] 0o644 in
    Lwt.finalize
      (fun () ->
        let* () =
          let* pos = LargeFile.lseek fd 0L SEEK_END in
          if pos = offset then Lwt.return_unit
          else LargeFile.ftruncate fd offset
        in
        let oc =
          { IoWriter.channel = Lwt_io.of_fd ~mode:Output fd; position = offset }
        in
        let* records =
          Lwt_list.fold_left_s
            (fun accu (typ, x) ->
              let* record = Writer.write_record oc ~timestamp typ x in
              Lwt.return @@ (record :: accu))
            [] xs
        in
        let* () = Lwt_io.flush oc.channel in
        let* () = fsync fd in
        Lwt.return (oc.position, records))
      (fun () -> close fd)

  let gethash ~index ~filename x =
    let&* i = Hashtbl.find_opt index x in
    let open Lwt_unix in
    let@ fd cont =
      Lwt.try_bind
        (fun () -> openfile filename [ O_RDONLY ] 0o644)
        cont
        (function
          | Unix.Unix_error (ENOENT, _, _) -> Lwt.return_none
          | e -> Lwt.reraise e)
    in
    Lwt.finalize
      (fun () ->
        let* _ = LargeFile.lseek fd i.location_offset SEEK_SET in
        assert (i.location_length <= Int64.of_int Sys.max_string_length);
        let length = Int64.to_int i.location_length in
        let buffer = Bytes.create length in
        let ic = Lwt_io.of_fd ~mode:Input fd in
        let* () = Lwt_io.read_into_exactly ic buffer 0 length in
        Lwt.return_some @@ Bytes.to_string buffer)
      (fun () -> close fd)

  let get_data uuid x =
    Lwt.try_bind
      (fun () -> get_index ~creat:false uuid)
      (fun r ->
        let&* r = r in
        let* filename = get_as_file (Election (uuid, Public_archive)) in
        gethash ~index:r.map ~filename x)
      (function
        | Creation_not_requested -> Lwt.return_none | e -> Lwt.reraise e)

  let () = data_ops.get <- get_data

  let get_roots uuid () =
    Lwt.try_bind
      (fun () -> get_index ~creat:false uuid)
      (fun r ->
        let&* r = r in
        Lwt.return_some @@ string_of_roots r.roots)
      (function
        | Creation_not_requested -> Lwt.return_none | e -> Lwt.reraise e)

  let () = roots_ops.get <- get_roots

  let append uuid ?last ops =
    let@ last cont =
      let* x = get_last_event uuid in
      match last with
      | None -> cont x
      | Some last -> if x = Some last then cont x else Lwt.return_false
    in
    let@ index cont =
      let* i = get_index ~creat:true uuid in
      match i with Some i -> cont i | None -> Lwt.return_false
    in
    let event_parent, event_height, pos =
      match last with
      | None -> (None, -1, 1024L (* header size *))
      | Some x -> (Some x.last_hash, x.last_height, x.last_pos)
    in
    let last_hash, last_height, roots, items =
      List.fold_left
        (fun (event_parent, event_height, roots, accu) x ->
          match x with
          | Event (event_typ, event_payload) ->
              let event_height = event_height + 1 in
              let event =
                { event_parent; event_height; event_typ; event_payload }
              in
              let event_s = string_of_event event in
              let event_h = Hash.hash_string event_s in
              let accu = (Archive.Event event, event_s) :: accu in
              ( Some event_h,
                event_height,
                Events.update_roots event_h event roots,
                accu )
          | Data payload ->
              let accu = (Archive.Data, payload) :: accu in
              (event_parent, event_height, roots, accu))
        (event_parent, event_height, index.roots, [])
        ops
    in
    let last_hash = match last_hash with None -> assert false | Some x -> x in
    let items = List.rev items in
    let* last_pos, records =
      let* filename = get_as_file (Election (uuid, Public_archive)) in
      raw_append ~filename ~timestamp:index.timestamp pos items
    in
    let* () = set_last_event uuid { last_hash; last_height; last_pos } in
    List.iter (fun r -> Hashtbl.add index.map r.Archive.hash r.location) records;
    index.roots <- roots;
    Lwt.return_true

  let make set =
    let with_lock uuid f =
      let* () = Mutex_set.lock set uuid in
      f ()
    in
    let with_lock_file x f =
      let uuid = match x with Election (uuid, _) -> Some uuid | _ -> None in
      with_lock uuid f
    in
    let module X = struct
      let get_as_file f = with_lock_file f (fun () -> get_as_file f)
      let get f = with_lock_file f (fun () -> get f)
      let update f = with_lock_file f (fun () -> update f)
      let create f x = with_lock_file f (fun () -> create f x)
      let ensure f x = with_lock_file f (fun () -> ensure f x)
      let del f = with_lock_file f (fun () -> del f)
      let list_accounts () = with_lock None list_accounts
      let list_elections () = with_lock None list_elections
      let new_election () = with_lock None new_election
      let new_account_id () = with_lock None new_account_id

      let init_credential_mapping uuid =
        with_lock (Some uuid) (fun () -> init_credential_mapping uuid)

      let delete_election uuid =
        with_lock (Some uuid) (fun () -> delete_election uuid)

      let delete_sensitive_data uuid =
        with_lock (Some uuid) (fun () -> delete_sensitive_data uuid)

      let delete_live_data uuid =
        with_lock (Some uuid) (fun () -> delete_live_data uuid)

      let append uuid ?last ops =
        with_lock (Some uuid) (fun () -> append uuid ?last ops)
    end in
    (module X : BACKEND0)
end

type with_transaction_ref = {
  mutable with_transaction : 'a. ((module BACKEND0) -> 'a Lwt.t) -> 'a Lwt.t;
}

module Make (Config : CONFIG) : Storage.S = struct
  let with_transaction_ref =
    {
      with_transaction =
        (fun _ -> Lwt.fail @@ Not_implemented "with_transaction");
    }

  module Accounts_input = struct
    type session = (module BACKEND0)

    let list_accounts s =
      let module S = (val s : BACKEND0) in
      S.list_accounts ()

    let get_account_by_id s id =
      let module S = (val s : BACKEND0) in
      let* x = S.get (Account id) in
      Lwt.return @@ Option.map account_of_string x

    let with_transaction f = with_transaction_ref.with_transaction f
  end

  module Elections_input = struct
    type session = (module BACKEND0)

    let get s f =
      let module S = (val s : BACKEND0) in
      S.get f

    let list_elections s =
      let module S = (val s : BACKEND0) in
      S.list_elections ()

    let with_transaction f = with_transaction_ref.with_transaction f
  end

  module Accounts_cache = Accounts_cache.Make (Accounts_input) ()
  module Elections_cache = Elections_cache.Make (Elections_input) ()
  module B = MakeBackend (Config) (Accounts_cache.Clear) (Elections_cache.Clear)

  let with_transaction_generic f =
    let set = Mutex_set.create B.mutexes in
    let x = B.make set in
    Lwt.finalize
      (fun () ->
        match f with
        | `Restricted f -> f x
        | `Full f ->
            let module X = (val x) in
            f (module X : BACKEND))
      (fun () ->
        Mutex_set.unlock set;
        Lwt.return_unit)

  let with_transaction f = with_transaction_generic (`Full f)

  let () =
    with_transaction_ref.with_transaction <-
      (fun f -> with_transaction_generic (`Restricted f))

  let get_user_id = Accounts_cache.get_user_id
  let get_elections_by_owner = Elections_cache.get_elections_by_owner
  let get_next_actions = Elections_cache.get_next_actions
end

let backend_name = "filesystem"

let make_backend (config : Xml.xml list) =
  let spool_dir = ref None in
  let accounts_dir = ref None in
  let uuid_length = ref Uuid.min_length in
  let account_id_min = ref 100000000 in
  let account_id_max = ref 999999999 in
  let maps = ref SMap.empty in
  let () =
    config
    |> List.iter @@ function
       | Xml.PCData x ->
           if String.trim x <> "" then
             Printf.ksprintf failwith "unexpected pcdata in configuration of %s"
               backend_name
       | Element ("uuid", [ ("length", length) ], []) ->
           let length = int_of_string length in
           if length >= Uuid.min_length then uuid_length := length
           else failwith "UUID length is too small"
       | Element ("spool", [ ("dir", dir) ], []) -> spool_dir := Some dir
       | Element ("accounts", [ ("dir", dir) ], []) -> accounts_dir := Some dir
       | Element ("account-ids", [ ("min", min); ("max", max) ], []) ->
           let min = int_of_string min and max = int_of_string max in
           if min > 0 && max - min > 4000000 then (
             (* birthday paradox: room for 2000 accounts *)
             account_id_min := min;
             account_id_max := max)
           else failwith "account-ids delta is not big enough"
       | Element ("map", [ ("from", from); ("to", to_) ], []) ->
           maps := SMap.add from to_ !maps
       | Element (tag, _, _) ->
           Printf.ksprintf failwith "unexpected tag <%s> in configuration of %s"
             tag backend_name
  in
  let spool_dir =
    match !spool_dir with
    | Some d -> d
    | None ->
        Printf.ksprintf failwith "missing <spool> in configuration of %s"
          backend_name
  in
  let accounts_dir =
    match !accounts_dir with
    | Some d -> d
    | None ->
        Printf.ksprintf failwith "missing <accounts> in configuration of %s"
          backend_name
  in
  let module Config = struct
    let uuid_length = !uuid_length
    let account_id_min = !account_id_min
    let account_id_max = !account_id_max
    let spool_dir = spool_dir
    let accounts_dir = accounts_dir
    let maps = !maps
  end in
  let module X = Make (Config) in
  (module X : Storage.S)

let register () = Storage.register_backend backend_name make_backend
