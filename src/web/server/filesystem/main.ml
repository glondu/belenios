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
open Belenios_storage_api
open Belenios_server_core
open Storage
open Types
open Serializable_j

let () = Stdlib.Random.self_init ()
let ( let&** ) x f = match x with None -> Lopt.none_lwt | Some x -> f x

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

  type ('key, 'a) abstract_file_ops = {
    mutable get : uuid -> 'key -> 'a v Lwt.t;
    mutable set : uuid -> 'key -> 'a v -> unit Lwt.t;
    mutable del : uuid -> 'key -> unit Lwt.t;
  }

  let make_uninitialized_ops what =
    let e x = Lwt.fail @@ Not_implemented (Printf.sprintf "%s.%s" what x) in
    {
      get = (fun _ _ -> e "get");
      set = (fun _ _ _ -> e "set");
      del = (fun _ _ -> e "del");
    }

  (** {1 Forward references} *)

  let draft_ops : (_, Belenios_storage_api.draft_election) abstract_file_ops =
    make_uninitialized_ops "draft_ops"

  let state_state_ops : (_, Belenios_storage_api.state_state) abstract_file_ops
      =
    make_uninitialized_ops "state_state_ops"

  let dates_ops : (_, Belenios_storage_api.election_dates) abstract_file_ops =
    make_uninitialized_ops "dates_ops"

  let records_ops : (_, Belenios_storage_api.election_records) abstract_file_ops
      =
    make_uninitialized_ops "records_ops"

  let extended_records_ops :
      (_, Belenios_storage_api.extended_record) abstract_file_ops =
    make_uninitialized_ops "extended_records_ops"

  let credential_mappings_ops : (_, credential_mapping) abstract_file_ops =
    make_uninitialized_ops "credential_mappings_ops"

  let data_ops : (_, string) abstract_file_ops =
    make_uninitialized_ops "data_ops"

  let roots_ops : (_, roots) abstract_file_ops =
    make_uninitialized_ops "roots_ops"

  let voters_config_ops : (_, voters_config) abstract_file_ops =
    make_uninitialized_ops "voters_config_ops"

  let voters_ops : (_, Voter.t) abstract_file_ops =
    make_uninitialized_ops "voters_ops"

  let credential_weights_ops : (_, Weight.t) abstract_file_ops =
    make_uninitialized_ops "credential_weights_ops"

  let credential_users_ops : (_, string) abstract_file_ops =
    make_uninitialized_ops "credential_users_ops"

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
      | Admin file ->
          let*& csv = Filesystem.read_file file in
          let* csv = Lwt_preemptive.detach csv_of_string csv in
          Lwt.return_some csv
      | Election uuid -> !get_password_file uuid
    in
    let* csv =
      match csv with
      | None -> Lwt.fail Password_db_not_found
      | Some x -> Lwt.return x
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
          match (who : admin_password_file) with
          | Username u' -> (u', u)
          | Address a' -> (a', a)
        in
        let&** r = SMap.find_opt (String.lowercase_ascii key) map in
        r |> Lopt.some_value string_of_password_record |> Lwt.return)
      (function Password_db_not_found -> Lopt.none_lwt | e -> Lwt.reraise e)

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
      match (key : admin_password_file) with
      | Username u -> update_by_username (String.lowercase_ascii u) [] csv
      | Address u -> update_by_address (String.lowercase_ascii u) [] csv
    in
    write csv

  let set_password_record_admin file key data =
    match Lopt.get_string data with
    | None -> assert false
    | Some data ->
        let* () =
          set_password_record_generic
            (fun () ->
              let*& csv = Filesystem.read_file file in
              let* csv = Lwt_preemptive.detach csv_of_string csv in
              Lwt.return_some csv)
            (fun csv ->
              let* csv = Lwt_preemptive.detach string_of_csv csv in
              Filesystem.write_file file csv)
            key data
        in
        password_records_cache#remove (Admin file);
        Lwt.return_unit

  let set_password_record_election uuid who data =
    match Lopt.get_string data with
    | None -> assert false
    | Some data ->
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

  let cleanup_files uuid xs =
    Lwt_list.iter_p (fun x -> Filesystem.cleanup_file (uuid /// x)) xs

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

  type _ election_file_props =
    | Concrete :
        string * kind * 'a Converters.t option
        -> 'a election_file_props
    | Abstract : ('key, 'a) abstract_file_ops * 'key -> 'a election_file_props

  let get_election_file_props uuid (type t) :
      t election_file -> t election_file_props = function
    | Draft -> Abstract (draft_ops, ())
    | State -> Concrete ("state.json", Trim, None)
    | State_state -> Abstract (state_state_ops, ())
    | Public_creds -> Concrete ("public_creds.json", Trim, None)
    | Private_creds -> Concrete ("private_creds.txt", Raw, None)
    | Dates -> Abstract (dates_ops, ())
    | Metadata -> Concrete ("metadata.json", Trim, None)
    | Private_key -> Concrete ("private_key.json", Trim, None)
    | Private_keys -> Concrete ("private_keys.jsons", Raw, None)
    | Audit_cache -> Concrete ("audit_cache.json", Trim, None)
    | Last_event -> Concrete ("last_event.json", Trim, None)
    | Public_archive -> Concrete (Uuid.unwrap uuid ^ ".bel", Raw, None)
    | Passwords -> Concrete ("passwords.csv", Raw, None)
    | Records -> Abstract (records_ops, ())
    | Voters -> Concrete ("voters.txt", Raw, None)
    | Confidential_archive -> Concrete ("archive.zip", Raw, None)
    | Extended_record key -> Abstract (extended_records_ops, key)
    | Credential_mapping key -> Abstract (credential_mappings_ops, key)
    | Data key -> Abstract (data_ops, key)
    | Roots -> Abstract (roots_ops, ())
    | Voters_config -> Abstract (voters_config_ops, ())
    | Voter key -> Abstract (voters_ops, key)
    | Credential_weight key -> Abstract (credential_weights_ops, key)
    | Credential_user key -> Abstract (credential_users_ops, key)
    | Password key -> Abstract (password_records_ops, key)

  let clear_caches (type t) : t file -> _ = function
    | Election (_, (Draft | State)) -> Elections_cache.clear ()
    | Account _ -> Accounts_cache.clear ()
    | _ -> ()

  let extended_records_filename = "extended_records.jsons"
  let credential_mappings_filename = "credential_mappings.jsons"

  type _ file_props =
    | Concrete : string * kind * 'a Converters.t option -> 'a file_props
    | Abstract : ('key, 'a) abstract_file_ops * uuid * 'key -> 'a file_props
    | Admin_password :
        string * admin_password_file
        -> password_record file_props

  let get_props (type t) : t file -> t file_props = function
    | Account id ->
        Concrete
          ( Config.accounts_dir // Printf.sprintf "%d.json" id,
            Trim,
            Some Converters.account )
    | Election (uuid, f) -> (
        match get_election_file_props uuid f with
        | Concrete (fname, kind, convert) ->
            Concrete (uuid /// fname, kind, convert)
        | Abstract (ops, key) -> Abstract (ops, uuid, key))
    | Auth_db f -> Concrete (SMap.find f Config.maps, Raw, None)
    | Admin_password (file, key) ->
        Admin_password (SMap.find file Config.maps, key)

  let file_exists (type t) (x : t file) =
    match get_props x with
    | Concrete (path, _, _) -> Filesystem.file_exists path
    | Abstract _ | Admin_password _ -> Lwt.fail @@ Not_implemented "file_exists"

  let deleted_filename = "deleted.json"

  let list_elections () =
    let* xs = files_of_directory Config.spool_dir in
    Lwt_list.fold_left_s
      (fun accu x ->
        match Uuid.wrap x with
        | exception _ -> Lwt.return accu
        | uuid ->
            let* b = Filesystem.file_exists (uuid /// deleted_filename) in
            if b then Lwt.return accu else Lwt.return (uuid :: accu))
      [] xs

  let get (type t) (f : t file) : t v Lwt.t =
    match get_props f with
    | Concrete (path, kind, convert) ->
        let* x = Filesystem.read_file path in
        let&** x = x in
        (match kind with Raw -> x | Trim -> String.trim x)
        |> (match convert with
           | None -> some_string_or_value f String
           | Some { of_string; _ } -> of_string >> some_string_or_value f Value)
        |> Lwt.return
    | Abstract (ops, uuid, key) -> ops.get uuid key
    | Admin_password (file, key) -> get_password_record_admin file key

  let set (type t u) (f : t file) (spec : (t, u) string_or_value_spec)
      (data : u) =
    let data = some_string_or_value f spec data in
    match get_props f with
    | Concrete (fname, kind, convert) -> (
        match
          match convert with
          | None -> Lopt.get_string data
          | Some { to_string; _ } -> Lopt.get_value data |> Option.map to_string
        with
        | None -> assert false
        | Some data ->
            let data = match kind with Raw -> data | Trim -> data ^ "\n" in
            let* () = Filesystem.write_file fname data in
            let () = clear_caches f in
            Lwt.return_unit)
    | Abstract (ops, uuid, key) -> ops.set uuid key data
    | Admin_password (file, key) -> set_password_record_admin file key data

  let () =
    get_password_file :=
      fun uuid ->
        let* x = get (Election (uuid, Passwords)) in
        let&* x = Lopt.get_value x in
        Lwt.return_some x

  let () =
    set_password_file := fun uuid x -> set (Election (uuid, Passwords)) Value x

  module HashedFile = struct
    type t = File : 'a file -> t

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

  let update (type t) (f : t file) =
    let* x = get f in
    let txid_cell = get_txid_cell (File f) in
    let txid = gen_txid () in
    txid_cell := txid;
    let set spec x =
      if !txid_cell = txid then set f spec x else Lwt.fail Race_condition
    in
    Lwt.return_some (x, set)

  let append_to_file fname lines =
    let open Lwt_io in
    let@ oc =
      with_file ~mode:Output ~flags:[ O_WRONLY; O_APPEND; O_CREAT ] fname
    in
    Lwt_list.iter_s (write_line oc) lines

  let del (type t) (f : t file) =
    match get_props f with
    | Concrete (f, _, _) -> Filesystem.cleanup_file f
    | Abstract (ops, uuid, key) -> ops.del uuid key
    | Admin_password _ -> Lwt.fail @@ Not_implemented "del"

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
      match Lopt.get_value state with
      | None | Some (`Tallied | `Archived) -> true
      | _ -> false
    in
    if final then
      let archive_name : _ file = Election (uuid, Confidential_archive) in
      let* b = file_exists archive_name in
      let* () = if not b then make_archive uuid else Lwt.return_unit in
      match get_props archive_name with
      | Concrete (f, _, _) -> Lwt.return f
      | _ -> Lwt.fail @@ Not_implemented "get_archive"
    else Lwt.fail Not_found

  let get_unixfilename (type t) : t file -> _ = function
    | Election (_, (Public_archive | Private_creds : t election_file)) as x -> (
        match get_props x with
        | Concrete (f, _, _) -> Lwt.return f
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

  let draft_filename = "draft.json"
  let private_creds_downloaded_filename = "private_creds.downloaded"

  type draft_concrete =
    | Draft_concrete :
        'a Election.version * 'a Serializable_j.raw_draft_election
        -> draft_concrete

  let get_draft uuid () =
    let* se_private_creds_downloaded =
      let* x =
        Filesystem.read_file (uuid /// private_creds_downloaded_filename)
      in
      match x with None -> Lwt.return_false | Some _ -> Lwt.return_true
    in
    let* concrete =
      let*& x = Filesystem.read_file (uuid /// draft_filename) in
      let x = String.trim x in
      let abstract =
        Serializable_j.raw_draft_election_of_string Yojson.Safe.read_json x
      in
      let (Version v) = Election.version_of_int abstract.se_version in
      let open (val Election.get_serializers v) in
      Draft_concrete (v, raw_draft_election_of_string read_question x)
      |> Lwt.return_some
    in
    match concrete with
    | None -> Lwt.return Lopt.none
    | Some (Draft_concrete (v, concrete)) ->
        let abstract =
          Converters.raw_draft_election_of_concrete concrete
            se_private_creds_downloaded
        in
        Draft (v, abstract)
        |> Lopt.some_value string_of_draft_election
        |> Lwt.return

  let set_draft uuid () data =
    match Lopt.get_value data with
    | None -> assert false
    | Some (Draft (v, abstract)) ->
        let concrete, se_private_creds_downloaded =
          Converters.raw_draft_election_to_concrete abstract
        in
        let* () =
          let filename = uuid /// private_creds_downloaded_filename in
          if se_private_creds_downloaded then Filesystem.write_file filename ""
          else Filesystem.cleanup_file filename
        in
        let data =
          let open (val Election.get_serializers v) in
          Serializable_j.string_of_raw_draft_election write_question concrete
        in
        let* () =
          Filesystem.write_file (uuid /// draft_filename) (data ^ "\n")
        in
        let () = Elections_cache.clear () in
        Lwt.return_unit

  let del_draft uuid () =
    cleanup_files uuid [ private_creds_downloaded_filename; draft_filename ]

  let () =
    draft_ops.get <- get_draft;
    draft_ops.set <- set_draft;
    draft_ops.del <- del_draft

  let decryption_tokens_filename = "decryption_tokens.json"
  let skipped_shufflers_filename = "skipped_shufflers.json"
  let shuffle_token_filename = "shuffle_token.json"

  let get_state_state uuid () =
    let* state = get (Election (uuid, State)) in
    match Lopt.get_value state with
    | Some `EncryptedTally ->
        let* x = Filesystem.read_file (uuid /// decryption_tokens_filename) in
        let&** x = x in
        x |> String.trim |> Belenios_storage_api.decryption_tokens_of_string
        |> (fun x -> Some (`Decryption x))
        |> Lopt.some_value Belenios_storage_api.string_of_state_state
        |> Lwt.return
    | Some `Shuffling ->
        let* skipped =
          Filesystem.read_file (uuid /// skipped_shufflers_filename)
        in
        let skipped =
          skipped
          |> Option.map
               (String.trim >> Belenios_storage_api.skipped_shufflers_of_string)
          |> Option.value ~default:[]
        in
        let* token = Filesystem.read_file (uuid /// shuffle_token_filename) in
        let token =
          token
          |> Option.map
               (String.trim >> Belenios_storage_api.shuffle_token_of_string)
        in
        Some (`Shuffle { skipped; token })
        |> Lopt.some_value Belenios_storage_api.string_of_state_state
        |> Lwt.return
    | _ -> Lopt.none_lwt

  let set_state_state uuid () (x : Belenios_storage_api.state_state Lopt.t) =
    match Lopt.get_value x with
    | None -> assert false
    | Some None ->
        cleanup_files uuid
          [
            decryption_tokens_filename;
            skipped_shufflers_filename;
            shuffle_token_filename;
          ]
    | Some (Some (`Shuffle { skipped; token })) ->
        let* () = cleanup_files uuid [ decryption_tokens_filename ] in
        let* () =
          skipped |> Belenios_storage_api.string_of_skipped_shufflers
          |> (fun x -> x ^ "\n")
          |> Filesystem.write_file (uuid /// skipped_shufflers_filename)
        in
        let* () =
          match token with
          | None -> cleanup_files uuid [ shuffle_token_filename ]
          | Some token ->
              token |> Belenios_storage_api.string_of_shuffle_token
              |> (fun x -> x ^ "\n")
              |> Filesystem.write_file (uuid /// shuffle_token_filename)
        in
        Lwt.return_unit
    | Some (Some (`Decryption tokens)) ->
        let* () =
          cleanup_files uuid
            [ skipped_shufflers_filename; shuffle_token_filename ]
        in
        tokens |> Belenios_storage_api.string_of_decryption_tokens
        |> (fun x -> x ^ "\n")
        |> Filesystem.write_file (uuid /// decryption_tokens_filename)

  let () =
    state_state_ops.get <- get_state_state;
    state_state_ops.set <- set_state_state

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
    let&** dates =
      match (raw_dates, hide_result) with
      | None, None -> None
      | None, Some t ->
          Some
            {
              Belenios_storage_api.default_election_dates with
              e_date_publish = Some t;
            }
      | Some d, e_date_publish ->
          let d = Serializable_j.election_dates_of_string (String.trim d) in
          Some
            {
              e_date_creation = Datetime.to_unixfloat d.e_creation;
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
    dates
    |> Lopt.some_value Belenios_storage_api.string_of_election_dates
    |> Lwt.return

  let set_dates uuid () dates =
    let@ d cont =
      match Lopt.get_value dates with
      | None -> assert false
      | Some d -> cont (d : Belenios_storage_api.election_dates)
    in
    let* () =
      let filename = uuid /// hide_result_filename in
      match d.e_date_publish with
      | None -> Filesystem.cleanup_file filename
      | Some t ->
          let t = t |> Datetime.from_unixfloat |> string_of_datetime in
          Filesystem.write_file filename (t ^ "\n")
    in
    let filename = uuid /// raw_dates_filename in
    let dates : Serializable_t.election_dates =
      {
        e_creation = Datetime.from_unixfloat d.e_date_creation;
        e_finalization =
          Option.map Datetime.from_unixfloat d.e_date_finalization;
        e_tally = Option.map Datetime.from_unixfloat d.e_date_tally;
        e_archive = Option.map Datetime.from_unixfloat d.e_date_archive;
        e_last_mail = Option.map Datetime.from_unixfloat d.e_date_last_mail;
        e_auto_open = Option.map Datetime.from_unixfloat d.e_date_auto_open;
        e_auto_close = Option.map Datetime.from_unixfloat d.e_date_auto_close;
      }
    in
    Filesystem.write_file filename
      (Serializable_j.string_of_election_dates dates ^ "\n")

  let () =
    dates_ops.get <- get_dates;
    dates_ops.set <- set_dates

  let records_filename = "records"

  let split_voting_record =
    let rex = Re.Pcre.regexp "\"(.*)(\\..*)?\" \".*:(.*)\"" in
    fun x ->
      let s = Re.Pcre.exec ~rex x in
      let date =
        Datetime.to_unixfloat @@ Datetime.wrap @@ Re.Pcre.get_substring s 1
      in
      let username = Re.Pcre.get_substring s 3 in
      (username, date)

  let get_records uuid () =
    let* raw_records = Filesystem.read_file (uuid /// records_filename) in
    let&** raw_records = raw_records in
    raw_records |> split_lines
    |> List.map split_voting_record
    |> Lopt.some_value Belenios_storage_api.string_of_election_records
    |> Lwt.return

  let () = records_ops.get <- get_records

  module ExtendedRecordsCacheTypes = struct
    type key = uuid
    type value = (float * string) SMap.t
  end

  module ExtendedRecordsCache = Ocsigen_cache.Make (ExtendedRecordsCacheTypes)

  let raw_get_extended_records uuid =
    let* x = Filesystem.read_file (uuid /// extended_records_filename) in
    let x = match x with None -> [] | Some x -> split_lines x in
    Lwt_list.fold_left_s
      (fun accu x ->
        let x = extended_record_of_string x in
        Lwt.return
        @@ SMap.add x.r_username
             (Datetime.to_unixfloat x.r_date, x.r_credential)
             accu)
      SMap.empty x

  let dump_extended_records uuid rs =
    let rs = SMap.bindings rs in
    let extended_records =
      List.map
        (fun (r_username, (r_date, r_credential)) ->
          let r_date = Datetime.from_unixfloat r_date in
          { r_username; r_date; r_credential } |> string_of_extended_record)
        rs
      |> join_lines
    in
    let records =
      rs
      |> List.map (fun (u, (d, _)) ->
             Printf.sprintf "%s %S"
               (d |> Datetime.from_unixfloat |> string_of_datetime)
               u)
      |> join_lines
    in
    let* () =
      Filesystem.write_file
        (uuid /// extended_records_filename)
        extended_records
    in
    Filesystem.write_file (uuid /// records_filename) records

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
      let r_date = Datetime.from_unixfloat r_date in
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
        let* x = find_extended_record uuid r_username in
        let&** r_date, r_credential = x in
        let open Belenios_storage_api in
        { r_username; r_date; r_credential }
        |> Lopt.some_value string_of_extended_record
        |> Lwt.return);
    extended_records_ops.set <-
      (fun uuid username data ->
        match Lopt.get_value data with
        | None -> assert false
        | Some { r_username; r_date; r_credential } ->
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
    match Lopt.get_value file with
    | Some x ->
        let public_credentials = x |> List.map strip_public_credential in
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
      (fun uuid c_credential ->
        let* x = find_credential_mapping uuid c_credential in
        let&** c_ballot = x in
        { c_credential; c_ballot }
        |> Lopt.some_value string_of_credential_mapping
        |> Lwt.return);
    credential_mappings_ops.set <-
      (fun uuid cred data ->
        match Lopt.get_string data with
        | None -> assert false
        | Some data ->
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
    match Lopt.get_value x with None -> Lwt.return [] | Some x -> Lwt.return x

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
    let&** { has_explicit_weights; username_or_address; voter_map } = x in
    let nb_voters = SMap.cardinal voter_map in
    let x : voters_config =
      { has_explicit_weights; username_or_address; nb_voters }
    in
    x |> Lopt.some_value string_of_voters_config |> Lwt.return

  let get_voter uuid id =
    let* x = get_voters uuid in
    let&** { voter_map; _ } = x in
    let&** x = SMap.find_opt (String.lowercase_ascii id) voter_map in
    x |> Lopt.some_value Voter.to_string |> Lwt.return

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
      match Lopt.get_value x with
      | None ->
          (* public credentials mapping is no longer available, use
             the public view from the archive *)
          let fail () =
            Lwt.fail (Election_not_found (uuid, "raw_get_credential_cache"))
          in
          let ( let& ) x f = match x with None -> fail () | Some x -> f x in
          let* x = get (Election (uuid, Roots)) in
          let& roots = Lopt.get_value x in
          let& h = roots.roots_setup_data in
          let* x = get (Election (uuid, Data h)) in
          let& x = Lopt.get_value x in
          let setup_data = x |> setup_data_of_string in
          let* x = get (Election (uuid, Data setup_data.setup_credentials)) in
          let& x = Lopt.get_value x in
          x |> public_credentials_of_string |> cont
      | Some x -> cont x
    in
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
        let&** x, _ = SMap.find_opt cred x.cred_map in
        let&** x = x in
        x |> Lopt.some_value Fun.id |> Lwt.return)
      (fun _ -> Lopt.none_lwt)

  let get_credential_weight uuid cred =
    Lwt.catch
      (fun () ->
        let* x = credential_cache#find uuid in
        let&** _, x = SMap.find_opt cred x.cred_map in
        x |> Lopt.some_value Weight.to_string |> Lwt.return)
      (fun _ -> Lopt.none_lwt)

  let () =
    credential_users_ops.get <- get_credential_user;
    credential_weights_ops.get <- get_credential_weight

  (** {1 Cleaning operations} *)

  let delete_draft_election uuid =
    let* () = rmdir !!(Uuid.unwrap uuid) in
    Elections_cache.clear ();
    Lwt.return_unit

  type generic_election_file = F : 'a election_file -> generic_election_file

  let delete_sensitive_data uuid =
    let* () =
      cleanup_files uuid
        [
          extended_records_filename;
          credential_mappings_filename;
          decryption_tokens_filename;
        ]
    in
    let* () =
      Lwt_list.iter_p
        (fun (F x) -> del (Election (uuid, x)))
        [ F State; F Private_key; F Private_keys; F Public_creds ]
    in
    Elections_cache.clear ();
    Lwt.return_unit

  let delete_live_data uuid =
    let* () =
      Lwt_list.iter_p
        (fun (F x) -> del (Election (uuid, x)))
        [
          F Last_event;
          F Metadata;
          F Audit_cache;
          F Public_archive;
          F Passwords;
          F Voters;
          F Confidential_archive;
        ]
    in
    let* () =
      cleanup_files uuid
        [
          raw_dates_filename;
          hide_result_filename;
          records_filename;
          skipped_shufflers_filename;
          shuffle_token_filename;
        ]
    in
    Elections_cache.clear ();
    Lwt.return_unit

  let ( let&! ) x f = match x with None -> Lwt.return_unit | Some x -> f x

  let delete_live_election uuid roots =
    let ( let&? ) x f =
      let* x = get (Election (uuid, x)) in
      let&! x = Lopt.get_value x in
      f x
    in
    let@ setup_data cont =
      let&! x = roots.roots_setup_data in
      let&? x = Data x in
      cont (setup_data_of_string x)
    in
    let@ election cont =
      let&? x = Data setup_data.setup_election in
      cont (Election.of_string (module Random) x)
    in
    let@ trustees cont =
      let&? x = Data setup_data.setup_trustees in
      cont (trustees_of_string Yojson.Safe.read_json Yojson.Safe.read_json x)
    in
    let module W = (val election) in
    let module CredSet = Set.Make (W.G) in
    let&? metadata = Metadata in
    let&? dates = Dates in
    let* () = delete_sensitive_data uuid in
    let de_template =
      {
        t_description = "";
        t_name = W.template.t_name;
        t_questions = Array.map W.erase_question W.template.t_questions;
        t_administrator = None;
        t_credential_authority = None;
      }
      |> string_of_template W.write_question
      |> template_of_string Yojson.Safe.read_json
    in
    let de_owners = metadata.e_owners in
    let de_date =
      match dates.e_date_tally with
      | Some x -> x
      | None -> (
          match dates.e_date_finalization with
          | Some x -> x
          | None -> dates.e_date_creation)
    in
    let de_authentication_method =
      match metadata.e_auth_config with
      | Some [ { auth_system = "cas"; auth_config; _ } ] ->
          let server = List.assoc "server" auth_config in
          `CAS server
      | Some [ { auth_system = "password"; _ } ] -> `Password
      | _ -> `Unknown
    in
    let de_credential_method =
      match metadata.e_cred_authority with
      | Some "server" -> `Automatic
      | _ -> `Manual
    in
    let* de_trustees =
      trustees
      |> List.map (function
           | `Single _ -> `Single
           | `Pedersen t ->
               `Pedersen (t.t_threshold, Array.length t.t_verification_keys))
      |> Lwt.return
    in
    let* de_nb_ballots =
      match roots.roots_last_ballot_event with
      | None -> Lwt.return 0
      | Some e ->
          let rec loop seen accu e =
            let@ event cont =
              let* x = get (Election (uuid, Data e)) in
              match Lopt.get_value x with
              | None -> Lwt.return accu
              | Some x -> cont (event_of_string x)
            in
            match
              (event.event_typ, event.event_payload, event.event_parent)
            with
            | `Ballot, Some b, Some p ->
                let@ ballot cont =
                  let* x = get (Election (uuid, Data b)) in
                  match Lopt.get_value x with
                  | None -> Lwt.return accu
                  | Some b -> cont (W.read_ballot ++ b)
                in
                let@ credential cont =
                  match W.get_credential ballot with
                  | None -> loop seen (accu + 1) p
                  | Some c -> cont c
                in
                if CredSet.mem credential seen then loop seen accu p
                else loop (CredSet.add credential seen) (accu + 1) p
            | _ -> Lwt.return accu
          in
          loop CredSet.empty 0 e
    in
    let* de_nb_voters, de_has_weights =
      let* x = get (Election (uuid, Voters_config)) in
      match Lopt.get_value x with
      | None -> Lwt.return (0, false)
      | Some { has_explicit_weights; nb_voters; _ } ->
          Lwt.return (nb_voters, has_explicit_weights)
    in
    let de =
      {
        de_uuid = uuid;
        de_template;
        de_owners;
        de_nb_voters;
        de_nb_ballots;
        de_date = Datetime.from_unixfloat de_date;
        de_tallied = roots.roots_result <> None;
        de_authentication_method;
        de_credential_method;
        de_trustees;
        de_has_weights;
      }
    in
    let* () =
      de |> string_of_deleted_election
      |> (fun x -> x ^ "\n")
      |> Filesystem.write_file (uuid /// deleted_filename)
    in
    delete_live_data uuid

  let delete_election uuid =
    let* x = get (Election (uuid, Roots)) in
    match Lopt.get_value x with
    | None -> delete_draft_election uuid
    | Some roots -> delete_live_election uuid roots

  let archive_election uuid =
    let* () = delete_sensitive_data uuid in
    let* x = update (Election (uuid, Dates)) in
    let&! dates, set = x in
    let&! dates = Lopt.get_value dates in
    set Value { dates with e_date_archive = Some (Unix.gettimeofday ()) }

  (** {1 Public archive operations} *)

  let get_last_event uuid =
    let* x = get (Election (uuid, Last_event)) in
    let&* x = Lopt.get_value x in
    Lwt.return_some x

  let set_last_event uuid x = set (Election (uuid, Last_event)) Value x
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
        let timestamp = Unix.time () |> Int64.of_float in
        let header = Archive.new_header ~timestamp in
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
    let* filename = get_unixfilename (Election (uuid, Public_archive)) in
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
    let&** i = Hashtbl.find_opt index x in
    let open Lwt_unix in
    let@ fd cont =
      Lwt.try_bind
        (fun () -> openfile filename [ O_RDONLY ] 0o644)
        cont
        (function
          | Unix.Unix_error (ENOENT, _, _) -> Lopt.none_lwt | e -> Lwt.reraise e)
    in
    Lwt.finalize
      (fun () ->
        let* _ = LargeFile.lseek fd i.location_offset SEEK_SET in
        assert (i.location_length <= Int64.of_int Sys.max_string_length);
        let length = Int64.to_int i.location_length in
        let buffer = Bytes.create length in
        let ic = Lwt_io.of_fd ~mode:Input fd in
        let* () = Lwt_io.read_into_exactly ic buffer 0 length in
        Bytes.to_string buffer |> Lopt.some_value Fun.id |> Lwt.return)
      (fun () -> close fd)

  let get_data uuid x =
    Lwt.try_bind
      (fun () -> get_index ~creat:false uuid)
      (fun r ->
        let&** r = r in
        let* filename = get_unixfilename (Election (uuid, Public_archive)) in
        gethash ~index:r.map ~filename x)
      (function Creation_not_requested -> Lopt.none_lwt | e -> Lwt.reraise e)

  let () = data_ops.get <- get_data

  let get_roots uuid () =
    Lwt.try_bind
      (fun () -> get_index ~creat:false uuid)
      (fun r ->
        let&** r = r in
        r.roots |> Lopt.some_value string_of_roots |> Lwt.return)
      (function Creation_not_requested -> Lopt.none_lwt | e -> Lwt.reraise e)

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
      let* filename = get_unixfilename (Election (uuid, Public_archive)) in
      raw_append ~filename ~timestamp:index.timestamp pos items
    in
    let* () = set_last_event uuid { last_hash; last_height; last_pos } in
    List.iter (fun r -> Hashtbl.add index.map r.Archive.hash r.location) records;
    index.roots <- roots;
    Lwt.return_true

  let make set_ =
    let with_lock uuid f =
      let* () = Mutex_set.lock set_ uuid in
      f ()
    in
    let with_lock_file x f =
      let uuid =
        match (x : _ file) with Election (uuid, _) -> Some uuid | _ -> None
      in
      with_lock uuid f
    in
    let module X = struct
      let get_unixfilename f = with_lock_file f (fun () -> get_unixfilename f)
      let get f = with_lock_file f (fun () -> get f)
      let set f s x = with_lock_file f (fun () -> set f s x)
      let del f = with_lock_file f (fun () -> del f)
      let update f = with_lock_file f (fun () -> update f)
      let list_accounts () = with_lock None list_accounts
      let list_elections () = with_lock None list_elections
      let new_election () = with_lock None new_election
      let new_account_id () = with_lock None new_account_id

      let init_credential_mapping uuid =
        with_lock (Some uuid) (fun () -> init_credential_mapping uuid)

      let archive_election uuid =
        with_lock (Some uuid) (fun () -> archive_election uuid)

      let delete_election uuid =
        with_lock (Some uuid) (fun () -> delete_election uuid)

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
      x |> Lopt.get_value |> Lwt.return

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
  let* () =
    let* x = Filesystem.read_file (spool_dir // "version") in
    match x with
    | Some x when String.trim x = "1" -> Lwt.return_unit
    | _ -> failwith "unknown spool version"
  in
  let module X = Make (Config) in
  Lwt.return (module X : Storage.S)

let register () = Storage.register_backend backend_name make_backend
