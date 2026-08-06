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

open Ppx_yojson_conv_lib.Yojson_conv
open Lwt.Syntax
open Belenios
open Common

(* The status contains:
   - a boolean telling whether there was a problem
   - a message to be put in the commit log
   This is updated along the way with merge. *)
type status = { mutable fail : bool; mutable msg : string }

let return_status fail msg = Lwt.return { fail; msg }

let merge_status self { fail; msg } =
  self.fail <- self.fail || fail;
  self.msg <- self.msg ^ msg

let rec_cmd xs = (Sys.executable_name, Array.of_list (Sys.argv.(0) :: xs))
let () = Random.self_init ()

let shuffle l =
  let l = Array.of_list l in
  let rec loop i =
    if i > 1 then (
      let j = Random.int i in
      let i = i - 1 in
      let t = l.(i) in
      l.(i) <- l.(j);
      l.(j) <- t;
      loop i)
    else Array.to_list l
  in
  loop (Array.length l)

(** {1 Getting URLs} *)

exception URLError of string

let useragents_array = ref [||]

let get_user_agent () =
  let useragents = !useragents_array in
  let n = Array.length useragents in
  if n > 0 then [ ("User-Agent", useragents.(Random.int n)) ] else []

let get_url url =
  let headers = get_user_agent () |> Cohttp.Header.of_list in
  let* response, body =
    Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url)
  in
  match response.status with
  | `OK -> Cohttp_lwt.Body.to_string body
  | status -> raise @@ URLError (Cohttp.Code.string_of_status status)

(** {1 Logging} *)

let log_file = ref Lwt_io.stdout
let log str = Lwt_io.fprintl !log_file ("Log: " ^ str)

let elog str =
  let* () = log str in
  Lwt_io.eprintl ("Log: " ^ str)

let logf fmt = Printf.ksprintf log fmt
let elogf fmt = Printf.ksprintf elog fmt

(** {1 Management of monitoring dir} *)

(* If it does not exist, create a fresh directory for an election and
   initialize the git. *)
let check_or_create_dir ~wdir ~uuid =
  let p = wdir // uuid in
  let* () =
    let* b = Lwt_unix.file_exists p in
    if b then Lwt.return_unit
    else
      let* () = logf "creating directory for election %s" uuid in
      let* () = Lwt_unix.mkdir p 0o755 in
      Lwt_unix.mkdir (p // "new") 0o755
  in
  let* () =
    let* b = Lwt_unix.file_exists (p // ".git") in
    if b then Lwt.return_unit
    else
      let* () = logf "init git for election %s" uuid in
      let* r = Lwt_process.exec ("git", [| "git"; "init"; p |]) in
      assert (r = WEXITED 0);
      let* () =
        "" |> Lwt_stream.of_string |> Lwt_io.chars_to_file (p // "fresh")
      in
      Lwt.return_unit
  in
  Lwt.return_unit

type data = {
  uuid : string;
  mutable election : string option;
  mutable ballots : string option;
  mutable audit_cache : string option;
  mutable archive : string option;
  mutable hash_voterlist : string option;
  mutable all_ballot_hashs : string option;
  mutable new_ballots : SSet.t option;
  mutable ballot_summary : string option;
  mutable checksums : string option;
  mutable members : string list option;
}

let empty_data uuid =
  {
    uuid;
    election = None;
    ballots = None;
    audit_cache = None;
    archive = None;
    hash_voterlist = None;
    all_ballot_hashs = None;
    new_ballots = None;
    ballot_summary = None;
    checksums = None;
    members = None;
  }

(* List of audit files. *)

type file =
  | Election
  | Ballots
  | Audit_cache
  | Archive
  | Hash_voterlist
  | All_ballot_hashs

let string_of_file = function
  | Election -> "election.json"
  | Archive -> "election.bel"
  | Ballots -> "ballots"
  | Audit_cache -> "audit-cache"
  | Hash_voterlist -> "hash_voterlist"
  | All_ballot_hashs -> "all_ballot_hashs"

let set_data data f x =
  match f with
  | Election -> data.election <- Some x
  | Archive -> data.archive <- Some x
  | Ballots -> data.ballots <- Some x
  | Audit_cache -> data.audit_cache <- Some x
  | Hash_voterlist -> data.hash_voterlist <- Some x
  | All_ballot_hashs -> data.all_ballot_hashs <- Some x

let get_data data f =
  match f with
  | Election -> data.election
  | Archive -> data.archive
  | Ballots -> data.ballots
  | Audit_cache -> data.audit_cache
  | Hash_voterlist -> data.hash_voterlist
  | All_ballot_hashs -> data.all_ballot_hashs

let audit_files = [ Election; Ballots; Audit_cache; Archive ]
let optional_audit_files = [ Hash_voterlist; All_ballot_hashs ]

let pread_full cmd =
  let@ p = Lwt_process.with_process_full cmd in
  let* stdout = p#stdout |> Lwt_io.read_chars |> Lwt_stream.to_string in
  let* stderr = p#stderr |> Lwt_io.read_chars |> Lwt_stream.to_string in
  let* r = p#status in
  Lwt.return (r, stdout, stderr)

let get_archive wdir url uuid =
  let path = wdir // uuid in
  let* r, stdout, stderr =
    pread_full
    @@ rec_cmd
         [ "archive"; "pull"; "--base-dir"; path; "--url"; url; "--uuid"; uuid ]
  in
  match r with WEXITED 0 -> Lwt.return stdout | _ -> raise @@ URLError stderr

let get_file ~wdir ~url ~uuid f =
  let link = Printf.sprintf "%s/api/elections/%s" url uuid in
  match f with
  | Election -> get_url (link ^ "/election")
  | Archive -> get_archive wdir url uuid
  | Ballots -> get_url (link ^ "/ballots/all")
  | Audit_cache -> get_url (link ^ "/audit-cache")
  | Hash_voterlist -> assert false
  | All_ballot_hashs -> assert false

let download_audit_data ~wdir ~url ~uuid =
  let data = empty_data uuid in
  let fail = ref false in
  let msg = Buffer.create 1024 in
  let* () =
    audit_files |> shuffle
    |> Lwt_list.iter_s (fun f ->
        Lwt.try_bind
          (fun () -> get_file ~wdir ~url ~uuid f)
          (fun x ->
            set_data data f x;
            Lwt.return_unit)
          (function
            | URLError e ->
                fail := true;
                Printf.bprintf msg
                  "Download %s failed with ret code %S for election %s"
                  (string_of_file f) e uuid;
                Lwt.return_unit
            | e -> Lwt.reraise e))
  in
  Lwt.return ({ fail = !fail; msg = Buffer.contents msg }, data)

let get_new_ballots old_ballotsfile new_ballotfile =
  let old =
    old_ballotsfile |> Yojson.Safe.from_string |> function
    | `Assoc o ->
        List.fold_left (fun accu (x, _) -> SSet.add x accu) SSet.empty o
    | _ -> assert false
  in
  new_ballotfile |> Yojson.Safe.from_string |> function
  | `Assoc o ->
      List.fold_left
        (fun accu (x, _) -> if SSet.mem x old then accu else SSet.add x accu)
        SSet.empty o
  | _ -> assert false

let has_warnings =
  let open Re in
  let rex = compile (str "W:") in
  fun x -> execp rex x

let compute_ballot_summary ~uuid ~which ~dir cont =
  let@ process =
    Lwt_process.with_process_in ~stderr:`Dev_null
    @@ rec_cmd [ "election"; "compute-ballot-summary"; "--dir"; dir ]
  in
  let* stdout = process#stdout |> Lwt_io.read_chars |> Lwt_stream.to_string in
  let* r = process#status in
  match r with
  | WEXITED 0 -> cont stdout
  | _ ->
      let msg =
        Printf.sprintf
          "Error: compute-ballot-summary on %s data failed for election %s"
          which uuid
      in
      return_status true msg

(* This writes data to the directory in order to run verify and
   verify-diff.  At first, this goes to a "new" subdirectory, and once
   verify-diff has been run, this is moved to the main directory of
   the election.
*)
let write_and_verify_new_data ~wdir data =
  let p = wdir // data.uuid in
  let pnew = p // "new" in
  let* () =
    (* copy new data to the "new" subdirectory *)
    audit_files @ optional_audit_files
    |> Lwt_list.iter_p (fun f ->
        match get_data data f with
        | None -> Lwt.return_unit
        | Some x ->
            x |> Lwt_stream.of_string
            |> Lwt_io.chars_to_file (pnew // string_of_file f))
  in
  let@ () =
   fun cont ->
    (* run belenios-tool verify on it *)
    let@ process =
      Lwt_process.with_process_in
      @@ rec_cmd [ "election"; "verify"; "--dir"; pnew ]
    in
    let* stdout = process#stdout |> Lwt_io.read_chars |> Lwt_stream.to_string in
    let* r = process#status in
    match r with
    | WEXITED 0 ->
        let* () = logf "Successfully verified new data of %s" data.uuid in
        cont ()
    | _ ->
        let msg =
          Printf.sprintf
            "Error: belenios-tool election verify failed on newly downloaded \
             data from election %s, with output %s"
            data.uuid stdout
        in
        return_status true msg
  in
  let archive_filename = p // "election.bel" in
  let* fresh =
    let p = p // "fresh" in
    let* b = Lwt_unix.file_exists p in
    let* () = if b then Lwt_unix.unlink p else Lwt.return_unit in
    Lwt.return b
  in
  let msg = Buffer.create 1024 in
  let@ () =
   fun cont ->
    if fresh then cont ()
    else
      (* if not the first time, run belenios-tool election verify-diff *)
      let@ () =
       fun cont ->
        let* archive_fd =
          Lwt_unix.openfile archive_filename
            [ O_CREAT; O_WRONLY; O_TRUNC ]
            0o644
        in
        let@ archive_maker =
          Lwt_process.with_process_none
            ~stdout:(`FD_move (Lwt_unix.unix_file_descr archive_fd))
            ~stderr:`Dev_null
          @@ rec_cmd [ "archive"; "make"; "--dir"; p ]
        in
        let* r = archive_maker#status in
        match r with
        | WEXITED 0 -> cont ()
        | _ ->
            let* () = Lwt_unix.unlink archive_filename in
            let msg =
              Printf.sprintf
                "Error: belenios-tool archive make failed on old data from \
                 election %s"
                data.uuid
            in
            return_status true msg
      in
      let@ () =
       fun cont ->
        let@ verdiff =
          Lwt_process.with_process_in ~stderr:(`FD_copy Unix.stdout)
          @@ rec_cmd [ "election"; "verify-diff"; "--dir1"; p; "--dir2"; pnew ]
        in
        let* stdout =
          verdiff#stdout |> Lwt_io.read_chars |> Lwt_stream.to_string
        in
        let* r = verdiff#status in
        match r with
        | WEXITED 0 ->
            if has_warnings stdout then Buffer.add_string msg stdout;
            let* () =
              logf "Successfully diff-verified new data of %s" data.uuid
            in
            cont ()
        | _ ->
            let msg =
              Printf.sprintf
                "Error: belenios-tool election verify-diff failed on newly \
                 downloaded data from election %s, with output %s"
                data.uuid stdout
            in
            return_status true msg
      in
      cont ()
  in
  let@ ballot_summary1 cont =
    if fresh then cont "{}"
    else compute_ballot_summary ~uuid:data.uuid ~which:"old" ~dir:p cont
  in
  let@ ballot_summary2 cont =
    compute_ballot_summary ~uuid:data.uuid ~which:"new" ~dir:pnew cont
  in
  let new_ballots = get_new_ballots ballot_summary1 ballot_summary2 in
  data.new_ballots <- Some new_ballots;
  data.ballot_summary <- Some ballot_summary2;
  let@ () =
   fun cont ->
    (* compute checksums *)
    let@ checksums =
      Lwt_process.with_process_in ~stderr:`Dev_null
      @@ rec_cmd [ "election"; "compute-checksums"; "--dir"; pnew ]
    in
    let* stdout =
      checksums#stdout |> Lwt_io.read_chars |> Lwt_stream.to_string
    in
    let* r = checksums#status in
    match r with
    | WEXITED 0 ->
        data.checksums <- Some stdout;
        cont ()
    | _ ->
        let msg =
          Printf.sprintf
            "Error: belenios-tool election compute-checksums failed on newly \
             downloaded data from election %s, with output %s"
            data.uuid stdout
        in
        return_status true msg
  in
  let* () =
    (* move new files to main directory *)
    audit_files @ optional_audit_files
    |> Lwt_list.iter_p (fun f ->
        match get_data data f with
        | None -> Lwt.return_unit
        | Some _ ->
            let f = string_of_file f in
            Lwt_unix.rename (pnew // f) (p // f))
  in
  let@ () =
   fun cont ->
    let@ p =
      Lwt_process.with_process_in
        ("tar", [| "tar"; "-t"; "-f"; archive_filename |])
    in
    let* members = p#stdout |> Lwt_io.read_lines |> Lwt_stream.to_list in
    let* r = p#status in
    match r with
    | WEXITED 0 ->
        data.members <- Some members;
        cont ()
    | _ ->
        let msg =
          Printf.sprintf
            "Error: reading members of election.bel for election %s failed"
            data.uuid
        in
        return_status true msg
  in
  let@ () =
   fun cont ->
    (* extract new archive *)
    let* r =
      Lwt_process.exec
        ("tar", [| "tar"; "-x"; "-f"; archive_filename; "-C"; p |])
    in
    match r with
    | WEXITED 0 ->
        let@ () =
         fun cont ->
          match data.election with
          | None -> assert false
          | Some election ->
              let h = Hash.(election |> hash_string |> to_hex) in
              let f = p // Printf.sprintf "%s.data.json" h in
              let* x = Lwt_io.chars_of_file f |> Lwt_stream.to_string in
              if x = election then cont ()
              else
                let msg =
                  Printf.sprintf
                    "Error: election.json of election %s differs from its \
                     archive"
                    data.uuid
                in
                return_status true msg
        in
        let* () = Lwt_unix.unlink (p // "election.json") in
        let* () = Lwt_unix.unlink archive_filename in
        cont ()
    | _ ->
        let msg =
          Printf.sprintf
            "Error: extraction of election.bel for election %s failed" data.uuid
        in
        return_status true msg
  in
  return_status false (Buffer.contents msg)

(* Verify that the hash of the ballots shown on the ballot-box web
   page are consistent with the json file. *)
let check_hash_ballots data =
  let ballots1 =
    match data.ballots with
    | None -> assert false
    | Some x -> !*ballot_dynamic_records_of_yojson x
  in
  let ballots2 =
    match data.ballot_summary with
    | None -> assert false
    | Some x -> !*ballot_dynamic_records_of_yojson x
  in
  if HMap.equal Stdlib.( = ) ballots1 ballots2 then
    let* () = logf "Successfully checked hash of ballots of %s" data.uuid in
    return_status false ""
  else
    let msg = "Error: hash of ballots do not correspond!\n" in
    return_status true msg

(* Verify that the data printed on the page of the election is
   consistent with the other audit files. *)
let check_audit_cache data =
  let fail = ref false in
  let msg = Buffer.create 1024 in
  let audit_cache =
    match data.audit_cache with
    | None -> assert false
    | Some x -> !*audit_cache_of_yojson x
  in
  let* () = logf "Checking audit cache of %s..." data.uuid in
  let checksums1 =
    match data.checksums with
    | None -> assert false
    | Some x -> !*election_checksums_of_yojson x
  in
  let checksums2 = audit_cache.checksums in
  let () =
    if checksums1 <> checksums2 then (
      Printf.bprintf msg "Error: Checksums mismatch in election %s\n" data.uuid;
      fail := true)
  in
  data.hash_voterlist <- Some (Hash.to_hex audit_cache.voters_hash);
  let* () =
    if not !fail then logf "Successfully checked audit cache of %s" data.uuid
    else Lwt.return_unit
  in
  return_status !fail (Buffer.contents msg)

let commit_file eldir f uuid =
  let* r = Lwt_process.exec ("git", [| "git"; "-C"; eldir; "add"; f |]) in
  match r with
  | WEXITED 0 -> Lwt.return_true
  | _ ->
      let* () = elogf "Failed git add %s for election %s" f uuid in
      Lwt.return_false

let commit ~wdir ~msg data =
  let exception Exit in
  Lwt.catch
    (fun () ->
      let eldir = wdir // data.uuid in
      let* () =
        audit_files @ optional_audit_files
        |> Lwt_list.iter_s (fun f ->
            let f' = string_of_file f in
            let x = get_data data f in
            match (f, x) with
            | (Election | Archive), _ | _, None -> Lwt.return_unit
            | _, Some _ ->
                let* b = commit_file eldir f' data.uuid in
                if b then Lwt.return_unit else raise Exit)
      in
      let* () =
        match data.members with
        | None -> assert false
        | Some fs ->
            fs
            |> Lwt_list.iter_s (fun f ->
                let* b = commit_file eldir f data.uuid in
                if b then Lwt.return_unit else raise Exit)
      in
      let* r =
        Lwt_process.exec
          ( "git",
            [|
              "git";
              "-C";
              eldir;
              "commit";
              "-q";
              "--allow-empty";
              "--allow-empty-message";
              "-m";
              msg;
            |] )
      in
      match r with
      | WEXITED 0 ->
          let* () = logf "Successfully added a commit for %s" data.uuid in
          Lwt.return_true
      | _ ->
          let* () = elogf "Failed git commit for election %s" data.uuid in
          raise Exit)
    (function Exit -> Lwt.return_false | e -> Lwt.reraise e)

(* When a new ballot arrives, check that it was not seen earlier.
   This could be some kind of replay attack (possible only if the
   voter revotes). *)
let check_noreplay path_to_all_ballot_hashs data =
  let new_hashs =
    match data.new_ballots with None -> assert false | Some x -> x
  in
  let fail = ref false in
  let msg = Buffer.create 1024 in
  let* list_hashs =
    Lwt_io.lines_of_file path_to_all_ballot_hashs |> Lwt_stream.to_list
  in
  let list_hashs =
    List.fold_left (fun accu x -> SSet.add x accu) SSet.empty list_hashs
  in
  SSet.iter
    (fun h ->
      if SSet.mem h list_hashs then (
        fail := true;
        Printf.bprintf msg
          "Error: The new ballot %s is a replay in election %s!\n" h data.uuid))
    new_hashs;
  let* () =
    let open Lwt_io in
    let@ oc =
      with_file ~flags:[ O_WRONLY; O_APPEND ] ~mode:Output
        path_to_all_ballot_hashs
    in
    let* () = new_hashs |> SSet.to_seq |> Lwt_stream.of_seq |> write_lines oc in
    flush oc
  in
  let* () =
    if !fail then Lwt.return_unit
    else logf "Successfully checked for a ballot replay of %s" data.uuid
  in
  return_status !fail (Buffer.contents msg)

let check_elections ~wdir ~url ~uuids =
  let* () =
    if uuids <> [] then
      logf "[%.0f] Starting monitoring elections." (Unix.gettimeofday ())
    else Lwt.return_unit
  in
  uuids |> shuffle
  |> Lwt_list.iter_s (fun uuid ->
      let* () = logf "Start monitoring election %s" uuid in
      let* () = check_or_create_dir ~wdir ~uuid in
      let* status, data = download_audit_data ~wdir ~url ~uuid in
      let* () =
        if not status.fail then (
          let* stat = write_and_verify_new_data ~wdir data in
          merge_status status stat;
          let* stat = check_hash_ballots data in
          merge_status status stat;
          let* stat = check_audit_cache data in
          merge_status status stat;
          let* () =
            (* create the hash_voterlist file, with the value read
                  from index.html or check that its value is
                  consistent *)
            let p = wdir // uuid // "hash_voterlist" in
            let* b = Lwt_unix.file_exists p in
            if b then
              let* oldhash = Lwt_io.chars_of_file p |> Lwt_stream.to_string in
              if data.hash_voterlist <> Some oldhash then (
                let msg =
                  Printf.sprintf
                    "Error: hash of the voter list changed for election %s" uuid
                in
                merge_status status { fail = true; msg };
                Lwt.return_unit)
              else Lwt.return_unit
            else
              match data.hash_voterlist with
              | None -> assert false
              | Some x -> x |> Lwt_stream.of_string |> Lwt_io.chars_to_file p
          in
          let* () =
            (*  create the all_ballot_hashs file, or update it from
                the new ballot files. Check that an old ballot was not
                replayed.  Note: the list of new ballot hashs is
                created earlier, during write_and_verify_new_data(),
                because it must compare the old and new ballot box. *)
            let p = wdir // uuid // "all_ballot_hashs" in
            let* b = Lwt_unix.file_exists p in
            if b then (
              let* stat = check_noreplay p data in
              merge_status status stat;
              Lwt.return_unit)
            else
              match data.new_ballots with
              | None -> assert false
              | Some x ->
                  x |> SSet.to_seq |> Lwt_stream.of_seq
                  |> Lwt_io.lines_to_file p
          in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      (* commit *)
      let* () =
        if status.msg <> "" then
          elogf "Commit log for election %s is %s" uuid status.msg
        else Lwt.return_unit
      in
      let* _ = commit ~wdir ~msg:status.msg data in
      Lwt.return_unit)

(** {1 Helper functions for monitoring static files} *)

let hash_file link =
  Lwt.try_bind
    (fun () -> get_url link)
    (fun data -> Lwt.return @@ Hash.hash_string data)
    (fun _ -> Printf.ksprintf failwith "Failed to download %s" link)

let read_linguas linguas =
  let linguas = Lwt_io.lines_of_file linguas in
  Lwt_stream.fold SSet.add linguas SSet.empty

let get_admin_available_languages belenios_srcpath =
  let* admin =
    read_linguas (belenios_srcpath // "po" // "admin" // "LINGUAS")
  in
  Lwt.return SSet.(elements admin)

let get_voter_available_languages belenios_srcpath =
  let* voter =
    read_linguas (belenios_srcpath // "po" // "voter" // "LINGUAS")
  in
  Lwt.return SSet.(elements voter)

let check_static_files ~hashref ~beleniospath ~url ~outputref ~sighashref =
  let* () =
    logf "[%.0f] Starting monitoring static files." (Unix.gettimeofday ())
  in
  let* hashref = Lwt_io.chars_of_file hashref |> Lwt_stream.to_string in
  let tmp_reference =
    !*(smap_of_yojson (option_of_yojson hash_of_yojson)) hashref
  in
  let* reference =
    tmp_reference |> SMap.to_seq |> Lwt_seq.of_seq
    |> Lwt_seq.fold_left_s
         (fun accu (f, descr) ->
           match f with
           | "/static/locales/admin/*.json" ->
               let* langs = get_admin_available_languages beleniospath in
               List.fold_left
                 (fun accu x ->
                   let f = Printf.sprintf "/static/locales/admin/%s.json" x in
                   SMap.add f None accu)
                 accu langs
               |> Lwt.return
           | "/static/locales/voter/*.json" ->
               let* langs = get_voter_available_languages beleniospath in
               List.fold_left
                 (fun accu x ->
                   let f = Printf.sprintf "/static/locales/voter/%s.json" x in
                   SMap.add f None accu)
                 accu langs
               |> Lwt.return
           | "/static/frontend/translations/*.json" ->
               let* langs =
                 Lwt_unix.files_of_directory
                   (beleniospath // "frontend" // "translations")
                 |> Lwt_stream.to_list
               in
               let langs =
                 List.filter (fun x -> String.ends_with ~suffix:".json" x) langs
               in
               List.fold_left
                 (fun accu x ->
                   let f =
                     Printf.sprintf "/static/frontend/translations/%s" x
                   in
                   SMap.add f None accu)
                 accu langs
               |> Lwt.return
           | f when String.contains f '*' ->
               Printf.ksprintf failwith "Wildcard not supported in %s" f
           | f -> SMap.add f descr accu |> Lwt.return)
         SMap.empty
  in
  let hashfile_changed = ref false in
  let* new_reference =
    reference |> SMap.to_seq |> Seq.map fst |> List.of_seq |> shuffle
    |> Lwt_list.fold_left_s
         (fun accu f ->
           let descr = SMap.find f reference in
           let* h = hash_file (url ^ f) in
           let* () =
             if Some h <> descr then (
               hashfile_changed := true;
               let descr =
                 match descr with None -> "None" | Some x -> Hash.to_hex x
               in
               Lwt_io.printlf
                 "Different hash of static file %s: got %s but expected %s" f
                 (Hash.to_hex h) descr)
             else Lwt.return_unit
           in
           SMap.add f h accu |> Lwt.return)
         SMap.empty
  in
  let* () =
    if !hashfile_changed then logf "Hash of static files have changed"
    else logf "Successfully checked hash of static files"
  in
  let* () =
    match (!hashfile_changed, outputref) with
    | true, Some outputref ->
        let* () = Lwt_io.printlf "Writing new reference file" in
        new_reference
        |> !+(yojson_of_smap yojson_of_hash)
        |> Lwt_stream.of_string
        |> Lwt_io.chars_to_file outputref
    | _ -> Lwt.return_unit
  in
  let* () =
    match sighashref with
    | None -> Lwt.return_unit
    | Some (sighashref, keyring) -> (
        let* signature = get_url sighashref in
        let@ p =
          Lwt_process.with_process
            ( "gpg",
              [|
                "gpg"; "--no-default-keyring"; "--keyring"; keyring; "--decrypt";
              |] )
        in
        let* () = signature |> Lwt_io.write p#stdin in
        let* stdout = p#stdout |> Lwt_io.read_chars |> Lwt_stream.to_string in
        let* r = p#status in
        match r with
        | WEXITED 0 ->
            let signed_ref = !*(smap_of_yojson hash_of_yojson) stdout in
            if SMap.equal Stdlib.( = ) signed_ref new_reference then
              logf "Successfully check signature of hash of static files"
            else
              failwith
                "Signed reference does not correspond to downloaded files"
        | _ -> failwith "GPG signature verification failed")
  in
  Lwt.return_unit

(** {1 Argument parsing} *)

open Cmdliner

let url_t =
  let doc = "Prefix url (without trailing /elections)." in
  Arg.(required & opt (some string) None & info [ "url" ] ~docv:"URL" ~doc)

let uuidfile_t =
  let doc = "File containing uuids of elections to monitor." in
  Arg.(
    value & opt (some string) None & info [ "uuidfile" ] ~docv:"UUIDFILE" ~doc)

let uuid_t =
  let doc = "Uuid of an election to monitor." in
  Arg.(value & opt (some string) None & info [ "uuid" ] ~docv:"UUID" ~doc)

let wdir_t =
  let doc = "Work dir where logs are keps." in
  Arg.(value & opt (some string) None & info [ "wdir" ] ~docv:"WDIR" ~doc)

let checkhash_t =
  let doc = "Also check static files on the server." in
  Arg.(value & flag & info [ "checkhash" ] ~docv:"CHECKHASH" ~doc)

let hashref_t =
  let doc = "Reference file for hash of static files." in
  Arg.(value & opt (some file) None & info [ "hashref" ] ~docv:"HASHREF" ~doc)

let outputref_t =
  let doc = "New reference file in case it changed on the server." in
  Arg.(
    value & opt (some string) None & info [ "outputref" ] ~docv:"OUTPUTREF" ~doc)

let sighashref_t =
  let doc = "URL where to find a gpg signature for the reference file." in
  Arg.(
    value
    & opt (some string) None
    & info [ "sighashref" ] ~docv:"SIGHASHREF" ~doc)

let keyring_t =
  let doc = "Keyring to check the signature." in
  Arg.(value & opt (some file) None & info [ "keyring" ] ~docv:"KEYRING" ~doc)

let beleniospath_t =
  let doc = "Path to Belenios sources" in
  Arg.(value & opt dir "." & info [ "beleniospath" ] ~docv:"BELENIOSPATH" ~doc)

let logfile_t =
  let doc = "File to write the non-error logs." in
  Arg.(value & opt (some file) None & info [ "logfile" ] ~docv:"LOGFILE" ~doc)

let useragents_t =
  let doc = "File with user agents to use for HTTP requests" in
  Arg.(
    value & opt (some file) None & info [ "useragents" ] ~docv:"USEAGENTS" ~doc)

let main url uuidfile uuid wdir checkhash hashref outputref sighashref keyring
    beleniospath logfile useragents =
  let@ () = wrap_main in
  let url =
    if String.ends_with ~suffix:"/" url then
      String.sub url 0 (String.length url - 1)
    else url
  in
  let* () =
    match useragents with
    | None -> Lwt.return_unit
    | Some useragents ->
        let* x = Lwt_io.lines_of_file useragents |> Lwt_stream.to_list in
        useragents_array := Array.of_list x;
        Lwt.return_unit
  in
  let* () =
    match logfile with
    | None -> Lwt.return_unit
    | Some x ->
        let* x = Lwt_io.open_file ~mode:Output x in
        log_file := x;
        Lwt.return_unit
  in
  let* uuids =
    match (uuid, uuidfile) with
    | None, None -> Lwt.return_nil
    | Some x, None -> Lwt.return [ x ]
    | None, Some f -> Lwt_io.lines_of_file f |> Lwt_stream.to_list
    | Some _, Some _ -> failwith "--uuid and --uuidfile are mutually exclusive"
  in
  let* () =
    (* check that wdir exists and is r/w (if uuids given) *)
    match uuids with
    | [] -> Lwt.return_unit
    | _ -> (
        match wdir with
        | None -> failwith "--wdir is mandatory for monitoring elections"
        | Some wdir ->
            let* b =
              let* stat = Lwt_unix.stat wdir in
              if stat.st_kind = S_DIR then
                Lwt.try_bind
                  (fun () -> Lwt_unix.access wdir [ W_OK; R_OK ])
                  (fun () -> Lwt.return_true)
                  (fun _ -> Lwt.return_false)
              else Lwt.return_false
            in
            if b then Lwt.return_unit
            else
              Printf.ksprintf failwith
                "The wdir %s should be read/write accessible" wdir)
  in
  let () =
    if checkhash && hashref = None then
      failwith "If --checkhash is set, a --hashref file must be given"
  in
  let sighashref =
    match (sighashref, keyring) with
    | Some sighashref, Some keyring when checkhash -> Some (sighashref, keyring)
    | Some _, _ ->
        failwith
          "If --sighashref is given, --checkhash must be set and a --keyring \
           file must be given"
    | _ -> None
  in
  let* () =
    if checkhash then
      let hashref = Option.get hashref in
      check_static_files ~beleniospath ~hashref ~url ~outputref ~sighashref
    else Lwt.return_unit
  in
  let* () = check_elections ~wdir:(Option.get wdir) ~url ~uuids in
  Lwt_io.close !log_file

let cmd =
  let doc = "monitor a Belenios server" in
  let man =
    [ `S "DESCRIPTION"; `P "This command monitors a Belenios server." ]
    @ common_man
  in
  Cmd.v
    (Cmd.info "monitor" ~doc ~man)
    Term.(
      ret
        (const main $ url_t $ uuidfile_t $ uuid_t $ wdir_t $ checkhash_t
       $ hashref_t $ outputref_t $ sighashref_t $ keyring_t $ beleniospath_t
       $ logfile_t $ useragents_t))
