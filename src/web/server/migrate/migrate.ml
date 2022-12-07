(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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
open Belenios_core.Serializable_j
open Belenios_core.Common
open Belenios_server
open Web_common
open Serializable_j
open Web_serializable_j

let base = Filename.dirname Sys.executable_name
let belenios_tool = base // "belenios-tool"

let log fmt = Printf.ksprintf (fun x -> Lwt_io.write_line Lwt_io.stdout x) fmt
let elog fmt = Printf.ksprintf (fun x -> Lwt_io.write_line Lwt_io.stderr x) fmt

let is_election_with_old_crypto uuid =
  let uuid_s = Uuid.unwrap uuid in
  let@ version = fun cont ->
    let* x = read_file_single_line ~uuid "election.json" in
    match x with
    | Some raw_election -> cont @@ Belenios.Election.get_version raw_election
    | None ->
       let* x = read_file_single_line ~uuid Spool.(filename draft) in
       match x with
       | Some draft ->
          let draft = old_draft_election_of_string draft in
          cont @@ Option.value ~default:0 draft.se_version
       | None ->
          let* x = read_file_single_line ~uuid "deleted.json" in
          match x with
          | Some _ -> Lwt.return_false
          | None ->
             let* () = elog "Unknown status for election %s" uuid_s in
             Lwt.return_false
  in
  Lwt.return (version < 1)

let convert_metadata_to_v1 (metadata : old_metadata) =
  {
    Belenios_server.Web_serializable_t.e_owners =
      begin
        match metadata.e_owner with
        | None -> []
        | Some (`Id x) -> x
        | Some (`User _) -> []
      end;
    e_auth_config = metadata.e_auth_config;
    e_cred_authority = metadata.e_cred_authority;
    e_trustees = metadata.e_trustees;
    e_languages = metadata.e_languages;
    e_contact = metadata.e_contact;
    e_booth_version = metadata.e_booth_version;
  }

let migrate_draft_to_v1 uuid accu =
  let uuid_s = Uuid.unwrap uuid in
  let* draft = read_file_single_line ~uuid Spool.(filename draft) in
  match draft with
  | None -> Lwt.return accu
  | Some draft ->
     let draft = old_draft_election_of_string draft in
     let* () = log "  Migrating draft %s..." uuid_s in
     let* () = write_file ~uuid "migration" ["0"] in
     let se_trustees =
       match draft.se_threshold_trustees with
       | None -> `Basic {dbp_trustees = draft.se_public_keys}
       | Some ts ->
          let dtp =
            {
              dtp_threshold = draft.se_threshold;
              dtp_trustees = ts;
              dtp_parameters = draft.se_threshold_parameters;
              dtp_error = draft.se_threshold_error;
            }
          in
          `Threshold dtp
     in
     let new_draft =
       {
         Belenios_server.Web_serializable_t.se_version =
           Option.value ~default:0 draft.se_version;
         se_owners =
           begin
             match draft.se_owner with
             | `Id x -> x
             | `User _ -> []
           end;
         se_group = draft.se_group;
         se_voters = draft.se_voters;
         se_questions = draft.se_questions;
         se_trustees;
         se_metadata = convert_metadata_to_v1 draft.se_metadata;
         se_public_creds = draft.se_public_creds;
         se_public_creds_received = draft.se_public_creds_received;
         se_creation_date = draft.se_creation_date;
         se_administrator = draft.se_administrator;
       }
     in
     let* () = Spool.set ~uuid Spool.draft new_draft in
     let* () = write_file ~uuid "migration" ["1"] in
     Lwt.return @@ uuid :: accu

let migrate_election_to_v1 uuid accu =
  let uuid_s = Uuid.unwrap uuid in
  let* migration = read_file ~uuid "migration" in
  match migration with
  | None ->
     begin
       let@ election = fun cont ->
         let* x = read_file_single_line ~uuid "election.json" in
         match x with
         | Some x -> cont x
         | None -> migrate_draft_to_v1 uuid accu
       in
       let* () = log "  Migrating %s..." uuid_s in
       let* () = write_file ~uuid "migration" ["0"] in
       let* trustees =
         let* x = read_file_single_line ~uuid "trustees.json" in
         match x with
         | None -> assert false
         | Some x -> Lwt.return x
       in
       let* public_creds =
         let* x = read_file ~uuid "public_creds.txt" in
         match x with
         | None -> assert false
         | Some x -> Lwt.return x
       in
       let* metadata =
         let* x = read_file_single_line ~uuid Spool.(filename metadata) in
         match x with
         | None -> Lwt.return Web_persist.empty_metadata
         | Some x ->
            let old_ = old_metadata_of_string x in
            let new_ = convert_metadata_to_v1 old_ in
            let* () = Spool.set ~uuid Spool.metadata new_ in
            Lwt.return new_
       in
       let* () = log "    Cleaning up event files..." in
       let* () = cleanup_file (uuid /// uuid_s ^ ".bel") in
       let* () = Spool.del ~uuid Spool.last_event in
       let* () = log "    Populating election setup..." in
       let* () =
         let public_creds_s = string_of_public_credentials public_creds in
         let setup_election = Hash.hash_string election in
         let setup_trustees = Hash.hash_string trustees in
         let setup_credentials = Hash.hash_string public_creds_s in
         let setup_data = {setup_election; setup_trustees; setup_credentials} in
         let setup_data_s = string_of_setup_data setup_data in
         Web_events.append ~uuid
           [
             Data election;
             Data trustees;
             Data public_creds_s;
             Data setup_data_s;
             Event (`Setup, Some (Hash.hash_string setup_data_s));
           ]
       in
       let module R = struct let raw_election = election end in
       let module E = Belenios.Election.Make (R) (LwtRandom) () in
       let weights =
         let module PPC = Belenios_core.Credential.MakeParsePublicCredential (E.G) in
         List.fold_left
           (fun accu x ->
             match PPC.parse_public_credential x with
             | Some (w, y) -> SMap.add (E.G.to_string y) w accu
             | None -> assert false
           ) SMap.empty public_creds
       in
       let* () = log "    Populating ballots..." in
       let* ballots =
         let* x = read_file ~uuid "ballots.jsons" in
         match x with
         | None -> Lwt.return []
         | Some ballots ->
            Lwt_list.map_s
              (fun x ->
                let* () =
                  Web_events.append ~uuid
                    [
                      Data x;
                      Event (`Ballot, Some (Hash.hash_string x));
                    ]
                in
                let b = E.ballot_of_string x in
                match E.get_credential b with
                | None -> assert false
                | Some c ->
                   match SMap.find_opt (E.G.to_string c) weights with
                   | None -> assert false
                   | Some w -> Lwt.return (w, b)
              ) ballots
       in
       let* () = log "    Finalizing election..." in
       let et = E.E.process_ballots ballots in
       let et_s = string_of_encrypted_tally E.(swrite G.to_string) et in
       let et_h = Hash.hash_string et_s in
       let sized =
         {
           sized_num_tallied = List.length ballots;
           sized_total_weight =
             List.fold_left (fun accu (w, _) -> Weight.(accu + w))
               Weight.zero ballots;
           sized_encrypted_tally = et_h;
         }
       in
       let sized_s = string_of_sized_encrypted_tally write_hash sized in
       let sized_h = Hash.hash_string sized_s in
       let et_ops =
         [
           Web_events.Event (`EndBallots, None);
           Data et_s;
           Data sized_s;
           Event (`EncryptedTally, Some sized_h);
         ]
       in
       let* state =
         let* x = read_file_single_line ~uuid Spool.(filename state) in
         match x with
         | None -> Lwt.return `Archived
         | Some x ->
            match election_state_of_string x with
            | x -> Lwt.return x
            | exception _ ->
               match old_election_state_of_string x with
               | `EncryptedTally _ ->
                  let x = `EncryptedTally in
                  let* () = Spool.set ~uuid Spool.state x in
                  Lwt.return x
               | (`Archived | `Closed | `Open | `Shuffling | `Tallied) as x -> Lwt.return x
       in
       let* () =
         let populate_shuffles () =
           let* () = Web_events.append ~uuid et_ops in
           let* skipped_shufflers, actual_shufflers =
             let* x = read_file ~uuid "shuffle_hashes.jsons" in
             Option.value ~default:[] x
             |> List.partition_map
                  (fun x ->
                    let x = shuffle_hash_of_string x in
                    if x.sh_hash = "" then Left x.sh_trustee else Right x
                  )
             |> Lwt.return
           in
           let* () = Spool.set ~uuid Spool.skipped_shufflers skipped_shufflers in
           let* shuffles = read_file ~uuid "shuffles.jsons" in
           let shuffles = Option.value ~default:[] shuffles in
           let trustees =
             metadata.e_trustees
             |> Option.value ~default:["server"]
             |> List.mapi (fun i x -> x, i + 1)
           in
           Lwt_list.iter_s
             (fun (x, shuffle_s) ->
               let owned_owner = List.assoc x.sh_trustee trustees in
               let owned_payload = Hash.hash_string shuffle_s in
               assert (x.sh_hash = Hash.to_b64 owned_payload);
               let owned_s =
                 {owned_owner; owned_payload}
                 |> string_of_owned write_hash
               in
               Web_events.append ~uuid
                 [
                   Data shuffle_s;
                   Data owned_s;
                   Event (`Shuffle, Some (Hash.hash_string owned_s));
                 ]
             ) (List.combine actual_shufflers shuffles)
         in
         match state with
         | `Open | `Closed -> Lwt.return_unit
         | `Shuffling -> populate_shuffles ()
         | `EncryptedTally ->
            let* () = populate_shuffles () in
            let* () =
              let open Belenios.Election in
              if has_nh_questions (of_string election) then
                Web_events.append ~uuid [Event (`EndShuffles, None)]
              else
                Lwt.return_unit
            in
            let* x = read_file_single_line ~uuid "partial_decryptions.json" in
            let x = Option.value ~default:[] (Option.map old_partial_decryptions_of_string x) in
            Lwt_list.iter_s
              (fun (owned_owner, pd) ->
                let opd =
                  {
                    owned_owner;
                    owned_payload = Hash.hash_string pd;
                  }
                  |> string_of_owned write_hash
                in
                Web_events.append ~uuid
                  [
                    Data pd;
                    Data opd;
                    Event (`PartialDecryption, Some (Hash.hash_string opd));
                  ]
              ) x
         | `Tallied | `Archived ->
            let* () = Web_events.append ~uuid et_ops in
            let* result =
              let* x = read_file_single_line ~uuid "result.json" in
              match x with
              | None -> assert false
              | Some x ->
                 x
                 |> old_election_result_of_string
                      Yojson.Safe.read_json (read_encrypted_tally E.(sread G.of_string))
                      (read_partial_decryption E.(sread G.of_string)) (read_shuffle E.(sread G.of_string))
                 |> Lwt.return
            in
            let flattened_trustees =
              trustees_of_string E.(sread G.of_string) trustees
              |> List.map
                   (function
                    | `Single x -> [x]
                    | `Pedersen p -> Array.to_list p.t_verification_keys
                   )
              |> List.flatten
              |> List.mapi (fun i x -> i + 1, x)
            in
            let* last_et =
              let find_owner name =
                List.find_map
                  (fun (i, x) ->
                    if x.trustee_name = name then
                      Some i
                    else None
                  ) flattened_trustees
                |> Option.get
              in
              match result.shuffles, result.shufflers with
              | None, None -> Lwt.return et
              | Some shuffles, Some shufflers ->
                 let et =
                   let last_shuffle = List.hd (List.rev shuffles) in
                   let nh = last_shuffle.shuffle_ciphertexts in
                   E.E.merge_nh_ciphertexts nh et
                 in
                 let* () =
                   Lwt_list.iter_s
                     (fun (x, name) ->
                       let owned_owner = find_owner name in
                       let shuffle_s = string_of_shuffle E.(swrite G.to_string) x in
                       let owned_payload = Hash.hash_string shuffle_s in
                       let owned = {owned_owner; owned_payload} in
                       let owned_s = string_of_owned write_hash owned in
                       let owned_h = Hash.hash_string owned_s in
                       Web_events.append ~uuid
                         [
                           Data shuffle_s;
                           Data owned_s;
                           Event (`Shuffle, Some owned_h);
                         ]
                     ) (List.combine shuffles shufflers)
                 in
                 let* () = Web_events.append ~uuid [Event (`EndShuffles, None)] in
                 Lwt.return et
              | _ -> assert false
            in
            let* () =
              let find_owner pd =
                List.find_map
                  (fun (i, x) ->
                    if E.E.check_factor last_et x.trustee_public_key pd then
                      Some i
                    else None
                  ) flattened_trustees
                |> Option.get
              in
              Lwt_list.iter_s
                (fun x ->
                  let owned_owner = find_owner x in
                  let pd_s = string_of_partial_decryption E.(swrite G.to_string) x in
                  let owned_payload = Hash.hash_string pd_s in
                  let owned = {owned_owner; owned_payload} in
                  let owned_s = string_of_owned write_hash owned in
                  let owned_h = Hash.hash_string owned_s in
                  Web_events.append ~uuid
                    [
                      Data pd_s;
                      Data owned_s;
                      Event (`PartialDecryption, Some owned_h);
                    ]
                ) result.partial_decryptions
            in
            let payload =
              {result = result.result}
              |> string_of_election_result Yojson.Safe.write_json
            in
            Web_events.append ~uuid
              [
                Data payload;
                Event (`Result, Some (Hash.hash_string payload));
              ]
       in
       let* () =
         Lwt_list.iter_s (fun x -> cleanup_file (uuid /// x))
           [
             "election.json"; "trustees.json"; "public_creds.txt";
             "ballots.jsons"; "encrypted_tally.json";
             "shuffle_hashes.jsons";
             "shuffles.jsons"; "partial_decryptions.json";
             "result.json"; "audit_cache.json";
             "ballots_index.json";
           ]
       in
       let* () = rmdir (uuid /// "ballots") in
       let* () =
         let* () = log "    Verifying resulting directory..." in
         let* r = Lwt_process.exec (belenios_tool, [|belenios_tool; "election"; "verify"; "--dir"; !!uuid_s|]) in
         if r = WEXITED 0 then (
           Lwt.return_unit
         ) else (
           let msg = Printf.sprintf "converted directory for %s does not pass verification" uuid_s in
           Lwt.fail (Failure msg)
         )
       in
       let* () = write_file ~uuid "migration" ["1"] in
       Lwt.return @@ uuid :: accu
     end
  | _ ->
     let msg = Printf.sprintf "partially migrated spool (%s)" uuid_s in
     Lwt.fail (Failure msg)

module UuidSet = Set.Make (struct type t = uuid let compare = compare end)

let get_uuids () =
  let* files =
    Lwt_unix.files_of_directory !Web_config.spool_dir
    |> Lwt_stream.to_list
  in
  let* uuids =
    Lwt_list.fold_left_s
      (fun accu uuid_s ->
        let@ () = fun cont ->
          if uuid_s = "." || uuid_s = ".." then Lwt.return accu else cont ()
        in
        match Uuid.wrap uuid_s with
        | exception _ ->
           let* () = elog "%s is not a valid UUID; ignored" uuid_s in
           Lwt.return accu
        | uuid -> Lwt.return @@ UuidSet.add uuid accu
      ) UuidSet.empty files
  in
  Lwt.return @@ UuidSet.elements uuids

let migrate_spool_to_v1 uuids =
  let version_file = !!"version" in
  let* version = read_file version_file in
  match version with
  | Some ["1"] -> Lwt.return 0
  | None ->
     let* processed =
       Lwt_list.fold_left_s
         (fun accu uuid -> migrate_election_to_v1 uuid accu)
         [] uuids
     in
     let* () = write_file version_file ["tmp"] in
     let* () =
       Lwt_list.iter_s
         (fun uuid ->
           let file = uuid /// "migration" in
           Lwt.catch
             (fun () -> Lwt_unix.unlink file)
             (fun _ -> Lwt.return_unit)
         ) processed
     in
     let* () = write_file version_file ["1"] in
     Lwt.return 0
  | _ ->
     let* () = elog "Unsupported version!" in
     Lwt.return 1

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: belenios-migrate <spool>\n%!";
    exit 1
  ) else (
    let spool = Sys.argv.(1) in
    Web_config.spool_dir := spool;
    Printf.printf "Will use %s\n%!" belenios_tool;
    Printf.printf "Migrating %s...\n%!" spool;
    let r =
      Lwt_main.run
        begin
          let* uuids = get_uuids () in
          let* old_elections = Lwt_list.filter_s is_election_with_old_crypto uuids in
          match old_elections with
          | [] -> migrate_spool_to_v1 uuids
          | _ ->
             let* () = elog "The following elections use old crypto:" in
             let* () = Lwt_list.iter_s (fun x -> elog "  %s" (Uuid.unwrap x)) old_elections in
             let* () = elog "Please delete them before migrating the spool." in
             Lwt.return 2
        end
    in
    Printf.printf "Done.\n%!";
    exit r
  )
