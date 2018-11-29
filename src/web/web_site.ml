(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2018 Inria                                           *)
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

open Lwt
open Platform
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common
open Web_services

let source_file = ref "belenios.tar.gz"
let maxmailsatonce = ref 1000
let uuid_length = ref None
let default_group = ref ""

let ( / ) = Filename.concat

module PString = String

open Eliom_service
open Eliom_registration

module T = Web_templates

let raw_find_election uuid =
  let%lwt raw_election = Web_persist.get_raw_election uuid in
  match raw_election with
  | Some raw_election ->
     return (Election.of_string raw_election)
  | _ -> Lwt.fail Not_found

module WCacheTypes = struct
  type key = uuid
  type value = Yojson.Safe.json election
end

module WCache = Ocsigen_cache.Make (WCacheTypes)

let find_election =
  let cache = new WCache.cache raw_find_election ~timer:3600. 100 in
  fun x -> cache#find x

let dump_passwords uuid db =
  List.map (fun line -> PString.concat "," line) db |>
    write_file ~uuid "passwords.csv"

let validate_election uuid se =
  let uuid_s = raw_string_of_uuid uuid in
  (* voters *)
  let () =
    if se.se_voters = [] then failwith "no voters"
  in
  (* passwords *)
  let () =
    match se.se_metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] ->
       if not @@ List.for_all (fun v -> v.sv_password <> None) se.se_voters then
         failwith "some passwords are missing"
    | _ -> ()
  in
  (* credentials *)
  let () =
    if not se.se_public_creds_received then
      failwith "public credentials are missing"
  in
  (* trustees *)
  let group = Group.of_string se.se_group in
  let module G = (val group : GROUP) in
  let%lwt y, trustees, pk_or_tp, private_keys =
    match se.se_threshold_trustees with
    | None ->
       let module KG = Trustees.MakeSimple (G) (LwtRandom) in
       let%lwt trustees, public_keys, private_key =
         match se.se_public_keys with
         | [] ->
            let%lwt private_key = KG.generate () in
            let%lwt public_key = KG.prove private_key in
            return (None, [public_key], `KEY private_key)
         | _ :: _ ->
            let private_key =
              List.fold_left (fun accu {st_private_key; _} ->
                  match st_private_key with
                  | Some x -> x :: accu
                  | None -> accu
                ) [] se.se_public_keys
            in
            let private_key = match private_key with
              | [] -> `None
              | [x] -> `KEY x
              | _ -> failwith "multiple private keys"
            in
            return (
                Some (List.map (fun {st_id; _} -> st_id) se.se_public_keys),
                (List.map
                   (fun {st_public_key; _} ->
                     if st_public_key = "" then failwith "some public keys are missing";
                     trustee_public_key_of_string G.read st_public_key
                   ) se.se_public_keys),
                private_key)
       in
       let y = KG.combine (Array.of_list public_keys) in
       return (y, trustees, `PK public_keys, private_key)
    | Some ts ->
       match se.se_threshold_parameters with
       | None -> failwith "key establishment not finished"
       | Some tp ->
          let tp = threshold_parameters_of_string G.read tp in
          let module P = Trustees.MakePKI (G) (LwtRandom) in
          let module C = Trustees.MakeChannels (G) (LwtRandom) (P) in
          let module K = Trustees.MakePedersen (G) (LwtRandom) (P) (C) in
          let trustees = List.map (fun {stt_id; _} -> stt_id) ts in
          let private_keys =
            List.map (fun {stt_voutput; _} ->
                match stt_voutput with
                | Some v ->
                   let voutput = voutput_of_string G.read v in
                   voutput.vo_private_key
                | None -> failwith "inconsistent state"
              ) ts
          in
          let y = K.combine tp in
          return (y, Some trustees, `TP tp, `KEYS private_keys)
  in
  (* election parameters *)
  let e_server_is_trustee = match private_keys with
      | `KEY _ -> Some true
      | `None | `KEYS _ -> None
  in
  let metadata = {
      se.se_metadata with
      e_trustees = trustees;
      e_server_is_trustee;
    } in
  let template = se.se_questions in
  let params = {
    e_description = template.t_description;
    e_name = template.t_name;
    e_public_key = {wpk_group = G.group; wpk_y = y};
    e_questions = template.t_questions;
    e_uuid = uuid;
  } in
  let raw_election = string_of_params (write_wrapped_pubkey G.write_group G.write) params in
  (* write election files to disk *)
  let dir = !spool_dir / uuid_s in
  let create_file fname what xs =
    Lwt_io.with_file
      ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
      ~perm:0o600 ~mode:Lwt_io.Output (dir / fname)
      (fun oc ->
        Lwt_list.iter_s
          (fun v ->
            let%lwt () = Lwt_io.write oc (what v) in
            Lwt_io.write oc "\n") xs)
  in
  let%lwt () =
    match pk_or_tp with
    | `PK pk -> create_file "public_keys.jsons" (string_of_trustee_public_key G.write) pk
    | `TP tp -> create_file "threshold.json" (string_of_threshold_parameters G.write) [tp]
  in
  let%lwt () = create_file "voters.txt" (fun x -> x.sv_id) se.se_voters in
  let%lwt () = create_file "metadata.json" string_of_metadata [metadata] in
  let%lwt () = create_file "election.json" (fun x -> x) [raw_election] in
  (* construct Web_election instance *)
  let election = Election.of_string raw_election in
  let module W = (val Election.get_group election) in
  let module E = Election.Make (W) (LwtRandom) in
  let module B = Web_election.Make (E) in
  (* initialize credentials *)
  let%lwt () =
    let fname = !spool_dir / uuid_s / "public_creds.txt" in
    match%lwt read_file fname with
    | Some xs ->
       let%lwt () = Web_persist.init_credential_mapping uuid xs in
       Lwt_unix.unlink fname
    | None -> return_unit
  in
  (* create file with private keys, if any *)
  let%lwt () =
    match private_keys with
    | `None -> return_unit
    | `KEY x -> create_file "private_key.json" string_of_number [x]
    | `KEYS x -> create_file "private_keys.jsons" (fun x -> x) x
  in
  (* clean up draft *)
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "draft.json") in
  (* write passwords *)
  let%lwt () =
    match metadata.e_auth_config with
    | Some [{auth_system = "password"; _}] ->
       let db =
         List.filter_map (fun v ->
             let _, login = split_identity v.sv_id in
             match v.sv_password with
             | Some (salt, hashed) -> Some [login; salt; hashed]
             | None -> None
           ) se.se_voters
       in
       if db <> [] then dump_passwords uuid db else return_unit
    | _ -> return_unit
  in
  (* finish *)
  let%lwt () = Web_persist.set_election_state uuid `Open in
  Web_persist.set_election_date `Validation uuid (now ())

let delete_sensitive_data uuid =
  let uuid_s = raw_string_of_uuid uuid in
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "state.json") in
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "decryption_tokens.json") in
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "partial_decryptions.json") in
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "extended_records.jsons") in
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "credential_mappings.jsons") in
  let%lwt () = rmdir (!spool_dir / uuid_s / "ballots") in
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "private_key.json") in
  let%lwt () = cleanup_file (!spool_dir / uuid_s / "private_keys.jsons") in
  return_unit

let archive_election uuid =
  let%lwt () = delete_sensitive_data uuid in
  let%lwt () = Web_persist.set_election_date `Archive uuid (now ()) in
  return_unit

let delete_election uuid =
  let uuid_s = raw_string_of_uuid uuid in
  let%lwt () = delete_sensitive_data uuid in
  let%lwt election = raw_find_election uuid in
  let%lwt metadata = Web_persist.get_election_metadata uuid in
  let de_template = {
      t_description = "";
      t_name = election.e_params.e_name;
      t_questions =
        Array.map (fun q ->
            {
              q_answers = Array.map (fun _ -> "") q.q_answers;
              q_blank = q.q_blank;
              q_min = q.q_min;
              q_max = q.q_max;
              q_question = "";
            }
          ) election.e_params.e_questions
    }
  in
  let de_owner = match metadata.e_owner with
    | None -> Printf.ksprintf failwith "election %s has no owner" uuid_s
    | Some x -> x
  in
  let%lwt de_date =
    let%lwt date = Web_persist.get_election_date `Tally uuid in
    match date with
    | Some x -> return x
    | None ->
       let%lwt date = Web_persist.get_election_date `Validation uuid in
       match date with
       | Some x -> return x
       | None ->
          let%lwt date = Web_persist.get_election_date `Creation uuid in
          match date with
          | Some x -> return x
          | None -> return default_validation_date
  in
  let de_authentication_method = match metadata.e_auth_config with
    | Some [{auth_system = "cas"; auth_config; _}] ->
       let server = List.assoc "server" auth_config in
       `CAS server
    | Some [{auth_system = "password"; _}] -> `Password
    | _ -> `Unknown
  in
  let de_credential_method = match metadata.e_cred_authority with
    | Some "server" -> `Automatic
    | _ -> `Manual
  in
  let%lwt de_trustees_threshold =
    let%lwt threshold = Web_persist.get_threshold uuid in
    match threshold with
    | None -> return None
    | Some x ->
       let x = threshold_parameters_of_string Yojson.Safe.read_json x in
       return (Some x.t_threshold)
  in
  let%lwt pks = Web_persist.get_public_keys uuid in
  let%lwt voters = Web_persist.get_voters uuid in
  let%lwt ballots = Web_persist.get_ballot_hashes uuid in
  let%lwt result = Web_persist.get_election_result uuid in
  let de = {
      de_uuid = uuid;
      de_template;
      de_owner;
      de_nb_voters = (match voters with None -> 0 | Some x -> List.length x);
      de_nb_ballots = List.length ballots;
      de_date;
      de_tallied = result <> None;
      de_authentication_method;
      de_credential_method;
      de_nb_trustees = (match pks with None -> 0 | Some x -> List.length x);
      de_trustees_threshold;
      de_server_is_trustee = metadata.e_server_is_trustee = Some true;
    }
  in
  let%lwt () = write_file ~uuid "deleted.json" [string_of_deleted_election de] in
  let files_to_delete = [
      "election.json";
      "ballots.jsons";
      "dates.json";
      "encrypted_tally.json";
      "metadata.json";
      "passwords.csv";
      "public_creds.txt";
      "public_keys.jsons";
      "threshold.json";
      "records";
      "result.json";
      "voters.txt";
      "archive.zip";
    ]
  in
  let%lwt () = Lwt_list.iter_p (fun x ->
                   cleanup_file (!spool_dir / uuid_s / x)
                 ) files_to_delete
  in
  return_unit

let () = Any.register ~service:home
  (fun () () ->
    let%lwt () = Eliom_reference.unset Web_state.cont in
    Redirection.send (Redirection admin)
  )

let get_elections_by_owner_sorted u =
  let%lwt elections = Web_persist.get_elections_by_owner u in
  let filter kind =
    List.filter (fun (x, _, _, _) -> x = kind) elections |>
    List.map (fun (_, a, b, c) -> a, b, c)
  in
  let draft = filter `Draft in
  let elections = filter `Validated in
  let tallied = filter `Tallied in
  let archived = filter `Archived in
  let sort l =
    List.sort (fun (_, x, _) (_, y, _) -> datetime_compare x y) l |>
    List.map (fun (x, _, y) -> x, y)
  in
  return (sort draft, sort elections, sort tallied, sort archived)

let with_site_user f =
  match%lwt Web_state.get_site_user () with
  | Some u -> f u
  | None -> forbidden ()

let () =
  Redirection.register ~service:admin_gdpr_accept
    (fun () () ->
      let%lwt () = Eliom_reference.set Web_state.show_cookie_disclaimer false in
      return (Redirection admin)
    )

let () = Html.register ~service:admin
  (fun () () ->
    let%lwt gdpr = Eliom_reference.get Web_state.show_cookie_disclaimer in
    if gdpr then T.admin_gdpr () else
    let cont () = Redirection.send (Redirection admin) in
    let%lwt () = Eliom_reference.set Web_state.cont [cont] in
    let%lwt site_user = Web_state.get_site_user () in
    let%lwt elections =
      match site_user with
      | None -> return None
      | Some u ->
         let%lwt elections = get_elections_by_owner_sorted u in
         return @@ Some elections
    in
    T.admin ~elections ()
  )

let () = File.register ~service:source_code
  ~content_type:"application/x-gzip"
  (fun () () -> return !source_file)

let generate_uuid =
  let gen = Uuidm.v4_gen (Random.State.make_self_init ()) in
  fun () ->
  match !uuid_length with
  | Some length ->
     let%lwt token = generate_token ~length () in
     return @@ uuid_of_raw_string token
  | None -> return @@ uuid_of_raw_string @@ Uuidm.to_string @@ gen ()

let redir_preapply s u () = Redirection.send (Redirection (preapply s u))

let create_new_election owner cred auth =
  let e_cred_authority = match cred with
    | `Automatic -> Some "server"
    | `Manual -> None
  in
  let e_auth_config = match auth with
    | `Password -> Some [{auth_system = "password"; auth_instance = "password"; auth_config = []}]
    | `Dummy -> Some [{auth_system = "dummy"; auth_instance = "dummy"; auth_config = []}]
    | `CAS server -> Some [{auth_system = "cas"; auth_instance = "cas"; auth_config = ["server", server]}]
  in
  let%lwt uuid = generate_uuid () in
  let%lwt token = generate_token () in
  let se_metadata = {
    e_owner = Some owner;
    e_auth_config;
    e_cred_authority;
    e_trustees = None;
    e_languages = Some ["en"; "fr"];
    e_contact = None;
    e_server_is_trustee = None;
  } in
  let se_questions = {
    t_description = default_description;
    t_name = default_name;
    t_questions = default_questions;
  } in
  let se = {
    se_owner = owner;
    se_group = !default_group;
    se_voters = [];
    se_questions;
    se_public_keys = [];
    se_metadata;
    se_public_creds = token;
    se_public_creds_received = false;
    se_threshold = None;
    se_threshold_trustees = None;
    se_threshold_parameters = None;
    se_threshold_error = None;
    se_creation_date = Some (now ());
  } in
  let%lwt () = Lwt_unix.mkdir (!spool_dir / raw_string_of_uuid uuid) 0o700 in
  let%lwt () = Web_persist.set_draft_election uuid se in
  redir_preapply election_draft uuid ()

let () = Html.register ~service:election_draft_pre
  (fun () () -> T.election_draft_pre ())

let () = Any.register ~service:election_draft_new
  (fun () (credmgmt, (auth, cas_server)) ->
    with_site_user (fun u ->
        let%lwt credmgmt = match credmgmt with
          | Some "auto" -> return `Automatic
          | Some "manual" -> return `Manual
          | _ -> fail_http 400
        in
        let%lwt auth = match auth with
          | Some "password" -> return `Password
          | Some "dummy" -> return `Dummy
          | Some "cas" -> return @@ `CAS cas_server
          | _ -> fail_http 400
        in
        create_new_election u credmgmt auth
      )
  )

let with_draft_election_ro uuid f =
  with_site_user (fun u ->
      match%lwt Web_persist.get_draft_election uuid with
      | None -> fail_http 404
      | Some se -> if se.se_owner = u then f se else forbidden ()
    )

let () =
  Html.register ~service:election_draft
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          T.election_draft uuid se ()
        )
    )

let () =
  Any.register ~service:election_draft_trustees
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          match se.se_threshold_trustees with
          | None -> T.election_draft_trustees uuid se () >>= Html.send
          | Some _ -> redir_preapply election_draft_threshold_trustees uuid ()
        )
    )

let () =
  Html.register ~service:election_draft_threshold_trustees
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          T.election_draft_threshold_trustees uuid se ()
        )
    )

let () =
  Html.register ~service:election_draft_credential_authority
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          T.election_draft_credential_authority uuid se ()
        )
    )

let election_draft_mutex = Lwt_mutex.create ()

let with_draft_election ?(save = true) uuid f =
  with_site_user (fun u ->
      Lwt_mutex.with_lock election_draft_mutex (fun () ->
          match%lwt Web_persist.get_draft_election uuid with
          | None -> fail_http 404
          | Some se ->
          if se.se_owner = u then (
            try%lwt
              let%lwt r = f se in
              let%lwt () = if save then Web_persist.set_draft_election uuid se else return_unit in
              return r
            with e ->
              let msg = match e with Failure s -> s | _ -> Printexc.to_string e in
              let service = preapply election_draft uuid in
              T.generic_page ~title:"Error" ~service msg () >>= Html.send
          ) else forbidden ()
        )
    )

let () =
  Any.register ~service:election_draft_languages
    (fun uuid languages ->
      with_draft_election uuid (fun se ->
          let langs = languages_of_string languages in
          match langs with
          | [] ->
             let service = preapply election_draft uuid in
             T.generic_page ~title:"Error" ~service
               "You must select at least one language!" () >>= Html.send
          | _ :: _ ->
             let unavailable =
               List.filter (fun x ->
                   not (List.mem x available_languages)
                 ) langs
             in
             match unavailable with
             | [] ->
                se.se_metadata <- {
                   se.se_metadata with
                   e_languages = Some langs
                 };
                redir_preapply election_draft uuid ()
             | l :: _ ->
                let service = preapply election_draft uuid in
                T.generic_page ~title:"Error" ~service
                  ("No such language: " ^ l) () >>= Html.send
        )
    )

let () =
  Any.register ~service:election_draft_contact
    (fun uuid contact ->
      with_draft_election uuid (fun se ->
          let contact =
            if contact = "" || contact = default_contact then
              None
            else Some contact
          in
          se.se_metadata <- {
              se.se_metadata with
              e_contact = contact
            };
          redir_preapply election_draft uuid ()
        )
    )

let () =
  Any.register ~service:election_draft_description
    (fun uuid (name, description) ->
      with_draft_election uuid (fun se ->
          se.se_questions <- {se.se_questions with
                               t_name = name;
                               t_description = description;
                             };
          redir_preapply election_draft uuid ()
        )
    )

let generate_password metadata langs title url id =
  let email, login = split_identity id in
  let%lwt salt = generate_token () in
  let%lwt password = generate_token () in
  let hashed = sha256_hex (salt ^ password) in
  let bodies = List.map (fun lang ->
    let module L = (val Web_i18n.get_lang lang) in
    let contact = T.contact_footer metadata L.please_contact in
    Printf.sprintf L.mail_password title login password url contact
  ) langs in
  let body = PString.concat "\n\n----------\n\n" bodies in
  let body = body ^ "\n\n-- \nBelenios" in
  let subject =
    let lang = List.hd langs in
    let module L = (val Web_i18n.get_lang lang) in
    Printf.sprintf L.mail_password_subject title
  in
  let%lwt () = send_email email subject body in
  return (salt, hashed)

let handle_password se uuid ~force voters =
  if List.length voters > !maxmailsatonce then
    Lwt.fail (Failure (Printf.sprintf "Cannot send passwords, there are too many voters (max is %d)" !maxmailsatonce))
  else if se.se_questions.t_name = default_name then
    Lwt.fail (Failure "The election name has not been edited!")
  else
  let title = se.se_questions.t_name in
  let url = Eliom_uri.make_string_uri ~absolute:true ~service:election_home
    (uuid, ()) |> rewrite_prefix
  in
  let langs = get_languages se.se_metadata.e_languages in
  let%lwt () =
    Lwt_list.iter_s (fun id ->
        match id.sv_password with
        | Some _ when not force -> return_unit
        | None | Some _ ->
           let%lwt x = generate_password se.se_metadata langs title url id.sv_id in
           return (id.sv_password <- Some x)
      ) voters
  in
  let service = preapply election_draft uuid in
  T.generic_page ~title:"Success" ~service
    "Passwords have been generated and mailed!" () >>= Html.send

let () =
  Any.register ~service:election_draft_auth_genpwd
    (fun uuid () ->
      with_draft_election uuid (fun se ->
          handle_password se uuid ~force:false se.se_voters
        )
    )

let () =
  Any.register ~service:election_regenpwd
    (fun uuid () ->
      T.regenpwd uuid () >>= Html.send)

let find_user_id uuid user =
  let uuid_s = raw_string_of_uuid uuid in
  let db = Lwt_io.lines_of_file (!spool_dir / uuid_s / "voters.txt") in
  let%lwt db = Lwt_stream.to_list db in
  let rec loop = function
    | [] -> Lwt.fail Not_found
    | id :: xs ->
       let _, login = split_identity id in
       if login = user then return id else loop xs
  in loop db

let load_password_db uuid =
  let uuid_s = raw_string_of_uuid uuid in
  let db = !spool_dir / uuid_s / "passwords.csv" in
  Lwt_preemptive.detach Csv.load db

let rec replace_password username ((salt, hashed) as p) = function
  | [] -> []
  | ((username' :: _ :: _ :: rest) as x) :: xs ->
     if username = username' then (username :: salt :: hashed :: rest) :: xs
     else x :: (replace_password username p xs)
  | x :: xs -> x :: (replace_password username p xs)

let () =
  Any.register ~service:election_regenpwd_post
    (fun uuid user ->
      with_site_user (fun u ->
          let%lwt election = find_election uuid in
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          if metadata.e_owner = Some u then (
            let title = election.e_params.e_name in
            let url = Eliom_uri.make_string_uri
                        ~absolute:true ~service:election_home
                        (uuid, ()) |> rewrite_prefix
            in
            let service = preapply election_admin uuid in
            (try%lwt
               let%lwt id = find_user_id uuid user in
               let langs = get_languages metadata.e_languages in
               let%lwt db = load_password_db uuid in
               let%lwt x = generate_password metadata langs title url id in
               let db = replace_password user x db in
               let%lwt () = dump_passwords uuid db in
               T.generic_page ~title:"Success" ~service
                 ("A new password has been mailed to " ^ id ^ ".") ()
               >>= Html.send
              with Not_found ->
                T.generic_page ~title:"Error" ~service
                  (user ^ " is not a registered user for this election.") ()
                >>= Html.send
            )
          ) else forbidden ()
        )
    )

let () =
  Html.register ~service:election_draft_questions
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          T.election_draft_questions uuid se ()
        )
    )

let () =
  Any.register ~service:election_draft_questions_post
    (fun uuid template ->
      with_draft_election uuid (fun se ->
          se.se_questions <- template_of_string template;
          redir_preapply election_draft uuid ()
        )
    )

let () =
  Html.register ~service:election_draft_voters
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          T.election_draft_voters uuid se !maxmailsatonce ()
        )
    )

(* see http://www.regular-expressions.info/email.html *)
let identity_rex = Pcre.regexp
  ~flags:[`CASELESS]
  "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}(,[A-Z0-9._%+-]+)?$"

let is_identity x =
  try ignore (Pcre.pcre_exec ~rex:identity_rex x); true
  with Not_found -> false

let merge_voters a b f =
  let existing = List.fold_left (fun accu sv ->
    SSet.add sv.sv_id accu
  ) SSet.empty a in
  let _, res = List.fold_left (fun (existing, accu) sv_id ->
    if SSet.mem sv_id existing then
      (existing, accu)
    else
      (SSet.add sv_id existing, {sv_id; sv_password = f sv_id} :: accu)
  ) (existing, List.rev a) b in
  List.rev res

let () =
  Any.register ~service:election_draft_voters_add
    (fun uuid voters ->
      with_draft_election uuid (fun se ->
          if se.se_public_creds_received then
            forbidden ()
          else (
            let voters = Pcre.split voters in
            let () =
              try
                let bad = List.find (fun x -> not (is_identity x)) voters in
                Printf.ksprintf failwith "%S is not a valid identity" bad
              with Not_found -> ()
            in
            se.se_voters <- merge_voters se.se_voters voters (fun _ -> None);
            redir_preapply election_draft_voters uuid ()
          )
        )
    )

let () =
  Any.register ~service:election_draft_voters_remove
    (fun uuid voter ->
      with_draft_election uuid (fun se ->
          if se.se_public_creds_received then
            forbidden ()
          else (
            se.se_voters <- List.filter (fun v -> v.sv_id <> voter) se.se_voters;
            redir_preapply election_draft_voters uuid ()
          )
        )
    )

let () =
  Any.register ~service:election_draft_voters_passwd
    (fun uuid voter ->
      with_draft_election uuid (fun se ->
          let voter = List.filter (fun v -> v.sv_id = voter) se.se_voters in
          handle_password se uuid ~force:true voter
        )
    )

let () =
  Any.register ~service:election_draft_trustee_add
    (fun uuid st_id ->
      with_draft_election uuid (fun se ->
          if is_email st_id then (
            let%lwt st_token = generate_token () in
            let trustee = {st_id; st_token; st_public_key = ""; st_private_key = None} in
            se.se_public_keys <- se.se_public_keys @ [trustee];
            redir_preapply election_draft_trustees uuid ()
          ) else (
            let msg = st_id ^ " is not a valid e-mail address!" in
            let service = preapply election_draft_trustees uuid in
            T.generic_page ~title:"Error" ~service msg () >>= Html.send
          )
        )
    )

let () =
  Any.register ~service:election_draft_trustee_add_server
    (fun uuid () ->
      with_draft_election uuid (fun se ->
          let st_id = "server" and st_token = "" in
          let module G = (val Group.of_string se.se_group) in
          let module K = Trustees.MakeSimple (G) (LwtRandom) in
          let%lwt private_key = K.generate () in
          let%lwt public_key = K.prove private_key in
          let st_public_key = string_of_trustee_public_key G.write public_key in
          let st_private_key = Some private_key in
          let trustee = {st_id; st_token; st_public_key; st_private_key} in
          se.se_public_keys <- se.se_public_keys @ [trustee];
          redir_preapply election_draft_trustees uuid ()
        )
    )

let () =
  Any.register ~service:election_draft_trustee_del
    (fun uuid index ->
      with_draft_election uuid (fun se ->
          let trustees =
            se.se_public_keys |>
              List.mapi (fun i x -> i, x) |>
              List.filter (fun (i, _) -> i <> index) |>
              List.map snd
          in
          se.se_public_keys <- trustees;
          redir_preapply election_draft_trustees uuid ()
        )
    )

let () =
  Html.register ~service:election_draft_credentials
    (fun (uuid, token) () ->
      match%lwt Web_persist.get_draft_election uuid with
      | None -> fail_http 404
      | Some se -> T.election_draft_credentials token uuid se ()
    )

let wrap_handler f =
  try%lwt f ()
  with
  | e -> T.generic_page ~title:"Error" (Printexc.to_string e) () >>= Html.send

let handle_credentials_post uuid token creds =
  match%lwt Web_persist.get_draft_election uuid with
  | None -> fail_http 404
  | Some se ->
  if se.se_public_creds <> token then forbidden () else
  if se.se_public_creds_received then forbidden () else
  let module G = (val Group.of_string se.se_group : GROUP) in
  let fname = !spool_dir / raw_string_of_uuid uuid / "public_creds.txt" in
  let%lwt () =
    Lwt_mutex.with_lock election_draft_mutex
      (fun () ->
        Lwt_io.with_file
          ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
          ~perm:0o600 ~mode:Lwt_io.Output fname
          (fun oc -> Lwt_io.write_chars oc creds)
      )
  in
  let%lwt () =
    let i = ref 1 in
    match%lwt read_file fname with
    | Some xs ->
       return (
           List.iter (fun x ->
               try
                 let x = G.of_string x in
                 if not (G.check x) then raise Exit;
                 incr i
               with _ ->
                 Printf.ksprintf failwith "invalid credential at line %d" !i
             ) xs
         )
    | None -> return_unit
  in
  let () = se.se_metadata <- {se.se_metadata with e_cred_authority = None} in
  let () = se.se_public_creds_received <- true in
  let%lwt () = Web_persist.set_draft_election uuid se in
  T.generic_page ~title:"Success"
    "Credentials have been received and checked!" () >>= Html.send

let () =
  Any.register ~service:election_draft_credentials_post
    (fun (uuid, token) creds ->
     let s = Lwt_stream.of_string creds in
     wrap_handler (fun () -> handle_credentials_post uuid token s))

let () =
  Any.register ~service:election_draft_credentials_post_file
    (fun (uuid, token) creds ->
     let s = Lwt_io.chars_of_file creds.Ocsigen_extensions.tmp_filename in
     wrap_handler (fun () -> handle_credentials_post uuid token s))

module CG = Credential.MakeGenerate (LwtRandom)

let () =
  Any.register ~service:election_draft_credentials_server
    (fun uuid () ->
      with_draft_election uuid (fun se ->
          let nvoters = List.length se.se_voters in
          if nvoters > !maxmailsatonce then
            Lwt.fail (Failure (Printf.sprintf "Cannot send credentials, there are too many voters (max is %d)" !maxmailsatonce))
          else if nvoters = 0 then
            Lwt.fail (Failure "No voters")
          else if se.se_questions.t_name = default_name then
            Lwt.fail (Failure "The election name has not been edited!")
          else if se.se_public_creds_received then
            forbidden ()
          else (
            let () = se.se_metadata <- {se.se_metadata with
                                         e_cred_authority = Some "server"
                                       } in
            let title = se.se_questions.t_name in
            let url = Eliom_uri.make_string_uri
                        ~absolute:true ~service:election_home
                        (uuid, ()) |> rewrite_prefix
            in
            let module G = (val Group.of_string se.se_group : GROUP) in
            let module CD = Credential.MakeDerive (G) in
            let%lwt creds =
              Lwt_list.fold_left_s (fun accu v ->
                  let email, _ = split_identity v.sv_id in
                  let cas =
                    match se.se_metadata.e_auth_config with
                    | Some [{auth_system = "cas"; _}] -> true
                    | _ -> false
                  in
                  let%lwt cred = CG.generate () in
                  let pub_cred =
                    let x = CD.derive uuid cred in
                    let y = G.(g **~ x) in
                    G.to_string y
                  in
                  let langs = get_languages se.se_metadata.e_languages in
                  let bodies = List.map (fun lang ->
                                   let module L = (val Web_i18n.get_lang lang) in
                                   let intro = if cas then L.mail_credential_cas else L.mail_credential_password in
                                   let contact = T.contact_footer se.se_metadata L.please_contact in
                                   Printf.sprintf L.mail_credential title intro cred url contact
                                 ) langs in
                  let body = PString.concat "\n\n----------\n\n" bodies in
                  let body = body ^ "\n\n-- \nBelenios" in
                  let subject =
                    let lang = List.hd langs in
                    let module L = (val Web_i18n.get_lang lang) in
                    Printf.sprintf L.mail_credential_subject title
                  in
                  let%lwt () = send_email email subject body in
                  return @@ SSet.add pub_cred accu
                ) SSet.empty se.se_voters
            in
            let creds = SSet.elements creds in
            let fname = !spool_dir / raw_string_of_uuid uuid / "public_creds.txt" in
            let%lwt () =
              Lwt_io.with_file
                ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
                ~perm:0o600 ~mode:Lwt_io.Output fname
                (fun oc ->
                  Lwt_list.iter_s (Lwt_io.write_line oc) creds)
            in
            se.se_public_creds_received <- true;
            let service = preapply election_draft uuid in
            T.generic_page ~title:"Success" ~service
              "Credentials have been generated and mailed!" () >>= Html.send
          )
        )
    )

let () =
  Html.register ~service:election_draft_trustee
    (fun (uuid, token) () ->
      match%lwt Web_persist.get_draft_election uuid with
      | None -> fail_http 404
      | Some se -> T.election_draft_trustee token uuid se ()
    )

let () =
  Any.register ~service:election_draft_trustee_post
    (fun (uuid, token) public_key ->
     if token = "" then forbidden () else
     wrap_handler
       (fun () ->
         let%lwt () =
           Lwt_mutex.with_lock election_draft_mutex
             (fun () ->
               match%lwt Web_persist.get_draft_election uuid with
               | None -> fail_http 404
               | Some se ->
                  let t = List.find (fun x -> token = x.st_token) se.se_public_keys in
                  let module G = (val Group.of_string se.se_group : GROUP) in
                  let pk = trustee_public_key_of_string G.read public_key in
                  let module KG = Trustees.MakeSimple (G) (LwtRandom) in
                  if not (KG.check pk) then failwith "invalid public key";
                  (* we keep pk as a string because of G.t *)
                  t.st_public_key <- public_key;
                  Web_persist.set_draft_election uuid se
             )
         in
         T.generic_page ~title:"Success"
           "Your key has been received and checked!"
           () >>= Html.send
       )
    )

let () =
  Any.register ~service:election_draft_confirm
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          T.election_draft_confirm uuid se () >>= Html.send
        )
    )

let () =
  Any.register ~service:election_draft_create
    (fun uuid () ->
      with_draft_election ~save:false uuid (fun se ->
          try%lwt
            let%lwt () = validate_election uuid se in
            redir_preapply election_admin uuid ()
          with e ->
            T.new_election_failure (`Exception e) () >>= Html.send
        )
    )

let destroy_election uuid =
  rmdir (!spool_dir / raw_string_of_uuid uuid)

let () =
  Any.register ~service:election_draft_destroy
    (fun uuid () ->
      with_draft_election ~save:false uuid (fun _ ->
          let%lwt () = destroy_election uuid in
          Redirection.send (Redirection admin)
        )
    )

let () =
  Html.register ~service:election_draft_import
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          let%lwt _, a, b, c = get_elections_by_owner_sorted se.se_owner in
          T.election_draft_import uuid se (a, b, c) ()
        )
    )

let () =
  Any.register ~service:election_draft_import_post
    (fun uuid from ->
      let from = uuid_of_raw_string from in
      with_draft_election uuid (fun se ->
          let from_s = raw_string_of_uuid from in
          let%lwt voters = Web_persist.get_voters from in
          let%lwt passwords = Web_persist.get_passwords from in
          let get_password =
            match passwords with
            | None -> fun _ -> None
            | Some p -> fun sv_id ->
                        let _, login = split_identity sv_id in
                        try Some (SMap.find login p)
                        with Not_found -> None
          in
          match voters with
          | Some voters ->
             if se.se_public_creds_received then
               forbidden ()
             else (
               se.se_voters <- merge_voters se.se_voters voters get_password;
               redir_preapply election_draft_voters uuid ()
             )
          | None ->
             T.generic_page ~title:"Error"
               ~service:(preapply election_draft_voters uuid)
               (Printf.sprintf
                  "Could not retrieve voter list from election %s"
                  from_s)
               () >>= Html.send
        )
    )

let () =
  Html.register ~service:election_draft_import_trustees
    (fun uuid () ->
      with_draft_election_ro uuid (fun se ->
          let%lwt _, a, b, c = get_elections_by_owner_sorted se.se_owner in
          T.election_draft_import_trustees uuid se (a, b, c) ()
        )
    )

exception TrusteeImportError of string

let () =
  Any.register ~service:election_draft_import_trustees_post
    (fun uuid from ->
      let from = uuid_of_raw_string from in
      with_draft_election uuid (fun se ->
          let%lwt metadata = Web_persist.get_election_metadata from in
          let%lwt threshold = Web_persist.get_threshold from in
          let%lwt public_keys = Web_persist.get_public_keys from in
          try%lwt
               match metadata.e_trustees, threshold, public_keys with
               | Some ts, Some raw_tp, None ->
                  if se.se_threshold_trustees <> None then
                    raise (TrusteeImportError "Importing threshold trustees after having already added ones is not supported");
                  let module G = (val Group.of_string se.se_group : GROUP) in
                  let module P = Trustees.MakePKI (G) (LwtRandom) in
                  let module C = Trustees.MakeChannels (G) (LwtRandom) (P) in
                  let module K = Trustees.MakePedersen (G) (LwtRandom) (P) (C) in
                  let tp = threshold_parameters_of_string G.read raw_tp in
                  if not (K.check tp) then
                    raise (TrusteeImportError "Imported threshold trustees are invalid for this election!");
                  let%lwt privs = Web_persist.get_private_keys from in
                  let%lwt se_threshold_trustees =
                    match privs with
                    | Some privs ->
                       let rec loop ts pubs privs accu =
                         match ts, pubs, privs with
                         | stt_id :: ts, vo_public_key :: pubs, vo_private_key :: privs ->
                            let%lwt stt_token = generate_token () in
                            let stt_voutput = {vo_public_key; vo_private_key} in
                            let stt_voutput = Some (string_of_voutput G.write stt_voutput) in
                            let stt = {
                                stt_id; stt_token; stt_voutput;
                                stt_step = Some 7; stt_cert = None;
                                stt_polynomial = None; stt_vinput = None;
                              } in
                            loop ts pubs privs (stt :: accu)
                         | [], [], [] -> return (List.rev accu)
                         | _, _, _ -> raise (TrusteeImportError "Inconsistency in imported election!")
                       in loop ts (Array.to_list tp.t_verification_keys) privs []
                    | None -> raise (TrusteeImportError "Encrypted decryption keys are missing!")
                  in
                  se.se_threshold <- Some tp.t_threshold;
                  se.se_threshold_trustees <- Some se_threshold_trustees;
                  se.se_threshold_parameters <- Some raw_tp;
                  redir_preapply election_draft_threshold_trustees uuid ()
               | Some ts, None, Some pks when List.length ts = List.length pks ->
                  let module G = (val Group.of_string se.se_group) in
                  let module KG = Trustees.MakeSimple (G) (LwtRandom) in
                  let%lwt trustees =
                    List.combine ts pks
                    |> Lwt_list.map_p
                         (fun (st_id, st_public_key) ->
                           let%lwt st_token, st_private_key, st_public_key =
                             if st_id = "server" then (
                               let%lwt private_key = KG.generate () in
                               let%lwt public_key = KG.prove private_key in
                               let public_key = string_of_trustee_public_key G.write public_key in
                               return ("", Some private_key, public_key)
                             ) else (
                               let%lwt st_token = generate_token () in
                               return (st_token, None, st_public_key)
                             )
                           in
                           return {st_id; st_token; st_public_key; st_private_key})
                  in
                  let () =
                    (* check that imported keys are valid *)
                    if not @@ List.for_all (fun t ->
                                  let pk = t.st_public_key in
                                  let pk = trustee_public_key_of_string G.read pk in
                                  KG.check pk) trustees then
                      raise (TrusteeImportError "Imported keys are invalid for this election!")
                  in
                  se.se_public_keys <- se.se_public_keys @ trustees;
                  redir_preapply election_draft_trustees uuid ()
               | _, _, _ ->
                  Lwt.fail (TrusteeImportError "Could not retrieve trustees from selected election!")
          with
          | TrusteeImportError msg ->
             T.generic_page ~title:"Error"
               ~service:(preapply election_draft_trustees uuid)
               msg () >>= Html.send
        )
    )

let () =
  Any.register ~service:election_home
    (fun (uuid, ()) () ->
      try%lwt
        let%lwt w = find_election uuid in
        let%lwt () = Eliom_reference.unset Web_state.ballot in
        let cont = redir_preapply election_home (uuid, ()) in
        let%lwt () = Eliom_reference.set Web_state.cont [cont] in
        match%lwt Eliom_reference.get Web_state.cast_confirmed with
        | Some result ->
           let%lwt () = Eliom_reference.unset Web_state.cast_confirmed in
           let%lwt () = Eliom_reference.unset Web_state.user in
           T.cast_confirmed w ~result () >>= Html.send
        | None ->
           let%lwt state = Web_persist.get_election_state uuid in
           T.election_home w state () >>= Html.send
      with Not_found ->
        let%lwt lang = Eliom_reference.get Web_state.language in
        let module L = (val Web_i18n.get_lang lang) in
        T.generic_page ~title:L.not_yet_open
          ~service:(preapply election_home (uuid, ()))
          L.come_back_later ()
          >>= Html.send)

let () =
  Any.register ~service:set_cookie_disclaimer
    (fun () () ->
      let%lwt () = Eliom_reference.set Web_state.show_cookie_disclaimer false in
      let%lwt cont = Web_state.cont_pop () in
      match cont with
      | Some f -> f ()
      | None ->
         let%lwt lang = Eliom_reference.get Web_state.language in
         let module L = (val Web_i18n.get_lang lang) in
         T.generic_page ~title:L.cookies_are_blocked
           L.please_enable_them ()
           >>= Html.send)

let () =
  Any.register ~service:election_admin
    (fun uuid () ->
     let%lwt w = find_election uuid in
     let%lwt metadata = Web_persist.get_election_metadata uuid in
     let%lwt site_user = Web_state.get_site_user () in
     match site_user with
     | Some u when metadata.e_owner = Some u ->
        let%lwt state = Web_persist.get_election_state uuid in
        let get_tokens_decrypt () =
          match%lwt Web_persist.get_decryption_tokens uuid with
          | Some x -> return x
          | None ->
            match metadata.e_trustees with
            | None -> failwith "missing trustees in get_tokens_decrypt"
            | Some ts ->
               let%lwt ts = Lwt_list.map_s (fun _ -> generate_token ()) ts in
               let%lwt () = Web_persist.set_decryption_tokens uuid ts in
               return ts
        in
        T.election_admin w metadata state get_tokens_decrypt () >>= Html.send
     | _ ->
        let cont = redir_preapply election_admin uuid in
        let%lwt () = Eliom_reference.set Web_state.cont [cont] in
        redir_preapply site_login None ()
    )

let election_set_state state uuid () =
  with_site_user (fun u ->
      let%lwt metadata = Web_persist.get_election_metadata uuid in
      if metadata.e_owner = Some u then (
        let%lwt () =
          match%lwt Web_persist.get_election_state uuid with
          | `Open | `Closed -> return ()
          | _ -> forbidden ()
        in
        let state = if state then `Open else `Closed in
        let%lwt () = Web_persist.set_election_state uuid state in
        redir_preapply election_admin uuid ()
      ) else forbidden ()
    )

let () = Any.register ~service:election_open (election_set_state true)
let () = Any.register ~service:election_close (election_set_state false)

let () =
  Any.register ~service:election_archive
    (fun uuid () ->
      with_site_user (fun u ->
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          if metadata.e_owner = Some u then (
            let%lwt () = archive_election uuid in
            redir_preapply election_admin uuid ()
          ) else forbidden ()
        )
    )

let () =
  Any.register ~service:election_delete
  (fun uuid () ->
    with_site_user (fun u ->
        let%lwt metadata = Web_persist.get_election_metadata uuid in
        if metadata.e_owner = Some u then (
          let%lwt () = delete_election uuid in
          redir_preapply admin () ()
        ) else forbidden ()
      )
  )

let () =
  Any.register ~service:election_update_credential
    (fun uuid () ->
      with_site_user (fun u ->
          let%lwt w = find_election uuid in
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          if metadata.e_owner = Some u then (
            T.update_credential w () >>= Html.send
          ) else forbidden ()
        )
    )

let () =
  Any.register ~service:election_update_credential_post
    (fun uuid (old, new_) ->
      with_site_user (fun u ->
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          if metadata.e_owner = Some u then (
            try%lwt
                  let%lwt () = Web_persist.replace_credential uuid old new_ in
                  String.send ("OK", "text/plain")
            with Error e ->
               let%lwt lang = Eliom_reference.get Web_state.language in
               let l = Web_i18n.get_lang lang in
               String.send ("Error: " ^ explain_error l e, "text/plain")
          ) else forbidden ()
        )
    )

let () =
  Any.register ~service:election_vote
    (fun () () ->
      let%lwt () = Eliom_reference.unset Web_state.ballot in
      Web_templates.booth () >>= Html.send)

let () =
  Any.register ~service:election_cast
    (fun uuid () ->
      let%lwt w = find_election uuid in
      let cont = redir_preapply election_cast uuid in
      let%lwt () = Eliom_reference.set Web_state.cont [cont] in
      match%lwt Eliom_reference.get Web_state.ballot with
      | Some b -> T.cast_confirmation w (sha256_b64 b) () >>= Html.send
      | None -> T.cast_raw w () >>= Html.send)

let () =
  Any.register ~service:election_cast_post
    (fun uuid (ballot_raw, ballot_file) ->
      let%lwt user = Web_state.get_election_user uuid in
      let%lwt the_ballot = match ballot_raw, ballot_file with
        | Some ballot, None -> return ballot
        | None, Some fi ->
           let fname = fi.Ocsigen_extensions.tmp_filename in
           Lwt_stream.to_string (Lwt_io.chars_of_file fname)
        | _, _ -> fail_http 400
      in
      let the_ballot = PString.trim the_ballot in
      let cont = redir_preapply election_cast uuid in
      let%lwt () = Eliom_reference.set Web_state.cont [cont] in
      let%lwt () = Eliom_reference.set Web_state.ballot (Some the_ballot) in
      match user with
      | None -> redir_preapply election_login ((uuid, ()), None) ()
      | Some _ -> cont ())

let () =
  Any.register ~service:election_cast_confirm
    (fun uuid () ->
      let%lwt election = find_election uuid in
      let module W = (val Election.get_group election) in
      let module E = Election.Make (W) (LwtRandom) in
      let module B = Web_election.Make (E) in
      match%lwt Eliom_reference.get Web_state.ballot with
      | Some the_ballot ->
         begin
           let%lwt () = Eliom_reference.unset Web_state.ballot in
           match%lwt Web_state.get_election_user uuid with
           | Some u ->
              let record = u, now () in
              let%lwt result =
                try%lwt
                  let%lwt hash = B.cast the_ballot record in
                  return (`Valid hash)
                with Error e -> return (`Error e)
              in
              let%lwt () = Eliom_reference.set Web_state.cast_confirmed (Some result) in
              redir_preapply election_home (uuid, ()) ()
           | None -> forbidden ()
         end
      | None -> fail_http 404)

let () =
  Any.register ~service:election_pretty_ballots
    (fun (uuid, ()) () ->
      let%lwt w = find_election uuid in
      let%lwt ballots = Web_persist.get_ballot_hashes uuid in
      let%lwt result = Web_persist.get_election_result uuid in
      T.pretty_ballots w ballots result () >>= Html.send)

let () =
  Any.register ~service:election_pretty_ballot
    (fun ((uuid, ()), hash) () ->
      let%lwt ballot = Web_persist.get_ballot_by_hash uuid hash in
      match ballot with
      | None -> fail_http 404
      | Some b ->
         String.send (b, "application/json") >>=
           (fun x -> return @@ cast_unknown_content_kind x))

let () =
  let rex = Pcre.regexp "\".*\" \".*:(.*)\"" in
  Any.register ~service:election_missing_voters
    (fun (uuid, ()) () ->
      with_site_user (fun u ->
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          if metadata.e_owner = Some u then (
            let%lwt voters =
              match%lwt read_file ~uuid (string_of_election_file ESVoters) with
              | Some vs ->
                 return (
                     List.fold_left (fun accu v ->
                         let _, login = split_identity v in
                         SSet.add login accu
                       ) SSet.empty vs
                   )
              | None -> return SSet.empty
            in
            let%lwt voters =
              match%lwt read_file ~uuid (string_of_election_file ESRecords) with
              | Some rs ->
                 return (
                     List.fold_left (fun accu r ->
                         let s = Pcre.exec ~rex r in
                         let v = Pcre.get_substring s 1 in
                         SSet.remove v accu
                       ) voters rs
                   )
              | None -> return voters
            in
            let buf = Buffer.create 128 in
            SSet.iter (fun v ->
                Buffer.add_string buf v;
                Buffer.add_char buf '\n'
              ) voters;
            String.send (Buffer.contents buf, "text/plain")
          ) else forbidden ()
        )
    )

let () =
  let rex = Pcre.regexp "\"(.*)\\..*\" \".*:(.*)\"" in
  Any.register ~service:election_pretty_records
    (fun (uuid, ()) () ->
      with_site_user (fun u ->
          let%lwt w = find_election uuid in
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          if metadata.e_owner = Some u then (
            let%lwt records =
              match%lwt read_file ~uuid (string_of_election_file ESRecords) with
              | Some rs ->
                 return (
                     List.rev_map (fun r ->
                         let s = Pcre.exec ~rex r in
                         let date = Pcre.get_substring s 1 in
                         let voter = Pcre.get_substring s 2 in
                         (date, voter)
                       ) rs
                   )
              | None -> return []
            in
            T.pretty_records w (List.rev records) () >>= Html.send
          ) else forbidden ()
        )
    )

let copy_file src dst =
  let open Lwt_io in
  chars_of_file src |> chars_to_file dst

let try_copy_file src dst =
  if%lwt file_exists src then copy_file src dst else return_unit

let make_archive uuid =
  let uuid_s = raw_string_of_uuid uuid in
  let%lwt temp_dir =
    Lwt_preemptive.detach (fun () ->
        let temp_dir = Filename.temp_file "belenios" "archive" in
        Sys.remove temp_dir;
        Unix.mkdir temp_dir 0o700;
        Unix.mkdir (temp_dir / "public") 0o755;
        Unix.mkdir (temp_dir / "restricted") 0o700;
        temp_dir
      ) ()
  in
  let%lwt () =
    Lwt_list.iter_p (fun x ->
        try_copy_file (!spool_dir / uuid_s / x) (temp_dir / "public" / x)
      ) [
        "election.json";
        "public_keys.jsons";
        "threshold.json";
        "public_creds.txt";
        "ballots.jsons";
        "result.json";
      ]
  in
  let%lwt () =
    Lwt_list.iter_p (fun x ->
        try_copy_file (!spool_dir / uuid_s / x) (temp_dir / "restricted" / x)
      ) [
        "voters.txt";
        "records";
      ]
  in
  let command =
    Printf.ksprintf Lwt_process.shell
      "cd \"%s\" && zip -r archive public restricted" temp_dir
  in
  let%lwt r = Lwt_process.exec command in
  match r with
  | Unix.WEXITED 0 ->
     let fname = !spool_dir / uuid_s / "archive.zip" in
     let fname_new = fname ^ ".new" in
     let%lwt () = copy_file (temp_dir / "archive.zip") fname_new in
     let%lwt () = Lwt_unix.rename fname_new fname in
     rmdir temp_dir
  | _ ->
     Printf.ksprintf Ocsigen_messages.errlog
       "Error while creating archive.zip for election %s, temporary directory left in %s"
       uuid_s temp_dir;
     return_unit

let () =
  Any.register ~service:election_download_archive
    (fun (uuid, ()) () ->
      with_site_user (fun u ->
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          let%lwt state = Web_persist.get_election_state uuid in
          if metadata.e_owner = Some u then (
            if state = `Archived then (
              let uuid_s = raw_string_of_uuid uuid in
              let archive_name = !spool_dir / uuid_s / "archive.zip" in
              let%lwt b = file_exists archive_name in
              let%lwt () = if not b then make_archive uuid else return_unit in
              File.send ~content_type:"application/zip" archive_name
            ) else (
              let service = preapply election_admin uuid in
              T.generic_page ~title:"Error" ~service
                "The election is not archived!" () >>= Html.send
            )
          ) else forbidden ()
        )
    )

let find_trustee_id uuid token =
  match%lwt Web_persist.get_decryption_tokens uuid with
  | None -> return (try int_of_string token with _ -> 0)
  | Some tokens ->
    let rec find i = function
      | [] -> raise Not_found
      | t :: ts -> if t = token then i else find (i+1) ts
    in
    return (find 1 tokens)

let () =
  Any.register ~service:election_tally_trustees
    (fun (uuid, token) () ->
      let%lwt w = find_election uuid in
      let%lwt () =
        match%lwt Web_persist.get_election_state uuid with
        | `EncryptedTally _ -> return ()
        | _ -> fail_http 404
      in
      let%lwt trustee_id = find_trustee_id uuid token in
      let%lwt pds = Web_persist.get_partial_decryptions uuid in
      if List.mem_assoc trustee_id pds then (
        T.generic_page ~title:"Error"
          "Your partial decryption has already been received and checked!"
          () >>= Html.send
      ) else (
        T.tally_trustees w trustee_id token () >>= Html.send
      ))

let () =
  Any.register ~service:election_tally_trustees_post
    (fun (uuid, token) partial_decryption ->
      let%lwt () =
        match%lwt Web_persist.get_election_state uuid with
        | `EncryptedTally _ -> return ()
        | _ -> forbidden ()
      in
      let%lwt trustee_id = find_trustee_id uuid token in
      let%lwt pds = Web_persist.get_partial_decryptions uuid in
      let%lwt () =
        if List.mem_assoc trustee_id pds then forbidden () else return ()
      in
      let%lwt () =
        if trustee_id > 0 then return () else fail_http 404
      in
      let%lwt election = find_election uuid in
      let module W = (val Election.get_group election) in
      let module E = Election.Make (W) (LwtRandom) in
      let%lwt pks =
        match%lwt Web_persist.get_threshold uuid with
        | Some tp ->
           let tp = threshold_parameters_of_string W.G.read tp in
           return tp.t_verification_keys
        | None ->
           match%lwt Web_persist.get_public_keys uuid with
           | None -> failwith "no public keys in election_tally_trustees_post"
           | Some pks ->
              let pks = Array.of_list pks in
              let pks = Array.map (trustee_public_key_of_string W.G.read) pks in
              return pks
      in
      let pk = pks.(trustee_id-1).trustee_public_key in
      let pd = partial_decryption_of_string W.G.read partial_decryption in
      let et = !spool_dir / raw_string_of_uuid uuid / string_of_election_file ESETally in
      let%lwt et = Lwt_io.chars_of_file et |> Lwt_stream.to_string in
      let et = encrypted_tally_of_string W.G.read et in
      if E.check_factor et pk pd then (
        let pds = (trustee_id, partial_decryption) :: pds in
        let%lwt () = Web_persist.set_partial_decryptions uuid pds in
        T.generic_page ~title:"Success"
          "Your partial decryption has been received and checked!" () >>=
        Html.send
      ) else (
        let service = preapply election_tally_trustees (uuid, token) in
        T.generic_page ~title:"Error" ~service
          "The partial decryption didn't pass validation!" () >>=
        Html.send
      ))

let handle_election_tally_release uuid () =
  with_site_user (fun u ->
      let uuid_s = raw_string_of_uuid uuid in
      let%lwt election = find_election uuid in
      let%lwt metadata = Web_persist.get_election_metadata uuid in
      let module W = (val Election.get_group election) in
      let module E = Election.Make (W) (LwtRandom) in
      if metadata.e_owner = Some u then (
        let%lwt npks, ntallied =
          match%lwt Web_persist.get_election_state uuid with
          | `EncryptedTally (npks, ntallied, _) -> return (npks, ntallied)
          | _ -> forbidden ()
        in
        let%lwt et =
          !spool_dir / uuid_s / string_of_election_file ESETally |>
            Lwt_io.chars_of_file |> Lwt_stream.to_string >>=
            wrap1 (encrypted_tally_of_string W.G.read)
        in
        let%lwt tp = Web_persist.get_threshold uuid in
        let tp =
          match tp with
          | None -> None
          | Some tp -> Some (threshold_parameters_of_string W.G.read tp)
        in
        let threshold =
          match tp with
          | None -> npks
          | Some tp -> tp.t_threshold
        in
        let%lwt pds = Web_persist.get_partial_decryptions uuid in
        let pds = List.map snd pds in
        let pds = List.map (partial_decryption_of_string W.G.read) pds in
        let%lwt () =
          if List.length pds < threshold then fail_http 404 else return_unit
        in
        let checker = E.check_factor et in
        let%lwt combinator =
          match tp with
          | None ->
             let module K = Trustees.MakeSimple (W.G) (LwtRandom) in
             let%lwt pks =
               match%lwt Web_persist.get_public_keys uuid with
               | Some l -> return (Array.of_list l)
               | _ -> fail_http 404
             in
             let pks =
               Array.map (fun pk ->
                   (trustee_public_key_of_string W.G.read pk).trustee_public_key
                 ) pks
             in
             return (K.combine_factors checker pks)
          | Some tp ->
             let module P = Trustees.MakePKI (W.G) (LwtRandom) in
             let module C = Trustees.MakeChannels (W.G) (LwtRandom) (P) in
             let module K = Trustees.MakePedersen (W.G) (LwtRandom) (P) (C) in
             return (K.combine_factors checker tp)
        in
        let result = E.compute_result ntallied et pds combinator in
        let%lwt () =
          let result = string_of_result W.G.write result in
          write_file ~uuid (string_of_election_file ESResult) [result]
        in
        let%lwt () = Web_persist.set_election_state uuid `Tallied in
        let%lwt () = Web_persist.set_election_date `Tally uuid (now ()) in
        let%lwt () = cleanup_file (!spool_dir / uuid_s / "decryption_tokens.json") in
        redir_preapply election_home (uuid, ()) ()
      ) else forbidden ()
    )

let () =
  Any.register ~service:election_tally_release
    handle_election_tally_release

let content_type_of_file = function
  | ESRaw -> "application/json; charset=utf-8"
  | ESTParams | ESETally | ESResult -> "application/json"
  | ESKeys | ESBallots -> "text/plain" (* should be "application/json-seq", but we don't use RS *)
  | ESCreds | ESRecords | ESVoters -> "text/plain"

let handle_pseudo_file uuid f site_user =
  let confidential =
    match f with
    | ESRaw | ESKeys | ESTParams | ESBallots | ESETally | ESResult | ESCreds -> false
    | ESRecords | ESVoters -> true
  in
  let%lwt () =
    if confidential then (
      let%lwt metadata = Web_persist.get_election_metadata uuid in
      match site_user with
      | Some u when metadata.e_owner = Some u -> return ()
      | _ -> forbidden ()
    ) else return ()
  in
  let content_type = content_type_of_file f in
  File.send ~content_type (!spool_dir / raw_string_of_uuid uuid / string_of_election_file f)

let () =
  Any.register ~service:election_dir
    (fun (uuid, f) () ->
     let%lwt site_user = Web_state.get_site_user () in
     handle_pseudo_file uuid f site_user)

let () =
  Any.register ~service:election_compute_encrypted_tally
    (fun uuid () ->
      with_site_user (fun u ->
          let%lwt election = find_election uuid in
          let%lwt metadata = Web_persist.get_election_metadata uuid in
          let module W = (val Election.get_group election) in
          let module E = Election.Make (W) (LwtRandom) in
          if metadata.e_owner = Some u then (
            let%lwt () =
              match%lwt Web_persist.get_election_state uuid with
              | `Closed -> return ()
              | _ -> forbidden ()
            in
            let%lwt nb, hash, tally = Web_persist.compute_encrypted_tally uuid in
            let%lwt npks =
              match%lwt Web_persist.get_threshold uuid with
              | Some tp ->
                 let tp = threshold_parameters_of_string W.G.read tp in
                 return (Array.length tp.t_verification_keys)
              | None ->
                 match%lwt Web_persist.get_public_keys uuid with
                 | Some pks -> return (List.length pks)
                 | None -> failwith "missing public keys and threshold parameters"
            in
            let%lwt () = Web_persist.set_election_state uuid (`EncryptedTally (npks, nb, hash)) in
            let tally = encrypted_tally_of_string W.G.read tally in
            let%lwt sk = Web_persist.get_private_key uuid in
            match metadata.e_trustees with
            | None ->
               (* no trustees: compute decryption and release tally *)
               let sk = match sk with
                 | Some x -> x
                 | None -> failwith "missing private key"
               in
               let%lwt pd = E.compute_factor tally sk in
               let pd = string_of_partial_decryption W.G.write pd in
               let%lwt () = Web_persist.set_partial_decryptions uuid [1, pd] in
               handle_election_tally_release uuid ()
            | Some ts ->
               let%lwt () =
                 Lwt_list.iteri_s (fun i t ->
                     if t = "server" then (
                       match%lwt Web_persist.get_private_key uuid with
                       | Some k ->
                          let%lwt pd = E.compute_factor tally k in
                          let pd = string_of_partial_decryption W.G.write pd in
                          Web_persist.set_partial_decryptions uuid [i+1, pd]
                       | None -> return_unit (* dead end *)
                     ) else return_unit
                   ) ts
               in
               redir_preapply election_admin uuid ()
          ) else forbidden ()
        )
    )

let () =
  Any.register ~service:set_language
    (fun lang () ->
      let%lwt () = Eliom_reference.set Web_state.language lang in
      let%lwt cont = Web_state.cont_pop () in
      match cont with
      | Some f -> f ()
      | None -> Redirection.send (Redirection home))

let () =
  Any.register ~service:election_draft_threshold_set
    (fun uuid threshold ->
      with_draft_election uuid (fun se ->
          match se.se_threshold_trustees with
          | None ->
             let msg = "Please add some trustees first!" in
             let service = preapply election_draft_threshold_trustees uuid in
             T.generic_page ~title:"Error" ~service msg () >>= Html.send
          | Some xs ->
             let maybe_threshold, step =
               if threshold = 0 then None, None
               else Some threshold, Some 1
             in
             if threshold >= 0 && threshold < List.length xs then (
               List.iter (fun x -> x.stt_step <- step) xs;
               se.se_threshold <- maybe_threshold;
               redir_preapply election_draft_threshold_trustees uuid ()
             ) else (
               let msg = "The threshold must be positive and lesser than the number of trustees!" in
               let service = preapply election_draft_threshold_trustees uuid in
               T.generic_page ~title:"Error" ~service msg () >>= Html.send
             )
        )
    )

let () =
  Any.register ~service:election_draft_threshold_trustee_add
    (fun uuid stt_id ->
      with_draft_election uuid (fun se ->
          if is_email stt_id then (
            let%lwt stt_token = generate_token () in
            let trustee = {
                stt_id; stt_token; stt_step = None;
                stt_cert = None; stt_polynomial = None;
                stt_vinput = None; stt_voutput = None;
              } in
            let trustees =
              match se.se_threshold_trustees with
              | None -> Some [trustee]
              | Some t -> Some (t @ [trustee])
            in
            se.se_threshold_trustees <- trustees;
            redir_preapply election_draft_threshold_trustees uuid ()
          ) else (
            let msg = stt_id ^ " is not a valid e-mail address!" in
            let service = preapply election_draft_threshold_trustees uuid in
            T.generic_page ~title:"Error" ~service msg () >>= Html.send
          )
        )
    )

let () =
  Any.register ~service:election_draft_threshold_trustee_del
    (fun uuid index ->
      with_draft_election uuid (fun se ->
          let trustees =
            let trustees =
              match se.se_threshold_trustees with
              | None -> []
              | Some x -> x
            in
            trustees |>
              List.mapi (fun i x -> i, x) |>
              List.filter (fun (i, _) -> i <> index) |>
              List.map snd
          in
          let trustees = match trustees with [] -> None | x -> Some x in
          se.se_threshold_trustees <- trustees;
          redir_preapply election_draft_threshold_trustees uuid ()
        )
    )

let () =
  Html.register ~service:election_draft_threshold_trustee
    (fun (uuid, token) () ->
      match%lwt Web_persist.get_draft_election uuid with
      | None -> fail_http 404
      | Some se -> T.election_draft_threshold_trustee token uuid se ()
    )

let () =
  Any.register ~service:election_draft_threshold_trustee_post
    (fun (uuid, token) data ->
      wrap_handler
        (fun () ->
          let%lwt () =
            Lwt_mutex.with_lock election_draft_mutex
              (fun () ->
                match%lwt Web_persist.get_draft_election uuid with
                | None -> fail_http 404
                | Some se ->
                   let ts =
                     match se.se_threshold_trustees with
                     | None -> failwith "No threshold trustees"
                     | Some xs -> Array.of_list xs
                   in
                   let i, t =
                     match Array.findi (fun i x ->
                               if token = x.stt_token then Some (i, x) else None
                             ) ts with
                     | Some (i, t) -> i, t
                     | None -> failwith "Trustee not found"
                   in
                   let get_certs () =
                     let certs = Array.map (fun x ->
                                     match x.stt_cert with
                                     | None -> failwith "Missing certificate"
                                     | Some y -> y
                                   ) ts in
                     {certs}
                   in
                   let get_polynomials () =
                     Array.map (fun x ->
                         match x.stt_polynomial with
                         | None -> failwith "Missing polynomial"
                         | Some y -> y
                       ) ts
                   in
                   let module G = (val Group.of_string se.se_group : GROUP) in
                   let module P = Trustees.MakePKI (G) (LwtRandom) in
                   let module C = Trustees.MakeChannels (G) (LwtRandom) (P) in
                   let module K = Trustees.MakePedersen (G) (LwtRandom) (P) (C) in
                   let%lwt () =
                     match t.stt_step with
                     | Some 1 ->
                        let cert = cert_of_string data in
                        if K.step1_check cert then (
                          t.stt_cert <- Some cert;
                          t.stt_step <- Some 2;
                          return_unit
                        ) else (
                          failwith "Invalid certificate"
                        )
                     | Some 3 ->
                        let certs = get_certs () in
                        let polynomial = polynomial_of_string data in
                        if K.step3_check certs i polynomial then (
                          t.stt_polynomial <- Some polynomial;
                          t.stt_step <- Some 4;
                          return_unit
                        ) else (
                          failwith "Invalid polynomial"
                        )
                     | Some 5 ->
                        let certs = get_certs () in
                        let polynomials = get_polynomials () in
                        let voutput = voutput_of_string G.read data in
                        if K.step5_check certs i polynomials voutput then (
                          t.stt_voutput <- Some data;
                          t.stt_step <- Some 6;
                          return_unit
                        ) else (
                          failwith "Invalid voutput"
                        )
                     | _ -> failwith "Unknown step"
                   in
                   let%lwt () =
                     if Array.forall (fun x -> x.stt_step = Some 2) ts then (
                       (try
                          K.step2 (get_certs ());
                          Array.iter (fun x -> x.stt_step <- Some 3) ts;
                        with e ->
                          se.se_threshold_error <- Some (Printexc.to_string e)
                       ); return_unit
                     ) else return_unit
                   in
                   let%lwt () =
                     if Array.forall (fun x -> x.stt_step = Some 4) ts then (
                       (try
                          let certs = get_certs () in
                          let polynomials = get_polynomials () in
                          let vinputs = K.step4 certs polynomials in
                          for j = 0 to Array.length ts - 1 do
                            ts.(j).stt_vinput <- Some vinputs.(j)
                          done;
                          Array.iter (fun x -> x.stt_step <- Some 5) ts
                        with e ->
                          se.se_threshold_error <- Some (Printexc.to_string e)
                       ); return_unit
                     ) else return_unit
                   in
                   let%lwt () =
                     if Array.forall (fun x -> x.stt_step = Some 6) ts then (
                       (try
                          let certs = get_certs () in
                          let polynomials = get_polynomials () in
                          let voutputs = Array.map (fun x ->
                                             match x.stt_voutput with
                                             | None -> failwith "Missing voutput"
                                             | Some y -> voutput_of_string G.read y
                                           ) ts in
                          let p = K.step6 certs polynomials voutputs in
                          se.se_threshold_parameters <- Some (string_of_threshold_parameters G.write p);
                          Array.iter (fun x -> x.stt_step <- Some 7) ts
                        with e ->
                          se.se_threshold_error <- Some (Printexc.to_string e)
                       ); return_unit
                     ) else return_unit
                   in
                   Web_persist.set_draft_election uuid se
              )
          in
          redir_preapply election_draft_threshold_trustee (uuid, token) ()
        )
    )

let extract_automatic_data_draft uuid_s =
  let uuid = uuid_of_raw_string uuid_s in
  match%lwt Web_persist.get_draft_election uuid with
  | None -> return_none
  | Some se ->
     let name = se.se_questions.t_name in
     let contact = se.se_metadata.e_contact in
     let t = Option.get se.se_creation_date default_creation_date in
     let next_t = datetime_add t (day days_to_delete) in
     return @@ Some (`Destroy, uuid, next_t, name, contact)

let extract_automatic_data_validated uuid_s =
  let uuid = uuid_of_raw_string uuid_s in
  let%lwt election = Web_persist.get_raw_election uuid in
  match election with
  | None -> return_none
  | Some election ->
     let election = Election.of_string election in
     let%lwt metadata = Web_persist.get_election_metadata uuid in
     let name = election.e_params.e_name in
     let contact = metadata.e_contact in
     let%lwt state = Web_persist.get_election_state uuid in
     match state with
     | `Open | `Closed | `EncryptedTally _ ->
        let%lwt t = Web_persist.get_election_date `Validation uuid in
        let t = Option.get t default_validation_date in
        let next_t = datetime_add t (day days_to_delete) in
        return @@ Some (`Delete, uuid, next_t, name, contact)
     | `Tallied ->
        let%lwt t = Web_persist.get_election_date `Tally uuid in
        let t = Option.get t default_tally_date in
        let next_t = datetime_add t (day days_to_archive) in
        return @@ Some (`Archive, uuid, next_t, name, contact)
     | `Archived ->
        let%lwt t = Web_persist.get_election_date `Archive uuid in
        let t = Option.get t default_archive_date in
        let next_t = datetime_add t (day days_to_delete) in
        return @@ Some (`Delete, uuid, next_t, name, contact)

let try_extract extract x =
  try%lwt extract x with _ -> return_none

let get_next_actions () =
  Lwt_unix.files_of_directory !spool_dir |>
  Lwt_stream.to_list >>=
  Lwt_list.filter_map_p
    (fun x ->
      if x = "." || x = ".." then return_none
      else (
        match%lwt try_extract extract_automatic_data_draft x with
        | None -> try_extract extract_automatic_data_validated x
        | x -> return x
      )
    )

let mail_automatic_warning : ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  "The election %s available at:

  %s

will be automatically %s after %s.

-- \nBelenios"

let process_election_for_data_policy (action, uuid, next_t, name, contact) =
  let uuid_s = raw_string_of_uuid uuid in
  let now = now () in
  let action, comment = match action with
    | `Destroy -> destroy_election, "destroyed"
    | `Delete -> delete_election, "deleted"
    | `Archive -> archive_election, "archived"
  in
  if datetime_compare now next_t > 0 then (
    let%lwt () = action uuid in
    return (
        Printf.ksprintf Ocsigen_messages.warning
          "Election %s has been automatically %s" uuid_s comment
      )
  ) else (
    let mail_t = datetime_add next_t (day (-days_to_mail)) in
    if datetime_compare now mail_t > 0 then (
      let%lwt last_t = Web_persist.get_election_date `LastMail uuid in
      let send = match last_t with
        | None -> true
        | Some t ->
           let next_mail_t = datetime_add t (day days_between_mails) in
           datetime_compare now next_mail_t > 0
      in
      if send then (
        match contact with
        | None -> return_unit
        | Some contact ->
           match extract_email contact with
           | None -> return_unit
           | Some email ->
              let subject =
                Printf.sprintf "Election %s will be automatically %s soon"
                  name comment
              in
              let uri =
                Eliom_uri.make_string_uri ~absolute:true
                  ~service:election_home (uuid, ()) |> rewrite_prefix
              in
              let body =
                Printf.sprintf mail_automatic_warning
                  name uri comment (format_datetime next_t)
              in
              let%lwt () = send_email email subject body in
              Web_persist.set_election_date `LastMail uuid now
      ) else return_unit
    ) else return_unit
  )

let _ =
  let open Ocsigen_messages in
  let rec loop () =
    let () = console (fun () -> "Data policy process started") in
    let%lwt elections = get_next_actions () in
    let%lwt () = Lwt_list.iter_p process_election_for_data_policy elections in
    let () = console (fun () -> "Data policy process completed") in
    let%lwt () = Lwt_unix.sleep 3600. in
    loop ()
  in loop ()
