(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2021 Inria                                           *)
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
open Lwt.Syntax
open Belenios_platform
open Platform
open Belenios_core
open Belenios
open Serializable_builtin_t
open Serializable_j
open Signatures
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common

module Make (X : Pages_sig.S) (Site_common : Site_common_sig.S) (Web_auth : Web_auth_sig.S) = struct

  open X
  open Web_services
  open Site_common

  let ( / ) = Filename.concat

  module PString = String

  open Eliom_service
  open Eliom_registration

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "admin"

  let dump_passwords uuid db =
    List.map (fun line -> PString.concat "," line) db |>
      write_file ~uuid "passwords.csv"

  let validate_election uuid se =
    let version = Option.get se.se_version 0 in
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
    let group = Group.of_string ~version se.se_group in
    let module G = (val group : GROUP) in
    let module Trustees = (val Trustees.get_by_version version) in
    let module K = Trustees.MakeCombinator (G) in
    let module KG = Trustees.MakeSimple (G) (LwtRandom) in
    let* trustee_names, trustees, private_keys =
      match se.se_threshold_trustees with
      | None ->
         let* trustee_names, trustees, private_key =
           match se.se_public_keys with
           | [] ->
              let* private_key = KG.generate () in
              let* public_key = KG.prove private_key in
              let public_key = { public_key with trustee_name = Some "server" } in
              return (["server"], [`Single public_key], `KEY private_key)
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
                  (List.map (fun {st_id; _} -> st_id) se.se_public_keys),
                  (List.map
                     (fun {st_public_key; st_name; _} ->
                       if st_public_key = "" then failwith "some public keys are missing";
                       let pk = trustee_public_key_of_string G.read st_public_key in
                       let pk = { pk with trustee_name = st_name } in
                       `Single pk
                     ) se.se_public_keys),
                  private_key)
         in
         return (trustee_names, trustees, private_key)
      | Some ts ->
         match se.se_threshold_parameters with
         | None -> failwith "key establishment not finished"
         | Some tp ->
            let tp = threshold_parameters_of_string G.read tp in
            let named =
              List.combine (Array.to_list tp.t_verification_keys) ts
              |> List.map (fun (k, t) -> { k with trustee_name = t.stt_name })
              |> Array.of_list
            in
            let tp = { tp with t_verification_keys = named } in
            let trustee_names = List.map (fun {stt_id; _} -> stt_id) ts in
            let private_keys =
              List.map (fun {stt_voutput; _} ->
                  match stt_voutput with
                  | Some v ->
                     let voutput = voutput_of_string G.read v in
                     voutput.vo_private_key
                  | None -> failwith "inconsistent state"
                ) ts
            in
            let* server_private_key = KG.generate () in
            let* server_public_key = KG.prove server_private_key in
            let server_public_key = { server_public_key with trustee_name = Some "server" } in
            return (
                "server" :: trustee_names,
                [`Single server_public_key; `Pedersen tp],
                `KEYS (server_private_key, private_keys)
              )
    in
    let y = K.combine_keys trustees in
    (* election parameters *)
    let e_server_is_trustee = match private_keys with
      | `KEY _ | `KEYS _ -> Some true
      | `None -> None
    in
    let metadata = {
        se.se_metadata with
        e_trustees = Some trustee_names;
        e_server_is_trustee;
      } in
    let template = se.se_questions in
    let params = {
        e_version = Option.get se.se_version 0;
        e_description = template.t_description;
        e_name = template.t_name;
        e_questions = template.t_questions;
        e_uuid = uuid;
        e_administrator = se.se_administrator;
        e_credential_authority = metadata.e_cred_authority;
      } in
    let raw_election =
      let public_key = G.to_string y in
      Election.make_raw_election params ~group:se.se_group ~public_key
    in
    (* write election files to disk *)
    let dir = !Web_config.spool_dir / uuid_s in
    let create_file fname what xs =
      Lwt_io.with_file
        ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
        ~perm:0o600 ~mode:Lwt_io.Output (dir / fname)
        (fun oc ->
          Lwt_list.iter_s
            (fun v ->
              let* () = Lwt_io.write oc (what v) in
              Lwt_io.write oc "\n") xs)
    in
    let* () = create_file "trustees.json" (string_of_trustees G.write) [trustees] in
    let* () = create_file "voters.txt" (fun x -> x.sv_id) se.se_voters in
    let* () = create_file "metadata.json" string_of_metadata [metadata] in
    let* () = create_file "election.json" (fun x -> x) [raw_election] in
    let* () = create_file "ballots.jsons" (fun x -> x) [] in
    (* initialize credentials *)
    let* () =
      let fname = !Web_config.spool_dir / uuid_s / "public_creds.txt" in
      let* file = read_file fname in
      match file with
      | Some xs -> Web_persist.init_credential_mapping uuid xs
      | None -> return_unit
    in
    (* create file with private keys, if any *)
    let* () =
      match private_keys with
      | `None -> return_unit
      | `KEY x -> create_file "private_key.json" string_of_number [x]
      | `KEYS (x, y) ->
         let* () = create_file "private_key.json" string_of_number [x] in
         create_file "private_keys.jsons" (fun x -> x) y
    in
    (* clean up draft *)
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "draft.json") in
    (* clean up private credentials, if any *)
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "private_creds.txt") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "private_creds.downloaded") in
    (* write passwords *)
    let* () =
      match metadata.e_auth_config with
      | Some [{auth_system = "password"; _}] ->
         let db =
           List.filter_map (fun v ->
               let _, login, _ = split_identity v.sv_id in
               match v.sv_password with
               | Some (salt, hashed) -> Some [login; salt; hashed]
               | None -> None
             ) se.se_voters
         in
         if db <> [] then dump_passwords uuid db else return_unit
      | _ -> return_unit
    in
    (* finish *)
    let* () = Web_persist.set_election_state uuid `Open in
    let* dates = Web_persist.get_election_dates uuid in
    Web_persist.set_election_dates uuid {dates with e_finalization = Some (now ())}

  let delete_sensitive_data uuid =
    let uuid_s = raw_string_of_uuid uuid in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "state.json") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "decryption_tokens.json") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "partial_decryptions.json") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "extended_records.jsons") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "credential_mappings.jsons") in
    let* () = rmdir (!Web_config.spool_dir / uuid_s / "ballots") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "ballots_index.json") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "private_key.json") in
    let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "private_keys.jsons") in
    return_unit

  let archive_election uuid =
    let* () = delete_sensitive_data uuid in
    let* dates = Web_persist.get_election_dates uuid in
    Web_persist.set_election_dates uuid {dates with e_archive = Some (now ())}

  let delete_election uuid =
    let uuid_s = raw_string_of_uuid uuid in
    let* () = delete_sensitive_data uuid in
    let* election = find_election uuid in
    match election with
    | None -> return_unit
    | Some election ->
       let open (val election) in
       let* metadata = Web_persist.get_election_metadata uuid in
       let de_template = {
           t_description = "";
           t_name = election.e_name;
           t_questions = Array.map Question.erase_question election.e_questions;
           t_administrator = None;
           t_credential_authority = None;
         }
       in
       let de_owner = match metadata.e_owner with
         | None -> Printf.ksprintf failwith "election %s has no owner" uuid_s
         | Some x -> x
       in
       let* dates = Web_persist.get_election_dates uuid in
       let de_date =
         match dates.e_tally with
         | Some x -> x
         | None ->
            match dates.e_finalization with
            | Some x -> x
            | None ->
               match dates.e_creation with
               | Some x -> x
               | None -> default_validation_date
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
       let* de_trustees =
         let* trustees = Web_persist.get_trustees uuid in
         trustees_of_string Yojson.Safe.read_json trustees
         |> List.map
              (function
               | `Single _ -> `Single
               | `Pedersen t -> `Pedersen (t.t_threshold, Array.length t.t_verification_keys)
              )
         |> return
       in
       let* voters = Web_persist.get_voters uuid in
       let* ballots = Web_persist.get_ballot_hashes uuid in
       let* result = Web_persist.get_election_result uuid in
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
           de_trustees;
           de_server_is_trustee = metadata.e_server_is_trustee = Some true;
         }
       in
       let* () = write_file ~uuid "deleted.json" [string_of_deleted_election de] in
       let files_to_delete = [
           "election.json";
           "ballots.jsons";
           "dates.json";
           "encrypted_tally.json";
           "metadata.json";
           "passwords.csv";
           "public_creds.txt";
           "trustees.json";
           "records";
           "result.json";
           "hide_result";
           "shuffle_token";
           "shuffles.jsons";
           "voters.txt";
           "archive.zip";
           "audit_cache.json";
         ]
       in
       let* () = Lwt_list.iter_p (fun x ->
                     cleanup_file (!Web_config.spool_dir / uuid_s / x)
                   ) files_to_delete
       in
       return_unit

  let () = Any.register ~service:home
             (fun () () -> Redirection.send (Redirection admin))

  let get_elections_by_owner_sorted u =
    let* elections = Web_persist.get_elections_by_owner u in
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
    let* user = Eliom_reference.get Web_state.site_user in
    match user with
    | Some u -> f u
    | None -> forbidden ()

  let with_metadata_check_owner uuid f =
    let* user = Eliom_reference.get Web_state.site_user in
    let* metadata = Web_persist.get_election_metadata uuid in
    match user, metadata.e_owner with
    | Some a, Some b when a = b -> f metadata
    | _, _ -> forbidden ()

  let without_site_user ?fallback f =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* user = Eliom_reference.get Web_state.site_user in
    match user with
    | None -> f ()
    | Some u ->
       match fallback with
       | Some g -> g u
       | None ->
          Pages_common.generic_page ~title:(s_ "Error")
            (s_ "This page is not accessible to authenticated administrators, because it is meant to be used by third parties.")
            () >>= Html.send

  let () =
    Redirection.register ~service:privacy_notice_accept
      (fun () cont ->
        let* () = Eliom_reference.set Web_state.show_cookie_disclaimer false in
        let cont = match cont with
          | ContAdmin -> Redirection admin
          | ContSignup service -> Redirection (preapply ~service:signup_captcha service)
        in
        return cont
      )

  let () = Html.register ~service:admin
             (fun () () ->
               let* site_user = Eliom_reference.get Web_state.site_user in
               match site_user with
               | None -> Pages_admin.admin_login Web_auth.get_site_login_handler
               | Some u ->
                  let* show = Eliom_reference.get Web_state.show_cookie_disclaimer in
                  if show then (
                    Pages_admin.privacy_notice ContAdmin
                  ) else (
                    let* elections = get_elections_by_owner_sorted u in
                    Pages_admin.admin ~elections
                  )
             )

  let generate_uuid () =
    let length = !Web_config.uuid_length in
    let* token = generate_token ?length () in
    return (uuid_of_raw_string token)

  let create_new_election owner cred auth =
    let e_cred_authority = match cred with
      | `Automatic -> Some "server"
      | `Manual -> None
    in
    let e_auth_config = match auth with
      | `Password -> Some [{auth_system = "password"; auth_instance = "password"; auth_config = []}]
      | `Dummy -> Some [{auth_system = "dummy"; auth_instance = "dummy"; auth_config = []}]
      | `CAS server -> Some [{auth_system = "cas"; auth_instance = "cas"; auth_config = ["server", server]}]
      | `Import name -> Some [{auth_system = "import"; auth_instance = name; auth_config = []}]
    in
    let* uuid = generate_uuid () in
    let* token = generate_token () in
    let se_metadata = {
        e_owner = Some owner;
        e_auth_config;
        e_cred_authority;
        e_trustees = None;
        e_languages = Some ["en"; "fr"];
        e_contact = None;
        e_server_is_trustee = None;
        e_booth_version = None;
      } in
    let se_questions = {
        t_description = default_description;
        t_name = default_name;
        t_questions = default_questions;
        t_administrator = None;
        t_credential_authority = None;
      } in
    let se = {
        se_version = Some default_version;
        se_owner = owner;
        se_group = !Web_config.default_group;
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
        se_administrator = None;
      } in
    let* () = Lwt_unix.mkdir (!Web_config.spool_dir / raw_string_of_uuid uuid) 0o700 in
    let* () = Web_persist.set_draft_election uuid se in
    redir_preapply election_draft uuid ()

  let () = Html.register ~service:election_draft_pre
             (fun () () -> Pages_admin.election_draft_pre ())

  let http_rex = "^https?://[a-z0-9/.-]+$"

  let is_http_url =
    let rex = Pcre.regexp ~flags:[`CASELESS] http_rex in
    fun x ->
    match pcre_exec_opt ~rex x with
    | Some _ -> true
    | None -> false

  let () = Any.register ~service:election_draft_new
             (fun () (credmgmt, (auth, cas_server)) ->
               let* l = get_preferred_gettext () in
               let open (val l) in
               with_site_user (fun u ->
                   let* credmgmt = match credmgmt with
                     | Some "auto" -> return `Automatic
                     | Some "manual" -> return `Manual
                     | _ -> fail_http `Bad_request
                   in
                   let* auth = match auth with
                     | Some "password" -> return `Password
                     | Some "dummy" -> return `Dummy
                     | Some "cas" ->
                        (match cas_server with
                         | None -> fail_http `Bad_request
                         | Some cas_server -> return @@ `CAS (PString.trim cas_server)
                        )
                     | Some x ->
                        let n = PString.length x in
                        if n > 1 && PString.get x 0 = '%' then (
                          let name = PString.sub x 1 (n - 1) in
                          return @@ `Import name
                        ) else fail_http `Bad_request
                     | _ -> fail_http `Bad_request
                   in
                   match auth with
                   | `CAS cas_server when not (is_http_url cas_server) ->
                      Pages_common.generic_page ~title:(s_ "Error") (s_ "Bad CAS server!") () >>= Html.send
                   | _ -> create_new_election u credmgmt auth
                 )
             )

  let with_draft_election_ro uuid f =
    with_site_user (fun u ->
        let* election = Web_persist.get_draft_election uuid in
        match election with
        | None -> fail_http `Not_found
        | Some se -> if se.se_owner = u then f se else forbidden ()
      )

  let () =
    Any.register ~service:election_draft
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft uuid se ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_trustees
      (fun uuid () ->
        with_draft_election_ro uuid (fun se ->
            match se.se_threshold_trustees with
            | None -> Pages_admin.election_draft_trustees uuid se () >>= Html.send
            | Some _ -> redir_preapply election_draft_threshold_trustees uuid ()
          )
      )

  let () =
    Any.register ~service:election_draft_threshold_trustees
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_threshold_trustees uuid se ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_credential_authority
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_credential_authority uuid se ()
        >>= Html.send
      )

  let with_draft_election ?(save = true) uuid f =
    with_site_user (fun u ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        Web_election_mutex.with_lock uuid (fun () ->
            let* election = Web_persist.get_draft_election uuid in
            match election with
            | None -> fail_http `Not_found
            | Some se ->
               if se.se_owner = u then (
                 Lwt.catch
                   (fun () ->
                     let* r = f se in
                     let* () = if save then Web_persist.set_draft_election uuid se else return_unit in
                     return r
                   )
                   (fun e ->
                     let msg = match e with Failure s -> s | _ -> Printexc.to_string e in
                     let service = preapply ~service:election_draft uuid in
                     Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
                   )
               ) else forbidden ()
          )
      )

  let () =
    Any.register ~service:election_draft_set_credential_authority
      (fun uuid name ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let service = Eliom_service.preapply ~service:election_draft_credential_authority uuid in
            match (
              if se.se_metadata.e_cred_authority = Some "server" then
                Error (s_ "You cannot set the credential authority for this election!")
              else
                match name with
                | "" -> Ok None
                | "server" -> Error (s_ "Invalid public name for credential authority!")
                | x -> Ok (Some x)
            ) with
            | Ok e_cred_authority ->
               se.se_metadata <- {se.se_metadata with e_cred_authority};
               let msg = s_ "The public name of the credential authority has been set successfully!" in
               Pages_common.generic_page ~title:(s_ "Success") ~service msg () >>= Html.send
            | Error msg ->
               Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
          )
      )

  let () =
    Any.register ~service:election_draft_languages
      (fun uuid languages ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let langs = languages_of_string languages in
            match langs with
            | [] ->
               let service = preapply ~service:election_draft uuid in
               Pages_common.generic_page ~title:(s_ "Error") ~service
                 (s_ "You must select at least one language!") () >>= Html.send
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
                  let service = preapply ~service:election_draft uuid in
                  Pages_common.generic_page ~title:(s_ "Error") ~service
                    (Printf.sprintf (f_ "No such language: %s") l) () >>= Html.send
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
    Any.register ~service:election_draft_admin_name
      (fun uuid name ->
        with_draft_election uuid (fun se ->
            let administrator = if name = "" then None else Some name in
            se.se_administrator <- administrator;
            redir_preapply election_draft uuid ()
          )
      )

  let () =
    Any.register ~service:election_draft_description
      (fun uuid (name, description) ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            if PString.length name > max_election_name_size then (
              let msg =
                Printf.sprintf (f_ "The election name must be %d characters or less!")
                  max_election_name_size
              in
              Pages_common.generic_page ~title:(s_ "Error") msg () >>= Html.send
            ) else (
              se.se_questions <- {se.se_questions with
                                   t_name = name;
                                   t_description = description;
                                 };
              redir_preapply election_draft uuid ()
            )
          )
      )

  let handle_password se uuid ~force voters =
    let* l = get_preferred_gettext () in
    let open (val l) in
    if List.length voters > !Web_config.maxmailsatonce then
      Lwt.fail (Failure (Printf.sprintf (f_ "Cannot send passwords, there are too many voters (max is %d)") !Web_config.maxmailsatonce))
    else if se.se_questions.t_name = default_name then
      Lwt.fail (Failure (s_ "The election name has not been edited!"))
    else
      let title = se.se_questions.t_name in
      let url = Eliom_uri.make_string_uri ~absolute:true ~service:election_home
                  (uuid, ()) |> rewrite_prefix
      in
      let langs = get_languages se.se_metadata.e_languages in
      let show_weight =
        List.exists
          (fun id ->
            let _, _, weight = split_identity_opt id.sv_id in
            weight <> None
          ) voters
      in
      let* () =
        Lwt_list.iter_s (fun id ->
            match id.sv_password with
            | Some _ when not force -> return_unit
            | None | Some _ ->
               let* x =
                 Pages_voter.generate_password se.se_metadata langs title uuid
                   url id.sv_id show_weight
               in
               return (id.sv_password <- Some x)
          ) voters
      in
      let service = preapply ~service:election_draft uuid in
      Pages_common.generic_page ~title:(s_ "Success") ~service
        (s_ "Passwords have been generated and mailed!") () >>= Html.send

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
        Pages_admin.regenpwd uuid () >>= Html.send)

  let find_user_id uuid user =
    let uuid_s = raw_string_of_uuid uuid in
    let db = Lwt_io.lines_of_file (!Web_config.spool_dir / uuid_s / "voters.txt") in
    let* db = Lwt_stream.to_list db in
    let rec loop = function
      | [] -> None
      | id :: xs ->
         let _, login, _ = split_identity id in
         if login = user then Some id else loop xs
    in
    let show_weight =
      List.exists
        (fun x ->
          let _, _, weight = split_identity_opt x in
          weight <> None
        ) db
    in
    return (loop db, show_weight)

  let load_password_db uuid =
    let uuid_s = raw_string_of_uuid uuid in
    let db = !Web_config.spool_dir / uuid_s / "passwords.csv" in
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
        let@ metadata = with_metadata_check_owner uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let@ election = with_election uuid in
        let open (val election) in
        let title = election.e_name in
        let url = Eliom_uri.make_string_uri
                    ~absolute:true ~service:election_home
                    (uuid, ()) |> rewrite_prefix
        in
        let service = preapply ~service:election_admin uuid in
        let* x = find_user_id uuid user in
        match x with
        | Some id, show_weight ->
           let langs = get_languages metadata.e_languages in
           let* db = load_password_db uuid in
           let* x =
             Pages_voter.generate_password metadata langs title uuid
               url id show_weight
           in
           let db = replace_password user x db in
           let* () = dump_passwords uuid db in
           Pages_common.generic_page ~title:(s_ "Success") ~service
             (Printf.sprintf (f_ "A new password has been mailed to %s.") id) ()
           >>= Html.send
        | None, _ ->
           Pages_common.generic_page ~title:(s_ "Error") ~service
             (Printf.sprintf (f_ "%s is not a registered user for this election.") user) ()
           >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_questions
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_questions uuid se ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_questions_post
      (fun uuid (template, booth_version) ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let template = template_of_string template in
            let fixed_group = is_group_fixed se in
            (match get_suitable_group_kind se.se_questions, get_suitable_group_kind template with
             | `NH, `NH | `H, `H -> ()
             | `NH, `H when fixed_group -> ()
             | `NH, `H -> se.se_group <- !Web_config.default_group
             | `H, `NH when fixed_group -> failwith (s_ "This kind of change is not allowed now!")
             | `H, `NH -> se.se_group <- !Web_config.nh_group
            );
            se.se_questions <- template;
            let e_booth_version = match booth_version with 1 -> None | x -> Some x in
            se.se_metadata <- { se.se_metadata with e_booth_version };
            redir_preapply election_draft uuid ()
          )
      )

  let () =
    Any.register ~service:election_draft_preview
      (fun (uuid, ()) () ->
        with_draft_election_ro uuid (fun se ->
            let version = Option.get se.se_version 0 in
            let group = se.se_group in
            let module G = (val Group.of_string ~version group : GROUP) in
            let params = {
                e_version = Option.get se.se_version 0;
                e_description = se.se_questions.t_description;
                e_name = se.se_questions.t_name;
                e_questions = se.se_questions.t_questions;
                e_uuid = uuid;
                e_administrator = se.se_administrator;
                e_credential_authority = se.se_metadata.e_cred_authority;
              }
            in
            let public_key = G.to_string G.g in
            let raw_election = Election.make_raw_election params ~group ~public_key in
            let* x = String.send (raw_election, "application/json") in
            return @@ Eliom_registration.cast_unknown_content_kind x
          )
      )

  let () =
    Any.register ~service:election_draft_voters
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        Pages_admin.election_draft_voters uuid se !Web_config.maxmailsatonce ()
        >>= Html.send
      )

  (* see http://www.regular-expressions.info/email.html *)
  let identity_rex = Pcre.regexp
                       ~flags:[`CASELESS]
                       "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}(,[A-Z0-9._%+-]*(,[1-9][0-9]*)?)?$"

  let is_identity x =
    match pcre_exec_opt ~rex:identity_rex x with
    | Some _ -> true
    | None -> false

  let merge_voters a b f =
    let weights =
      List.fold_left
        (fun accu sv ->
          let _, login, weight = split_identity sv.sv_id in
          let login = PString.lowercase_ascii login in
          SMap.add login weight accu
        ) SMap.empty a
    in
    let weights, res =
      List.fold_left
        (fun (weights, accu) sv_id ->
          let _, login, weight = split_identity sv_id in
          let login = PString.lowercase_ascii login in
          if SMap.mem login weights then
            (weights, accu)
          else (
            SMap.add login weight weights,
            {sv_id; sv_password = f sv_id} :: accu
          )
        ) (weights, List.rev a) b
    in
    List.rev res,
    Weight.(SMap.fold (fun _ x y -> x + y) weights zero)

  let bool_of_opt = function
    | None -> false
    | Some _ -> true

  let check_consistency voters =
    match voters with
    | [] -> true
    | voter :: voters ->
       let has_login, has_weight =
         let _, login, weight = split_identity_opt voter.sv_id in
         bool_of_opt login,
         bool_of_opt weight
       in
       let rec loop = function
         | [] -> true
         | voter :: voters ->
            let _, login, weight = split_identity_opt voter.sv_id in
            bool_of_opt login = has_login
            && bool_of_opt weight = has_weight
            && loop voters
       in
       loop voters

  let () =
    Any.register ~service:election_draft_voters_add
      (fun uuid voters ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            if se.se_public_creds_received then
              forbidden ()
            else (
              let voters = Pcre.split voters in
              let () =
                match List.find_opt (fun x -> not (is_identity x)) voters with
                | Some bad ->
                   Printf.ksprintf failwith (f_ "%S is not a valid identity") bad
                | None -> ()
              in
              let voters, total_weight =
                merge_voters se.se_voters voters (fun _ -> None)
              in
              let () =
                let expanded = Weight.expand ~total:total_weight total_weight in
                if Z.compare expanded Weight.max_expanded_weight > 0 then
                  Printf.ksprintf failwith
                    (f_ "The total weight (%s) cannot be handled. Its expanded value must be less than %s.")
                    Weight.(to_string total_weight)
                    (Z.to_string Weight.max_expanded_weight)
              in
              if not (check_consistency voters) then
                failwith
                  (s_ "The voter list is not consistent (a login or a weight is missing).");
              let uses_password_auth =
                match se.se_metadata.e_auth_config with
                | Some configs ->
                   List.exists
                     (fun {auth_system; _} -> auth_system = "password")
                     configs
                | None -> false
              in
              let cred_auth_is_server =
                se.se_metadata.e_cred_authority = Some "server"
              in
              if
                (uses_password_auth || cred_auth_is_server)
                && List.length voters > !Web_config.maxmailsatonce
              then
                Lwt.fail
                  (Failure
                     (Printf.sprintf (f_ "There are too many voters (max is %d)")
                        !Web_config.maxmailsatonce))
              else (
                se.se_voters <- voters;
                redir_preapply election_draft_voters uuid ()
              )
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
    Any.register ~service:election_draft_voters_remove_all
      (fun uuid () ->
        with_draft_election uuid (fun se ->
            if se.se_public_creds_received then
              forbidden ()
            else (
              se.se_voters <- [];
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

  let trustee_add_server se =
    let st_id = "server" and st_token = "" in
    let version = Option.get se.se_version 0 in
    let module G = (val Group.of_string ~version se.se_group) in
    let module Trustees = (val Trustees.get_by_version (Option.get se.se_version 0)) in
    let module K = Trustees.MakeSimple (G) (LwtRandom) in
    let* private_key = K.generate () in
    let* public_key = K.prove private_key in
    let st_public_key = string_of_trustee_public_key G.write public_key in
    let st_private_key = Some private_key in
    let st_name = Some "server" in
    let trustee = {st_id; st_token; st_public_key; st_private_key; st_name} in
    se.se_public_keys <- se.se_public_keys @ [trustee];
    return_unit

  let () =
    Any.register ~service:election_draft_trustee_add
      (fun uuid (st_id, name) ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let* () =
              if List.exists (fun x -> x.st_id = "server") se.se_public_keys then
                return_unit
              else trustee_add_server se
            in
            if is_email st_id then (
              let* st_token = generate_token () in
              let st_name = Some name in
              let trustee = {st_id; st_token; st_public_key = ""; st_private_key = None; st_name} in
              se.se_public_keys <- se.se_public_keys @ [trustee];
              redir_preapply election_draft_trustees uuid ()
            ) else (
              let msg = Printf.sprintf (f_ "%s is not a valid e-mail address!") st_id in
              let service = preapply ~service:election_draft_trustees uuid in
              Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
            )
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
    Any.register ~service:election_draft_credentials
      (fun (uuid, token) () ->
        without_site_user (fun () ->
            let* election = Web_persist.get_draft_election uuid in
            match election with
            | None -> fail_http `Not_found
            | Some se -> Pages_admin.election_draft_credentials token uuid se () >>= Html.send
          )
      )

  let handle_credentials_post uuid token creds =
    let* election = Web_persist.get_draft_election uuid in
    match election with
    | None -> fail_http `Not_found
    | Some se ->
       if se.se_public_creds <> token then forbidden () else
         if se.se_public_creds_received then forbidden () else
           let version = Option.get se.se_version 0 in
           let module G = (val Group.of_string ~version se.se_group : GROUP) in
           let fname = !Web_config.spool_dir / raw_string_of_uuid uuid / "public_creds.txt" in
           let* () =
             Web_election_mutex.with_lock uuid
               (fun () ->
                 Lwt_io.with_file
                   ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
                   ~perm:0o600 ~mode:Lwt_io.Output fname
                   (fun oc -> Lwt_io.write_chars oc creds)
               )
           in
           let* weights =
             let i = ref 1 in
             let* x = read_file fname in
             match x with
             | Some xs ->
                let weights =
                  List.fold_left
                    (fun weights x ->
                      try
                        let x, w = extract_weight x in
                        let x = G.of_string x in
                        if not (G.check x) then raise Exit;
                        incr i;
                        w :: weights
                      with _ ->
                        Printf.ksprintf failwith "invalid credential at line %d" !i
                    ) [] xs
                in
                let* () = write_file fname xs in
                return (List.sort compare weights)
             | None -> failwith "anomaly when reading back credentials"
           in
           let expected_weights =
             List.fold_left
               (fun accu {sv_id; _} ->
                 let _, _, weight = split_identity sv_id in
                 weight :: accu
               ) [] se.se_voters
             |> List.sort compare
           in
           if weights <> expected_weights then failwith "discrepancy in weights";
           let () = se.se_public_creds_received <- true in
           let* () = Web_persist.set_draft_election uuid se in
           Pages_admin.election_draft_credentials_done se () >>= Html.send

  let () =
    Any.register ~service:election_draft_credentials_post
      (fun (uuid, token) creds ->
        without_site_user (fun () ->
            let s = Lwt_stream.of_string creds in
            wrap_handler (fun () -> handle_credentials_post uuid token s)
          )
      )

  let () =
    Any.register ~service:election_draft_credentials_post_file
      (fun (uuid, token) creds ->
        without_site_user (fun () ->
            let s = Lwt_io.chars_of_file creds.Ocsigen_extensions.tmp_filename in
            wrap_handler (fun () -> handle_credentials_post uuid token s)
          )
      )

  module CG = Credential.MakeGenerate (LwtRandom)

  let () =
    Any.register ~service:election_draft_credentials_server
      (fun uuid () ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let nvoters = List.length se.se_voters in
            if nvoters > !Web_config.maxmailsatonce then
              Lwt.fail (Failure (Printf.sprintf (f_ "Cannot send credentials, there are too many voters (max is %d)") !Web_config.maxmailsatonce))
            else if nvoters = 0 then
              Lwt.fail (Failure (s_ "No voters"))
            else if se.se_questions.t_name = default_name then
              Lwt.fail (Failure (s_ "The election name has not been edited!"))
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
              let version = Option.get se.se_version 0 in
              let module G = (val Group.of_string ~version se.se_group : GROUP) in
              let module CMap = Map.Make (G) in
              let module CD = Credential.MakeDerive (G) in
              let show_weight =
                List.exists
                  (fun v ->
                    let _, _, weight = split_identity_opt v.sv_id in
                    weight <> None
                  ) se.se_voters
              in
              let* public_creds, private_creds =
                Lwt_list.fold_left_s (fun (public_creds, private_creds) v ->
                    let recipient, login, weight = split_identity v.sv_id in
                    let oweight = if show_weight then Some weight else None in
                    let has_passwords =
                      match se.se_metadata.e_auth_config with
                      | Some [{auth_system = "password"; _}] -> true
                      | _ -> false
                    in
                    let* cred = CG.generate () in
                    let pub_cred =
                      let x = CD.derive uuid cred in
                      G.(g **~ x)
                    in
                    let langs = get_languages se.se_metadata.e_languages in
                    let* subject, body =
                      Pages_voter.generate_mail_credential langs has_passwords
                        title ~login cred oweight url se.se_metadata
                    in
                    let* () = send_email (MailCredential uuid) ~recipient ~subject ~body in
                    return (CMap.add pub_cred weight public_creds, (v.sv_id, cred) :: private_creds)
                  ) (CMap.empty, []) se.se_voters
              in
              let private_creds = List.rev_map (fun (id, c) -> id ^ " " ^ c) private_creds in
              let* () = write_file ~uuid "private_creds.txt" private_creds in
              let public_creds =
                CMap.bindings public_creds
                |> List.map
                     (fun (cred, weight) ->
                       let cred = G.to_string cred in
                       if show_weight then
                         Printf.sprintf "%s,%s" cred (Weight.to_string weight)
                       else cred
                     )
              in
              let fname = !Web_config.spool_dir / raw_string_of_uuid uuid / "public_creds.txt" in
              let* () =
                Lwt_io.with_file
                  ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
                  ~perm:0o600 ~mode:Lwt_io.Output fname
                  (fun oc ->
                    Lwt_list.iter_s (Lwt_io.write_line oc) public_creds)
              in
              se.se_public_creds_received <- true;
              let service = preapply ~service:election_draft uuid in
              Pages_common.generic_page ~title:(s_ "Success") ~service
                (s_ "Credentials have been generated and mailed! You should download private credentials (and store them securely), in case someone loses his/her credential.") () >>= Html.send
            )
          )
      )

  let () =
    Any.register ~service:election_draft_credentials_get
      (fun uuid () ->
        with_draft_election_ro uuid
          (fun _ ->
            let* () = write_file ~uuid "private_creds.downloaded" [] in
            File.send ~content_type:"text/plain"
              (!Web_config.spool_dir / raw_string_of_uuid uuid / "private_creds.txt")
          )
      )

  let () =
    Any.register ~service:election_draft_trustee
      (fun (uuid, token) () ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        without_site_user
          ~fallback:(fun u ->
            let* election = Web_persist.get_draft_election uuid in
            match election with
            | None -> fail_http `Not_found
            | Some se ->
               if se.se_owner = u then (
                 Pages_admin.election_draft_trustees ~token uuid se () >>= Html.send
               ) else forbidden ()
          )
          (fun () ->
            let* election = Web_persist.get_draft_election uuid in
            match election with
            | None -> fail_http `Not_found
            | Some se ->
               match List.find_opt (fun t -> t.st_token = token) se.se_public_keys with
               | None -> forbidden ()
               | Some t ->
                  if t.st_public_key <> "" then
                    let msg = s_ "Your public key has already been received!" in
                    let title = s_ "Error" in
                    Pages_common.generic_page ~title msg () >>= Html.send ~code:403
                  else
                    Pages_admin.election_draft_trustee token uuid se () >>= Html.send
          )
      )

  let () =
    Any.register ~service:election_draft_trustee_post
      (fun (uuid, token) public_key ->
        without_site_user (fun () ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            if token = "" then
              forbidden ()
            else
              let* x =
                Web_election_mutex.with_lock uuid
                  (fun () ->
                    let* election = Web_persist.get_draft_election uuid in
                    match election with
                    | None -> fail_http `Not_found
                    | Some se ->
                       match List.find_opt (fun x -> token = x.st_token) se.se_public_keys with
                       | None -> return_none
                       | Some t ->
                          if t.st_public_key <> "" then
                            let msg = s_ "A public key already existed, the key you've just uploaded has been ignored!" in
                            let title = s_ "Error" in
                            return_some (title, msg, 400)
                          else
                            let version = Option.get se.se_version 0 in
                            let module G = (val Group.of_string ~version se.se_group : GROUP) in
                            let module Trustees = (val Trustees.get_by_version (Option.get se.se_version 0)) in
                            let pk = trustee_public_key_of_string G.read public_key in
                            let module K = Trustees.MakeCombinator (G) in
                            if not (K.check [`Single pk]) then
                              let msg = s_ "Invalid public key!" in
                              let title = s_ "Error" in
                              return_some (title, msg, 400)
                            else (
                              (* we keep pk as a string because of G.t *)
                              t.st_public_key <- public_key;
                              let* () = Web_persist.set_draft_election uuid se in
                              let msg = s_ "Your key has been received and checked!" in
                              let title = s_ "Success" in
                              return_some (title, msg, 200)
                            )
                  )
              in
              match x with
              | None -> forbidden ()
              | Some (title, msg, code) -> Pages_common.generic_page ~title msg () >>= Html.send ~code
          )
      )

  let () =
    Any.register ~service:election_draft_confirm
      (fun uuid () ->
        with_draft_election_ro uuid (fun se ->
            Pages_admin.election_draft_confirm uuid se () >>= Html.send
          )
      )

  let () =
    Any.register ~service:election_draft_create
      (fun uuid () ->
        with_draft_election ~save:false uuid (fun se ->
            Lwt.catch
              (fun () ->
                let* () = validate_election uuid se in
                redir_preapply election_admin uuid ()
              )
              (fun e ->
                Pages_admin.new_election_failure (`Exception e) () >>= Html.send
              )
          )
      )

  let destroy_election uuid =
    rmdir (!Web_config.spool_dir / raw_string_of_uuid uuid)

  let () =
    Any.register ~service:election_draft_destroy
      (fun uuid () ->
        with_draft_election ~save:false uuid (fun _ ->
            let* () = destroy_election uuid in
            Redirection.send (Redirection admin)
          )
      )

  let () =
    Any.register ~service:election_draft_import
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        let* _, a, b, c = get_elections_by_owner_sorted se.se_owner in
        Pages_admin.election_draft_import uuid se (a, b, c) ()
        >>= Html.send
      )

  let () =
    Any.register ~service:election_draft_import_post
      (fun uuid from ->
        let from = uuid_of_raw_string from in
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let from_s = raw_string_of_uuid from in
            let* voters = Web_persist.get_voters from in
            let* passwords = Web_persist.get_passwords from in
            let get_password =
              match passwords with
              | None -> fun _ -> None
              | Some p -> fun sv_id ->
                          let _, login, _ = split_identity sv_id in
                          SMap.find_opt login p
            in
            match voters with
            | Some voters ->
               if se.se_public_creds_received then
                 forbidden ()
               else (
                 let voters, total_weight =
                   merge_voters se.se_voters voters get_password
                 in
                 let expanded = Weight.expand ~total:total_weight total_weight in
                 if Z.compare expanded Weight.max_expanded_weight <= 0 then (
                   se.se_voters <- voters;
                   redir_preapply election_draft_voters uuid ()
                 ) else (
                   Pages_common.generic_page ~title:(s_ "Error")
                     ~service:(preapply ~service:election_draft_voters uuid)
                     (Printf.sprintf
                        (f_ "The total weight (%s) cannot be handled. Its expanded value must be less than %s.")
                        Weight.(to_string total_weight)
                        (Z.to_string Weight.max_expanded_weight)
                     ) ()
                   >>= Html.send
                 )
               )
            | None ->
               Pages_common.generic_page ~title:(s_ "Error")
                 ~service:(preapply ~service:election_draft_voters uuid)
                 (Printf.sprintf
                    (f_ "Could not retrieve voter list from election %s")
                    from_s)
                 () >>= Html.send
          )
      )

  let () =
    Any.register ~service:election_draft_import_trustees
      (fun uuid () ->
        let@ se = with_draft_election_ro uuid in
        let* _, a, b, c = get_elections_by_owner_sorted se.se_owner in
        Pages_admin.election_draft_import_trustees uuid se (a, b, c) ()
        >>= Html.send
      )

  exception TrusteeImportError of string

  let () =
    Any.register ~service:election_draft_import_trustees_post
      (fun uuid from ->
        let from = uuid_of_raw_string from in
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let* metadata = Web_persist.get_election_metadata from in
            Lwt.catch
              (fun () ->
                match metadata.e_trustees with
                | None -> Lwt.fail (TrusteeImportError (s_ "Could not retrieve trustees from selected election!"))
                | Some names ->
                   let* trustees = Web_persist.get_trustees from in
                   let version = Option.get se.se_version 0 in
                   let module G = (val Group.of_string ~version se.se_group : GROUP) in
                   let module Trustees = (val Trustees.get_by_version (Option.get se.se_version 0)) in
                   let module K = Trustees.MakeCombinator (G) in
                   let trustees = trustees_of_string G.read trustees in
                   if not (K.check trustees) then
                     Lwt.fail (TrusteeImportError (s_ "Imported trustees are invalid for this election!"))
                   else
                     let import_pedersen t names =
                       let* privs = Web_persist.get_private_keys from in
                       let* se_threshold_trustees =
                         match privs with
                         | Some privs ->
                            let rec loop ts pubs privs accu =
                              match ts, pubs, privs with
                              | stt_id :: ts, vo_public_key :: pubs, vo_private_key :: privs ->
                                 let stt_name = vo_public_key.trustee_name in
                                 let* stt_token = generate_token () in
                                 let stt_voutput = {vo_public_key; vo_private_key} in
                                 let stt_voutput = Some (string_of_voutput G.write stt_voutput) in
                                 let stt = {
                                     stt_id; stt_token; stt_voutput;
                                     stt_step = Some 7; stt_cert = None;
                                     stt_polynomial = None; stt_vinput = None;
                                     stt_name;
                                   } in
                                 loop ts pubs privs (stt :: accu)
                              | [], [], [] -> return (List.rev accu)
                              | _, _, _ -> Lwt.fail (TrusteeImportError (s_ "Inconsistency in imported election!"))
                            in loop names (Array.to_list t.t_verification_keys) privs []
                         | None -> Lwt.fail (TrusteeImportError (s_ "Encrypted decryption keys are missing!"))
                       in
                       se.se_threshold <- Some t.t_threshold;
                       se.se_threshold_trustees <- Some se_threshold_trustees;
                       se.se_threshold_parameters <- Some (string_of_threshold_parameters G.write t);
                       redir_preapply election_draft_threshold_trustees uuid ()
                     in
                     match trustees with
                     | [`Pedersen t] ->
                        import_pedersen t names
                     | [`Single x; `Pedersen t] when x.trustee_name = Some "server" ->
                        import_pedersen t (List.tl names)
                     | ts ->
                        let* ts =
                          try
                            List.map
                              (function
                               | `Single x -> x
                               | `Pedersen _ -> raise (TrusteeImportError (s_ "Unsupported trustees!"))
                              ) ts
                            |> return
                          with
                          | e -> Lwt.fail e
                        in
                        let* ts =
                          let module KG = Trustees.MakeSimple (G) (LwtRandom) in
                          List.combine names ts
                          |> Lwt_list.map_p
                               (fun (st_id, public_key) ->
                                 let* st_token, st_private_key, st_public_key =
                                   if st_id = "server" then (
                                     let* private_key = KG.generate () in
                                     let* public_key = KG.prove private_key in
                                     let public_key = string_of_trustee_public_key G.write public_key in
                                     return ("", Some private_key, public_key)
                                   ) else (
                                     let* st_token = generate_token () in
                                     let public_key = string_of_trustee_public_key G.write public_key in
                                     return (st_token, None, public_key)
                                   )
                                 in
                                 let st_name = public_key.trustee_name in
                                 return {st_id; st_token; st_public_key; st_private_key; st_name})
                        in
                        se.se_public_keys <- ts;
                        redir_preapply election_draft_trustees uuid ()
              )
              (function
               | TrusteeImportError msg ->
                  Pages_common.generic_page ~title:(s_ "Error")
                    ~service:(preapply ~service:election_draft_trustees uuid)
                    msg () >>= Html.send
               | e -> Lwt.fail e
              )
          )
      )

  let election_admin_handler ?shuffle_token ?tally_token uuid =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let@ election = with_election uuid in
    let* metadata = Web_persist.get_election_metadata uuid in
    let* site_user = Eliom_reference.get Web_state.site_user in
    match site_user with
    | Some u when metadata.e_owner = Some u ->
       let* state = Web_persist.get_election_state uuid in
       let module W = (val election) in
       let* pending_server_shuffle =
         match state with
         | `Shuffling ->
            if Election.has_nh_questions W.election then
              let* x = Web_persist.get_shuffles uuid in
              match x with
              | None -> return_true
              | Some _ -> return_false
            else return_false
         | _ -> return_false
       in
       let* () =
         if pending_server_shuffle then (
           let* cc = Web_persist.get_nh_ciphertexts election in
           let cc = nh_ciphertexts_of_string W.G.read cc in
           let* shuffle = W.E.shuffle_ciphertexts cc in
           let shuffle = string_of_shuffle W.G.write shuffle in
           let* x = Web_persist.append_to_shuffles election shuffle in
           match x with
           | Some h ->
              let sh = {sh_trustee = "server"; sh_hash = h; sh_name = Some "server"} in
              let* () = Web_persist.add_shuffle_hash uuid sh in
              Web_persist.remove_audit_cache uuid
           | None ->
              Lwt.fail (Failure (Printf.sprintf (f_ "Automatic shuffle by server has failed for election %s!") (raw_string_of_uuid uuid)))
         ) else return_unit
       in
       let get_tokens_decrypt () =
         (* this function is called only when there is a Pedersen trustee *)
         let* x = Web_persist.get_decryption_tokens uuid in
         match x with
         | Some x -> return x
         | None ->
            match metadata.e_trustees with
            | None -> failwith "missing trustees in get_tokens_decrypt"
            | Some ts ->
               let* ts = Lwt_list.map_s (fun _ -> generate_token ()) ts in
               let* () = Web_persist.set_decryption_tokens uuid ts in
               return ts
       in
       Pages_admin.election_admin ?shuffle_token ?tally_token election metadata state get_tokens_decrypt () >>= Html.send
    | Some _ ->
       let msg = s_ "You are not allowed to administer this election!" in
       Pages_common.generic_page ~title:(s_ "Forbidden") msg ()
       >>= Html.send ~code:403
    | _ ->
       redir_preapply site_login (None, ContSiteElection uuid) ()

  let () =
    Any.register ~service:election_admin
      (fun uuid () -> election_admin_handler uuid)

  let election_set_state state uuid () =
    let@ _ = with_metadata_check_owner uuid in
    let* allowed =
      let* state = Web_persist.get_election_state uuid in
      match state with
      | `Open | `Closed -> return_true
      | _ -> return_false
    in
    if allowed then (
      let state = if state then `Open else `Closed in
      let* () = Web_persist.set_election_state uuid state in
      let* dates = Web_persist.get_election_dates uuid in
      let* () =
        Web_persist.set_election_dates uuid
          {dates with e_auto_open = None; e_auto_close = None}
      in
      redir_preapply election_admin uuid ()
    ) else forbidden ()

  let () = Any.register ~service:election_open (election_set_state true)
  let () = Any.register ~service:election_close (election_set_state false)

  let election_set_result_hidden f uuid x =
    let@ _ = with_metadata_check_owner uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    Lwt.catch
      (fun () ->
        let* () = Web_persist.set_election_result_hidden uuid (f l x) in
        redir_preapply election_admin uuid ()
      )
      (function
       | Failure msg ->
          let service = preapply ~service:election_admin uuid in
          Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
       | e -> Lwt.fail e
      )

  let parse_datetime_from_post l x =
    let open (val l : Web_i18n_sig.GETTEXT) in
    try datetime_of_string ("\"" ^ x ^ ".000000\"") with
    | _ -> Printf.ksprintf failwith (f_ "%s is not a valid date!") x

  let () =
    Any.register ~service:election_hide_result
      (election_set_result_hidden
         (fun l x ->
           let open (val l : Web_i18n_sig.GETTEXT) in
           let t = parse_datetime_from_post l x in
           let max = datetime_add (now ()) (day days_to_publish_result) in
           if datetime_compare t max > 0 then
             Printf.ksprintf failwith
               (f_ "The date must be less than %d days in the future!")
               days_to_publish_result
           else
             Some t
         )
      )

  let () =
    Any.register ~service:election_show_result
      (election_set_result_hidden (fun _ () -> None))

  let () =
    Any.register ~service:election_auto_post
      (fun uuid (auto_open, auto_close) ->
        let@ _ = with_metadata_check_owner uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let auto_dates =
          try
            let format x =
              if x = "" then None
              else Some (parse_datetime_from_post l x)
            in
            Ok (format auto_open, format auto_close)
          with Failure e -> Error e
        in
        match auto_dates with
        | Ok (e_auto_open, e_auto_close) ->
           let* dates = Web_persist.get_election_dates uuid in
           let* () =
             Web_persist.set_election_dates uuid
               {dates with e_auto_open; e_auto_close}
           in
           redir_preapply election_admin uuid ()
        | Error msg ->
           let service = preapply ~service:election_admin uuid in
           Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
      )

  let () =
    Any.register ~service:election_delete
      (fun uuid () ->
        let@ _ = with_metadata_check_owner uuid in
        let* () = delete_election uuid in
        redir_preapply admin () ()
      )

  let () =
    let rex = Pcre.regexp "\".*\" \".*:(.*)\"" in
    Any.register ~service:election_missing_voters
      (fun (uuid, ()) () ->
        let@ _ = with_metadata_check_owner uuid in
        let* voters =
          let* file = read_file ~uuid (string_of_election_file ESVoters) in
          match file with
          | Some vs ->
             return (
                 List.fold_left (fun accu v ->
                     let _, login, _ = split_identity v in
                     SSet.add login accu
                   ) SSet.empty vs
               )
          | None -> return SSet.empty
        in
        let* voters =
          let* file = read_file ~uuid (string_of_election_file ESRecords) in
          match file with
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
        let* x = String.send (Buffer.contents buf, "text/plain") in
        return @@ Eliom_registration.cast_unknown_content_kind x
      )

  let () =
    let rex = Pcre.regexp "\"(.*)\\..*\" \".*:(.*)\"" in
    Any.register ~service:election_pretty_records
      (fun (uuid, ()) () ->
        let@ _ = with_metadata_check_owner uuid in
        let@ election = with_election uuid in
        let* records =
          let* file = read_file ~uuid (string_of_election_file ESRecords) in
          match file with
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
        Pages_admin.pretty_records election (List.rev records) () >>= Html.send
      )

  let () =
    Any.register ~service:election_project_result
      (fun ((uuid, ()), index) () ->
        if index < 0 then (
          fail_http `Not_found
        ) else (
          let* hidden =
            let* x = Web_persist.get_election_result_hidden uuid in
            match x with
            | None -> return_false
            | Some _ -> return_true
          in
          let* allow =
            if hidden then (
              let* metadata = Web_persist.get_election_metadata uuid in
              let* site_user = Eliom_reference.get Web_state.site_user in
              match site_user with
              | Some u when metadata.e_owner = Some u -> return_true
              | _ -> return_false
            ) else return_true
          in
          if allow then (
            let* result = Web_persist.get_election_result uuid in
            match result with
            | None -> fail_http `Not_found
            | Some result ->
               let result = election_result_of_string Yojson.Safe.read_json Yojson.Safe.read_json result in
               match result.result with
               | `List xs ->
                  (match List.nth_opt xs index with
                   | None -> fail_http `Not_found
                   | Some x ->
                      let* x = String.send (Yojson.Safe.to_string x, "application/json") in
                      return @@ Eliom_registration.cast_unknown_content_kind x
                  )
               | _ -> fail_http `Not_found
          ) else forbidden ()
        )
      )

  let copy_file src dst =
    let open Lwt_io in
    chars_of_file src |> chars_to_file dst

  let try_copy_file src dst =
    let* b = file_exists src in
    if b then copy_file src dst else return_unit

  let make_archive uuid =
    let uuid_s = raw_string_of_uuid uuid in
    let* temp_dir =
      Lwt_preemptive.detach (fun () ->
          let temp_dir = Filename.temp_file "belenios" "archive" in
          Sys.remove temp_dir;
          Unix.mkdir temp_dir 0o700;
          Unix.mkdir (temp_dir / "public") 0o755;
          Unix.mkdir (temp_dir / "restricted") 0o700;
          temp_dir
        ) ()
    in
    let* () =
      Lwt_list.iter_p (fun x ->
          try_copy_file (!Web_config.spool_dir / uuid_s / x) (temp_dir / "public" / x)
        ) [
          "election.json";
          "trustees.json";
          "public_creds.txt";
          "ballots.jsons";
          "result.json";
        ]
    in
    let* () =
      Lwt_list.iter_p (fun x ->
          try_copy_file (!Web_config.spool_dir / uuid_s / x) (temp_dir / "restricted" / x)
        ) [
          "voters.txt";
          "records";
        ]
    in
    let command =
      Printf.ksprintf Lwt_process.shell
        "cd \"%s\" && zip -r archive public restricted" temp_dir
    in
    let* r = Lwt_process.exec command in
    match r with
    | Unix.WEXITED 0 ->
       let fname = !Web_config.spool_dir / uuid_s / "archive.zip" in
       let fname_new = fname ^ ".new" in
       let* () = copy_file (temp_dir / "archive.zip") fname_new in
       let* () = Lwt_unix.rename fname_new fname in
       rmdir temp_dir
    | _ ->
       Printf.ksprintf Ocsigen_messages.errlog
         "Error while creating archive.zip for election %s, temporary directory left in %s"
         uuid_s temp_dir;
       return_unit

  let () =
    Any.register ~service:election_download_archive
      (fun (uuid, ()) () ->
        let@ _ = with_metadata_check_owner uuid in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* state = Web_persist.get_election_state uuid in
        if state = `Archived then (
          let uuid_s = raw_string_of_uuid uuid in
          let archive_name = !Web_config.spool_dir / uuid_s / "archive.zip" in
          let* b = file_exists archive_name in
          let* () = if not b then make_archive uuid else return_unit in
          File.send ~content_type:"application/zip" archive_name
        ) else (
          let service = preapply ~service:election_admin uuid in
          Pages_common.generic_page ~title:(s_ "Error") ~service
            (s_ "The election is not archived!") () >>= Html.send
        )
      )

  let find_trustee_id uuid token =
    let* x = Web_persist.get_decryption_tokens uuid in
    match x with
    | None -> return (int_of_string_opt token)
    | Some tokens ->
       let rec find i = function
         | [] -> None
         | t :: ts -> if t = token then Some i else find (i+1) ts
       in
       return (find 1 tokens)

  let () =
    Any.register ~service:election_tally_trustees
      (fun (uuid, token) () ->
        without_site_user
          ~fallback:(fun _ ->
            election_admin_handler ~tally_token:token uuid
          )
          (fun () ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let@ election = with_election uuid in
            let* state = Web_persist.get_election_state uuid in
            match state with
            | `EncryptedTally _ ->
               let* x = find_trustee_id uuid token in
               (match x with
                | Some trustee_id ->
                   let* pds = Web_persist.get_partial_decryptions uuid in
                   if List.mem_assoc trustee_id pds then (
                     Pages_common.generic_page ~title:(s_ "Error")
                       (s_ "Your partial decryption has already been received and checked!")
                       () >>= Html.send
                   ) else (
                     Pages_admin.tally_trustees election trustee_id token () >>= Html.send
                   )
                | None -> forbidden ()
               )
            | `Open | `Closed | `Shuffling ->
               let msg = s_ "The election is not ready to be tallied. Please come back later." in
               Pages_common.generic_page ~title:(s_ "Forbidden") msg () >>= Html.send ~code:403
            | `Tallied | `Archived ->
               let msg = s_ "The election has already been tallied." in
               Pages_common.generic_page ~title:(s_ "Forbidden") msg () >>= Html.send ~code:403
          )
      )

  exception TallyEarlyError

  let render_tally_early_error_as_forbidden f =
    Lwt.catch f
      (function
       | TallyEarlyError -> forbidden ()
       | e -> Lwt.fail e)

  let () =
    Any.register ~service:election_tally_trustees_post
      (fun (uuid, token) partial_decryption ->
        let@ () = render_tally_early_error_as_forbidden in
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* () =
          let* state = Web_persist.get_election_state uuid in
          match state with
          | `EncryptedTally _ -> return ()
          | _ -> Lwt.fail TallyEarlyError
        in
        let* trustee_id =
          let* x = find_trustee_id uuid token in
          match x with
          | Some x -> return x
          | None -> Lwt.fail TallyEarlyError
        in
        let* pds = Web_persist.get_partial_decryptions uuid in
        let* () =
          if List.mem_assoc trustee_id pds then Lwt.fail TallyEarlyError else return ()
        in
        let* () =
          if trustee_id > 0 then return () else fail_http `Not_found
        in
        let@ election = with_election uuid in
        let module W = (val election) in
        let* pks =
          let* trustees = Web_persist.get_trustees uuid in
          let trustees = trustees_of_string W.G.read trustees in
          trustees
          |> List.map
               (function
                | `Single x -> [x]
                | `Pedersen t -> Array.to_list t.t_verification_keys
               )
          |> List.flatten
          |> Array.of_list
          |> return
        in
        let pk = pks.(trustee_id-1).trustee_public_key in
        let pd = partial_decryption_of_string W.G.read partial_decryption in
        let et = !Web_config.spool_dir / raw_string_of_uuid uuid / string_of_election_file ESETally in
        let* et = Lwt_io.chars_of_file et |> Lwt_stream.to_string in
        let et = encrypted_tally_of_string W.G.read et in
        if W.E.check_factor et pk pd then (
          let pds = (trustee_id, partial_decryption) :: pds in
          let* () = Web_persist.set_partial_decryptions uuid pds in
          Pages_common.generic_page ~title:(s_ "Success")
            (s_ "Your partial decryption has been received and checked!") () >>=
            Html.send
        ) else (
          let service = preapply ~service:election_tally_trustees (uuid, token) in
          Pages_common.generic_page ~title:(s_ "Error") ~service
            (s_ "The partial decryption didn't pass validation!") () >>=
            Html.send
      ))

  let handle_election_tally_release uuid () =
    let@ () = render_tally_early_error_as_forbidden in
    let@ _ = with_metadata_check_owner uuid in
    let* l = get_preferred_gettext () in
    let open (val l) in
    let uuid_s = raw_string_of_uuid uuid in
    let@ election = with_election uuid in
    let module W = (val election) in
    let* () =
      let* state = Web_persist.get_election_state uuid in
      match state with
      | `EncryptedTally _ -> return_unit
      | _ -> Lwt.fail TallyEarlyError
    in
    let* ntallied =
      let* hashes = Web_persist.get_ballot_hashes uuid in
      let weights = List.map snd hashes in
      let open Weight in
      Lwt_list.fold_left_s (fun x y -> return (x + y)) zero weights
    in
    let* et =
      !Web_config.spool_dir / uuid_s / string_of_election_file ESETally |>
        Lwt_io.chars_of_file |> Lwt_stream.to_string >>=
        wrap1 (encrypted_tally_of_string W.G.read)
    in
    let* trustees = Web_persist.get_trustees uuid in
    let trustees = trustees_of_string W.G.read trustees in
    let* pds = Web_persist.get_partial_decryptions uuid in
    let pds = List.map snd pds in
    let pds = List.map (partial_decryption_of_string W.G.read) pds in
    let* shuffles, shufflers =
      let* x = Web_persist.get_shuffles uuid in
      match x with
      | None -> return (None, None)
      | Some s ->
         let s = List.map (shuffle_of_string W.G.read) s in
         let* x = Web_persist.get_shuffle_hashes uuid in
         match x with
         | None -> return (Some s, None)
         | Some x ->
            let x =
              x
              |> List.map (fun x -> if x.sh_hash = "" then [] else [x.sh_name])
              |> List.flatten
            in
            assert (List.length s = List.length x);
            return (Some s, Some x)
    in
    match W.E.compute_result ?shuffles ?shufflers ntallied et pds trustees with
    | Ok result ->
       let* () =
         let result = string_of_election_result W.G.write W.write_result result in
         write_file ~uuid (string_of_election_file ESResult) [result]
       in
       let* () = Web_persist.remove_audit_cache uuid in
       let* () = Web_persist.set_election_state uuid `Tallied in
       let* dates = Web_persist.get_election_dates uuid in
       let* () = Web_persist.set_election_dates uuid {dates with e_tally = Some (now ())} in
       let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "decryption_tokens.json") in
       let* () = cleanup_file (!Web_config.spool_dir / uuid_s / "shuffles.jsons") in
       let* () = Web_persist.clear_shuffle_token uuid in
       redir_preapply election_home (uuid, ()) ()
    | Error e ->
       let msg =
         Printf.sprintf
           (f_ "An error occurred while computing the result (%s). Most likely, it means that some trustee has not done his/her job.")
           (Trustees.string_of_combination_error e)
       in
       Pages_common.generic_page ~title:(s_ "Error") msg () >>= Html.send

  let () =
    Any.register ~service:election_tally_release
      handle_election_tally_release

  module type ELECTION_LWT = ELECTION_OPS with type 'a m = 'a Lwt.t

  let perform_server_side_decryption uuid e metadata tally =
    let module W = (val e : Site_common_sig.ELECTION_LWT) in
    let tally = encrypted_tally_of_string W.G.read tally in
    let decrypt i =
      let* x = Web_persist.get_private_key uuid in
      match x with
      | Some sk ->
         let* pd = W.E.compute_factor tally sk in
         let pd = string_of_partial_decryption W.G.write pd in
         Web_persist.set_partial_decryptions uuid [i, pd]
      | None ->
         Printf.ksprintf failwith
           "missing private key for server in election %s"
           (raw_string_of_uuid uuid)
    in
    let trustees =
      match metadata.e_trustees with
      | None -> ["server"]
      | Some ts -> ts
    in
    trustees
    |> List.mapi (fun i t -> i, t)
    |> Lwt_list.exists_s
         (fun (i, t) ->
           if t = "server" then (
             let* () = decrypt (i + 1) in
             return_false
           ) else return_true
         )

  let transition_to_encrypted_tally uuid e metadata tally =
    let* () =
      Web_persist.set_election_state uuid (`EncryptedTally (0, 0, ""))
    in
    let* b = perform_server_side_decryption uuid e metadata tally in
    if b then
      redir_preapply election_admin uuid ()
    else
      handle_election_tally_release uuid ()

  let () =
    Any.register ~service:election_compute_encrypted_tally
      (fun uuid () ->
        let@ () = render_tally_early_error_as_forbidden in
        let@ metadata = with_metadata_check_owner uuid in
        let@ election = with_election uuid in
        let module W = (val election) in
        let* () =
          let* state = Web_persist.get_election_state uuid in
          match state with
          | `Closed -> return ()
          | _ -> Lwt.fail TallyEarlyError
        in
        let* tally = Web_persist.compute_encrypted_tally election in
        if Election.has_nh_questions W.election then (
          let* () = Web_persist.set_election_state uuid `Shuffling in
          redir_preapply election_admin uuid ()
        ) else (
          transition_to_encrypted_tally uuid election metadata tally
        )
      )

  let () =
    Any.register ~service:election_shuffle_link
      (fun (uuid, token) () ->
        without_site_user
          ~fallback:(fun _ ->
            election_admin_handler ~shuffle_token:token uuid
          )
          (fun () ->
            let* expected_token = Web_persist.get_shuffle_token uuid in
            match expected_token with
            | Some x when token = x.tk_token ->
               let@ election = with_election uuid in
               Pages_admin.shuffle election token >>= Html.send
            | _ -> forbidden ()
          )
      )

  let () =
    Any.register ~service:election_shuffle_post
      (fun (uuid, token) shuffle ->
        let@ election = with_election uuid in
        without_site_user (fun () ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            let* expected_token = Web_persist.get_shuffle_token uuid in
            match expected_token with
            | Some x when token = x.tk_token ->
               Lwt.catch
                 (fun () ->
                   let* y = Web_persist.append_to_shuffles election shuffle in
                   match y with
                   | Some h ->
                      let* () = Web_persist.clear_shuffle_token uuid in
                      let sh = {sh_trustee = x.tk_trustee; sh_hash = h; sh_name = x.tk_name} in
                      let* () = Web_persist.add_shuffle_hash uuid sh in
                      let* () = Web_persist.remove_audit_cache uuid in
                      Pages_common.generic_page ~title:(s_ "Success") (s_ "The shuffle has been successfully applied!") () >>= Html.send
                   | None ->
                      Pages_common.generic_page ~title:(s_ "Error") (s_ "An error occurred while applying the shuffle.") () >>= Html.send
                 )
                 (fun e ->
                   Pages_common.generic_page ~title:(s_ "Error") (Printf.sprintf (f_ "Data is invalid! (%s)") (Printexc.to_string e)) () >>= Html.send
                 )
            | _ -> forbidden ()
          )
      )

  let extract_names trustees =
    trustees
    |> List.map
         (function
          | `Pedersen x ->
             x.t_verification_keys
             |> Array.to_list
             |> List.map (fun x -> x.trustee_name)
          | `Single x -> [x.trustee_name]
         )
    |> List.flatten

  let get_trustee_names uuid =
    let* trustees = Web_persist.get_trustees uuid in
    let trustees = trustees_of_string Yojson.Safe.read_json trustees in
    return (extract_names trustees)

  let get_trustee_name uuid metadata trustee =
    match metadata.e_trustees with
    | None -> return_none
    | Some xs ->
       let* names = get_trustee_names uuid in
       return (List.assoc trustee (List.combine xs names))

  let () =
    Any.register ~service:election_shuffler_select
      (fun () (uuid, trustee) ->
        let@ metadata = with_metadata_check_owner uuid in
        let* name = get_trustee_name uuid metadata trustee in
        let* () = Web_persist.clear_shuffle_token uuid in
        let* _ = Web_persist.gen_shuffle_token uuid trustee name in
        redir_preapply election_admin uuid ()
      )

  let () =
    Any.register ~service:election_shuffler_skip_confirm
      (fun () (uuid, trustee) ->
        let@ _ = with_metadata_check_owner uuid in
        Pages_admin.election_shuffler_skip_confirm uuid trustee >>= Html.send
      )

  let () =
    Any.register ~service:election_shuffler_skip
      (fun () (uuid, trustee) ->
        let@ metadata = with_metadata_check_owner uuid in
        let* sh_name = get_trustee_name uuid metadata trustee in
        let* () = Web_persist.clear_shuffle_token uuid in
        let sh = {sh_trustee = trustee; sh_hash = ""; sh_name} in
        let* () = Web_persist.add_shuffle_hash uuid sh in
        redir_preapply election_admin uuid ()
      )

  let () =
    Any.register ~service:election_decrypt (fun uuid () ->
        let@ () = render_tally_early_error_as_forbidden in
        let@ metadata = with_metadata_check_owner uuid in
        let@ election = with_election uuid in
        let* () =
          let* state = Web_persist.get_election_state uuid in
          match state with
          | `Shuffling -> return ()
          | _ -> Lwt.fail TallyEarlyError
        in
        let* tally =
          let* x = Web_persist.compute_encrypted_tally_after_shuffling election in
          match x with
          | Some x -> return x
          | None -> Lwt.fail (Failure "election_decrypt handler: compute_encrypted_tally_after_shuffling")
        in
        transition_to_encrypted_tally uuid election metadata tally
      )

  let () =
    Any.register ~service:election_draft_threshold_set
      (fun uuid threshold ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            match se.se_threshold_trustees with
            | None ->
               let msg = s_ "Please add some trustees first!" in
               let service = preapply ~service:election_draft_threshold_trustees uuid in
               Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
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
                 let msg = s_ "The threshold must be positive and smaller than the number of trustees!" in
                 let service = preapply ~service:election_draft_threshold_trustees uuid in
                 Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
               )
          )
      )

  let () =
    Any.register ~service:election_draft_threshold_trustee_add
      (fun uuid (stt_id, name) ->
        with_draft_election uuid (fun se ->
            let* l = get_preferred_gettext () in
            let open (val l) in
            if is_email stt_id then (
              let stt_name = Some name in
              let* stt_token = generate_token () in
              let trustee = {
                  stt_id; stt_token; stt_step = None;
                  stt_cert = None; stt_polynomial = None;
                  stt_vinput = None; stt_voutput = None;
                  stt_name;
                } in
              let trustees =
                match se.se_threshold_trustees with
                | None -> Some [trustee]
                | Some t -> Some (t @ [trustee])
              in
              se.se_threshold_trustees <- trustees;
              redir_preapply election_draft_threshold_trustees uuid ()
            ) else (
              let msg = Printf.sprintf (f_ "%s is not a valid e-mail address!") stt_id in
              let service = preapply ~service:election_draft_threshold_trustees uuid in
              Pages_common.generic_page ~title:(s_ "Error") ~service msg () >>= Html.send
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
    Any.register ~service:election_draft_threshold_trustee
      (fun (uuid, token) () ->
        without_site_user
          ~fallback:(fun u ->
            let* election = Web_persist.get_draft_election uuid in
            match election with
            | None -> fail_http `Not_found
            | Some se ->
               if se.se_owner = u then (
                 Pages_admin.election_draft_threshold_trustees ~token uuid se () >>= Html.send
               ) else forbidden ()
          )
          (fun () ->
            let* election = Web_persist.get_draft_election uuid in
            match election with
            | None -> fail_http `Not_found
            | Some se -> Pages_admin.election_draft_threshold_trustee token uuid se () >>= Html.send
          )
      )

  let wrap_handler_without_site_user f =
    without_site_user (fun () -> wrap_handler f)

  let () =
    Any.register ~service:election_draft_threshold_trustee_post
      (fun (uuid, token) data ->
        wrap_handler_without_site_user
          (fun () ->
            let* () =
              Web_election_mutex.with_lock uuid
                (fun () ->
                  let* election = Web_persist.get_draft_election uuid in
                  match election with
                  | None -> fail_http `Not_found
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
                     let version = Option.get se.se_version 0 in
                     let module G = (val Group.of_string ~version se.se_group : GROUP) in
                     let module Trustees = (val Trustees.get_by_version (Option.get se.se_version 0)) in
                     let module P = Trustees.MakePKI (G) (LwtRandom) in
                     let module C = Trustees.MakeChannels (G) (LwtRandom) (P) in
                     let module K = Trustees.MakePedersen (G) (LwtRandom) (P) (C) in
                     let* () =
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
                     let* () =
                       if Array.forall (fun x -> x.stt_step = Some 2) ts then (
                         (try
                            K.step2 (get_certs ());
                            Array.iter (fun x -> x.stt_step <- Some 3) ts;
                          with e ->
                            se.se_threshold_error <- Some (Printexc.to_string e)
                         ); return_unit
                       ) else return_unit
                     in
                     let* () =
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
                     let* () =
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

  module HashedInt = struct
    type t = int
    let equal = (=)
    let hash x = x
  end

  module Captcha_throttle = Lwt_throttle.Make (HashedInt)
  let captcha_throttle = Captcha_throttle.create ~rate:1 ~max:5 ~n:1

  let signup_captcha_handler service error email =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* b = Captcha_throttle.wait captcha_throttle 0 in
    if b then
      let* challenge = Web_captcha.create_captcha () in
      Pages_admin.signup_captcha ~service error challenge email
    else
      let service = preapply ~service:signup_captcha service in
      Pages_common.generic_page ~title:(s_ "Account creation") ~service
        (s_ "You cannot create an account now. Please try later.") ()

  let () =
    Html.register ~service:signup_captcha
      (fun service () ->
        let* b = Eliom_reference.get Web_state.show_cookie_disclaimer in
        if b then
          Pages_admin.privacy_notice (ContSignup service)
        else
          signup_captcha_handler service None ""
      )

  let () =
    Html.register ~service:signup_captcha_post
      (fun service (challenge, (response, email)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* error =
          let* ok = Web_captcha.check_captcha ~challenge ~response in
          if ok then
            if is_email email then return_none else return_some BadAddress
          else return_some BadCaptcha
        in
        match error with
        | None ->
           let* () = Web_signup.send_confirmation_link l ~service email in
           let* () = Eliom_reference.set Web_state.signup_address (Some email) in
           Pages_admin.signup_login ()
        | _ -> signup_captcha_handler service error email
      )

  let changepw_captcha_handler service error email username =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let* b = Captcha_throttle.wait captcha_throttle 1 in
    if b then
      let* challenge = Web_captcha.create_captcha () in
      Pages_admin.signup_changepw ~service error challenge email username
    else
      let service = preapply ~service:changepw_captcha service in
      Pages_common.generic_page ~title:(s_ "Change password") ~service
        (s_ "You cannot change your password now. Please try later.") ()

  let () =
    Html.register ~service:changepw_captcha
      (fun service () -> changepw_captcha_handler service None "" "")

  let () =
    Html.register ~service:changepw_captcha_post
      (fun service (challenge, (response, (email, username))) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* error =
          let* ok = Web_captcha.check_captcha ~challenge ~response in
          if ok then return_none
          else return_some BadCaptcha
        in
        match error with
        | None ->
           let* () =
             let* x = Web_auth_password.lookup_account ~service ~email ~username in
             match x with
             | None ->
                return (
                    Printf.ksprintf Ocsigen_messages.warning
                      "Unsuccessful attempt to change the password of %S (%S) for service %s"
                      username email service
                  )
             | Some (username, address) ->
                let* () = Eliom_reference.set Web_state.signup_address (Some address) in
                Web_signup.send_changepw_link l ~service ~address ~username
           in
           Pages_admin.signup_login ()
        | _ -> changepw_captcha_handler service error email username
      )

  let () =
    Any.register ~service:signup_login_post
      (fun () code ->
        let code = PString.trim code in
        let* address = Eliom_reference.get Web_state.signup_address in
        match address with
        | None -> forbidden ()
        | Some address ->
           let* x = Web_signup.confirm_link address in
           match x with
           | Some (code2, service, kind) when code = code2 ->
              let* () = Eliom_reference.set Web_state.signup_env (Some (service, kind)) in
              redir_preapply signup () ()
           | _ -> forbidden ()
      )

  let () =
    Any.register ~service:signup
      (fun () () ->
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match address, x with
        | Some address, Some (_, `CreateAccount) -> Pages_admin.signup address None "" >>= Html.send
        | Some address, Some (_, `ChangePassword username) -> Pages_admin.changepw ~username ~address None >>= Html.send
        | _ -> forbidden ()
      )

  let () =
    Any.register ~service:signup_post
      (fun () (username, (password, password2)) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match address, x with
        | Some email, Some (service, `CreateAccount) ->
           if password = password2 then (
             let user = { user_name = username; user_domain = service } in
             let* x = Web_auth_password.add_account user ~password ~email in
             match x with
             | Ok () ->
                let* () = Web_signup.remove_link email in
                let* () = Eliom_reference.unset Web_state.signup_address in
                let* () = Eliom_reference.unset Web_state.signup_env in
                let service = preapply ~service:site_login (Some service, ContSiteAdmin) in
                Pages_common.generic_page ~title:(s_ "Account creation") ~service (s_ "The account has been created.") ()
                >>= Html.send
             | Error e -> Pages_admin.signup email (Some e) username >>= Html.send
           ) else Pages_admin.signup email (Some PasswordMismatch) username >>= Html.send
        | _ -> forbidden ()
      )

  let () =
    Any.register ~service:changepw_post
      (fun () (password, password2) ->
        let* l = get_preferred_gettext () in
        let open (val l) in
        let* address = Eliom_reference.get Web_state.signup_address in
        let* x = Eliom_reference.get Web_state.signup_env in
        match address, x with
        | Some address, Some (service, `ChangePassword username) ->
           if password = password2 then (
             let user = { user_name = username; user_domain = service } in
             let* x = Web_auth_password.change_password user ~password in
             match x with
             | Ok () ->
                let* () = Web_signup.remove_link address in
                let* () = Eliom_reference.unset Web_state.signup_address in
                let* () = Eliom_reference.unset Web_state.signup_env in
                let service = preapply ~service:site_login (Some service, ContSiteAdmin) in
                Pages_common.generic_page ~title:(s_ "Change password") ~service (s_ "The password has been changed.") ()
                >>= Html.send
             | Error e -> Pages_admin.changepw ~username ~address (Some e) >>= Html.send
           ) else Pages_admin.changepw ~username ~address (Some PasswordMismatch) >>= Html.send
        | _ -> forbidden ()
      )

  let () =
    Html.register ~service:compute_fingerprint
      (fun () () -> Pages_admin.compute_fingerprint ())

  let extract_automatic_data_draft uuid_s =
    let uuid = uuid_of_raw_string uuid_s in
    let* election = Web_persist.get_draft_election uuid in
    match election with
    | None -> return_none
    | Some se ->
       let t = Option.get se.se_creation_date default_creation_date in
       let next_t = datetime_add t (day days_to_delete) in
       return_some (`Destroy, uuid, next_t)

  let extract_automatic_data_validated uuid_s =
    let uuid = uuid_of_raw_string uuid_s in
    let* election = Web_persist.get_raw_election uuid in
    match election with
    | None -> return_none
    | Some _ ->
       let* state = Web_persist.get_election_state uuid in
       let* dates = Web_persist.get_election_dates uuid in
       match state with
       | `Open | `Closed | `Shuffling | `EncryptedTally _ ->
          let t = Option.get dates.e_finalization default_validation_date in
          let next_t = datetime_add t (day days_to_delete) in
          return_some (`Delete, uuid, next_t)
       | `Tallied ->
          let t = Option.get dates.e_tally default_tally_date in
          let next_t = datetime_add t (day days_to_archive) in
          return_some (`Archive, uuid, next_t)
       | `Archived ->
          let t = Option.get dates.e_archive default_archive_date in
          let next_t = datetime_add t (day days_to_delete) in
          return_some (`Delete, uuid, next_t)

  let try_extract extract x =
    Lwt.catch
      (fun () -> extract x)
      (fun _ -> return_none)

  let get_next_actions () =
    Lwt_unix.files_of_directory !Web_config.spool_dir |>
      Lwt_stream.to_list >>=
      Lwt_list.filter_map_s
        (fun x ->
          if x = "." || x = ".." then return_none
          else (
            let* r = try_extract extract_automatic_data_draft x in
            match r with
            | None -> try_extract extract_automatic_data_validated x
            | x -> return x
          )
        )

  let process_election_for_data_policy (action, uuid, next_t) =
    let uuid_s = raw_string_of_uuid uuid in
    let now = now () in
    let action, comment = match action with
      | `Destroy -> destroy_election, "destroyed"
      | `Delete -> delete_election, "deleted"
      | `Archive -> archive_election, "archived"
    in
    if datetime_compare now next_t > 0 then (
      let* () = action uuid in
      return (
          Printf.ksprintf Ocsigen_messages.warning
            "Election %s has been automatically %s" uuid_s comment
        )
    ) else return_unit

  let rec data_policy_loop () =
    let open Ocsigen_messages in
    let () = accesslog "Data policy process started" in
    let* elections = get_next_actions () in
    let* () = Lwt_list.iter_s process_election_for_data_policy elections in
    let () = accesslog "Data policy process completed" in
    let* () = Lwt_unix.sleep 3600. in
    data_policy_loop ()

end
