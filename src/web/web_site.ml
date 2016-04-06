(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2016 Inria                                           *)
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
open Serializable_j
open Signatures
open Common
open Web_serializable_builtin_t
open Web_serializable_j
open Web_common
open Web_services

let source_file = ref "belenios.tar.gz"

let ( / ) = Filename.concat

module PString = String

open Eliom_service
open Eliom_registration

module LwtRandom = MakeLwtRandom (struct let rng = make_rng () end)

(* Table with elections in setup mode. *)
let election_stable = Ocsipersist.open_table "site_setup"

(* Table with tokens given to trustees. *)
let election_pktokens = Ocsipersist.open_table "site_pktokens"

(* Table with tokens given to credential authorities. *)
let election_credtokens = Ocsipersist.open_table "site_credtokens"

module T = Web_templates

let raw_find_election uuid =
  lwt raw_election = Web_persist.get_raw_election uuid in
  match raw_election with
  | Some raw_election ->
     return (Group.election_params_of_string raw_election)
  | _ -> Lwt.fail Not_found

module WCacheTypes = struct
  type key = string
  type value = (module ELECTION_DATA)
end

module WCache = Ocsigen_cache.Make (WCacheTypes)

let find_election =
  let cache = new WCache.cache raw_find_election 100 in
  fun x -> cache#find x

let dump_passwords dir table =
  Lwt_io.(with_file Output (dir / "passwords.csv") (fun oc ->
    Ocsipersist.iter_step (fun voter (salt, hashed) ->
      write_line oc (voter ^ "," ^ salt ^ "," ^ hashed)
    ) table
  ))

let finalize_election uuid se =
  let uuid_s = Uuidm.to_string uuid in
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
  let module KG = Election.MakeSimpleDistKeyGen (G) (LwtRandom) in
  lwt trustees, public_keys, private_key =
    match se.se_public_keys with
    | [] ->
       lwt private_key, public_key = KG.generate_and_prove () in
       return (None, [public_key], Some private_key)
    | _ :: _ ->
       return (
         Some (List.map (fun {st_id; _} -> st_id) se.se_public_keys),
         (List.map
            (fun {st_public_key; _} ->
              if st_public_key = "" then failwith "some public keys are missing";
              trustee_public_key_of_string G.read st_public_key
            ) se.se_public_keys),
         None)
  in
  let y = KG.combine (Array.of_list public_keys) in
  (* election parameters *)
  let metadata = { se.se_metadata with e_trustees = trustees } in
  let template = se.se_questions in
  let params = {
    e_description = template.t_description;
    e_name = template.t_name;
    e_public_key = {wpk_group = G.group; wpk_y = y};
    e_questions = template.t_questions;
    e_uuid = uuid;
    e_short_name = template.t_short_name;
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
            Lwt_io.write oc (what v) >>
              Lwt_io.write oc "\n") xs)
  in
  Lwt_unix.mkdir dir 0o700 >>
  create_file "public_keys.jsons" (string_of_trustee_public_key G.write) public_keys >>
  create_file "voters.txt" (fun x -> x.sv_id) se.se_voters >>
  create_file "metadata.json" string_of_metadata [metadata] >>
  create_file "election.json" (fun x -> x) [raw_election] >>
  (* construct Web_election instance *)
  let election = Group.election_params_of_string raw_election in
  let module W = Web_election.Make ((val election)) (LwtRandom) in
  (* set up authentication *)
  lwt () =
    match metadata.e_auth_config with
    | None -> return ()
    | Some xs ->
       let auth_config =
         List.map (fun {auth_system; auth_instance; auth_config} ->
           auth_instance, (auth_system, List.map snd auth_config)
         ) xs
       in
       Web_persist.set_auth_config uuid_s auth_config
  in
  (* inject credentials *)
  lwt () =
    let fname = !spool_dir / uuid_s ^ ".public_creds.txt" in
    Lwt_io.lines_of_file fname |>
    Lwt_stream.iter_s W.B.inject_cred >>
    W.B.update_files () >>
    Lwt_unix.unlink fname
  in
  (* create file with private key, if any *)
  lwt () =
    match private_key with
    | None -> return_unit
    | Some x -> create_file "private_key.json" string_of_number [x]
  in
  (* clean up setup database *)
  Ocsipersist.remove election_credtokens se.se_public_creds >>
  Lwt_list.iter_s
    (fun {st_token; _} ->
      Ocsipersist.remove election_pktokens st_token)
    se.se_public_keys >>
  Ocsipersist.remove election_stable uuid_s >>
  (* inject passwords *)
  (match metadata.e_auth_config with
  | Some [{auth_system = "password"; _}] ->
     let table = "password_" ^ underscorize uuid_s in
     let table = Ocsipersist.open_table table in
     Lwt_list.iter_s
       (fun v ->
         let _, login = split_identity v.sv_id in
         match v.sv_password with
         | Some x -> Ocsipersist.add table login x
         | None -> return_unit
       ) se.se_voters >>
       dump_passwords (!spool_dir / uuid_s) table
  | _ -> return_unit) >>
  (* finish *)
  Web_persist.set_election_state uuid_s `Open >>
  Web_persist.set_election_date uuid_s (now ())

let cleanup_table ?uuid_s table =
  let table = Ocsipersist.open_table table in
  match uuid_s with
  | None ->
     lwt indexes = Ocsipersist.fold_step (fun k _ accu ->
       return (k :: accu)) table []
     in
     Lwt_list.iter_s (Ocsipersist.remove table) indexes
  | Some u -> Ocsipersist.remove table u

let cleanup_file f =
  try_lwt Lwt_unix.unlink f
  with _ -> return_unit

let archive_election uuid_s =
  let uuid_u = underscorize uuid_s in
  lwt () = cleanup_table ~uuid_s "election_states" in
  lwt () = cleanup_table ~uuid_s "election_pds" in
  lwt () = cleanup_table ~uuid_s "auth_configs" in
  lwt () = cleanup_table ("password_" ^ uuid_u) in
  lwt () = cleanup_table ("records_" ^ uuid_u) in
  lwt () = cleanup_table ("creds_" ^ uuid_u) in
  lwt () = cleanup_table ("ballots_" ^ uuid_u) in
  lwt () = cleanup_file (!spool_dir / uuid_s / "private_key.json") in
  return_unit

let () = Any.register ~service:home
  (fun () () ->
    Eliom_reference.unset Web_state.cont >>
    T.home () >>= Html5.send
  )

let get_finalized_elections_by_owner u =
  lwt elections, tallied, archived =
    Web_persist.get_elections_by_owner u >>=
    Lwt_list.fold_left_s (fun accu uuid_s ->
        lwt w = find_election uuid_s in
        lwt state = Web_persist.get_election_state uuid_s in
        lwt date = Web_persist.get_election_date uuid_s in
        let elections, tallied, archived = accu in
        match state with
        | `Tallied _ -> return (elections, (date, w) :: tallied, archived)
        | `Archived -> return (elections, tallied, (date, w) :: archived)
        | _ -> return ((date, w) :: elections, tallied, archived)
    ) ([], [], [])
  in
  let sort l =
    List.sort (fun (x, _) (y, _) -> datetime_compare x y) l |>
    List.map (fun (_, x) -> x)
  in
  return (sort elections, sort tallied, sort archived)

let () = Html5.register ~service:admin
  (fun () () ->
    let cont () = Redirection.send admin in
    Eliom_reference.set Web_state.cont [cont] >>
    lwt site_user = Web_state.get_site_user () in
    lwt elections =
      match site_user with
      | None -> return None
      | Some u ->
         lwt elections, tallied, archived = get_finalized_elections_by_owner u in
         lwt setup_elections =
           Ocsipersist.fold_step (fun k v accu ->
             if v.se_owner = u
             then return ((uuid_of_string k, v.se_questions.t_name) :: accu)
             else return accu
           ) election_stable []
         in
         return @@ Some (elections, tallied, archived, setup_elections)
    in
    T.admin ~elections ()
  )

let () = File.register
  ~service:source_code
  ~content_type:"application/x-gzip"
  (fun () () -> return !source_file)

let do_get_randomness =
  let prng = Lazy.from_fun (Lwt_preemptive.detach (fun () ->
    pseudo_rng (random_string secure_rng 16)
  )) in
  let mutex = Lwt_mutex.create () in
  fun () ->
    Lwt_mutex.with_lock mutex (fun () ->
      lwt prng = Lazy.force prng in
      return (random_string prng 32)
    )

let b64_encode_compact x =
  Cryptokit.(transform_string (Base64.encode_compact ()) x)

let () = String.register
  ~service:get_randomness
  (fun () () ->
    lwt r = do_get_randomness () in
    b64_encode_compact r |>
    (fun x -> string_of_randomness { randomness=x }) |>
    (fun x -> return (x, "application/json"))
  )

let generate_uuid = Uuidm.v4_gen (Random.State.make_self_init ())

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
  let uuid = generate_uuid () in
  let uuid_s = Uuidm.to_string uuid in
  lwt token = generate_token () in
  let se_metadata = {
    e_owner = Some owner;
    e_auth_config;
    e_cred_authority;
    e_trustees = None;
  } in
  let question = {
    q_answers = [| "Answer 1"; "Answer 2"; "Blank" |];
    q_min = 1;
    q_max = 1;
    q_question = "Question 1?";
  } in
  let se_questions = {
    t_description = "Description of the election.";
    t_name = "Name of the election";
    t_questions = [| question |];
    t_short_name = "short_name";
  } in
  let se = {
    se_owner = owner;
    se_group = "{\"g\":\"14887492224963187634282421537186040801304008017743492304481737382571933937568724473847106029915040150784031882206090286938661464458896494215273989547889201144857352611058572236578734319505128042602372864570426550855201448111746579871811249114781674309062693442442368697449970648232621880001709535143047913661432883287150003429802392229361583608686643243349727791976247247948618930423866180410558458272606627111270040091203073580238905303994472202930783207472394578498507764703191288249547659899997131166130259700604433891232298182348403175947450284433411265966789131024573629546048637848902243503970966798589660808533\",\"p\":\"16328632084933010002384055033805457329601614771185955389739167309086214800406465799038583634953752941675645562182498120750264980492381375579367675648771293800310370964745767014243638518442553823973482995267304044326777047662957480269391322789378384619428596446446984694306187644767462460965622580087564339212631775817895958409016676398975671266179637898557687317076177218843233150695157881061257053019133078545928983562221396313169622475509818442661047018436264806901023966236718367204710755935899013750306107738002364137917426595737403871114187750804346564731250609196846638183903982387884578266136503697493474682071\",\"q\":\"61329566248342901292543872769978950870633559608669337131139375508370458778917\"}";
    se_voters = [];
    se_questions;
    se_public_keys = [];
    se_metadata;
    se_public_creds = token;
    se_public_creds_received = false;
  } in
  lwt () = Ocsipersist.add election_stable uuid_s se in
  lwt () = Ocsipersist.add election_credtokens token uuid_s in
  return (preapply election_setup uuid)

let () = Html5.register ~service:election_setup_pre
  (fun () () -> T.election_setup_pre ())

let () = Redirection.register ~service:election_setup_new
  (fun () (credmgmt, (auth, cas_server)) ->
   match_lwt Web_state.get_site_user () with
   | Some u ->
      lwt credmgmt = match credmgmt with
        | Some "auto" -> return `Automatic
        | Some "manual" -> return `Manual
        | _ -> fail_http 400
      in
      lwt auth = match auth with
        | Some "password" -> return `Password
        | Some "dummy" -> return `Dummy
        | Some "cas" -> return @@ `CAS cas_server
        | _ -> fail_http 400
      in
      create_new_election u credmgmt auth
   | None -> forbidden ())

let generic_setup_page f uuid () =
  match_lwt Web_state.get_site_user () with
  | Some u ->
     let uuid_s = Uuidm.to_string uuid in
     lwt se = Ocsipersist.find election_stable uuid_s in
     if se.se_owner = u
     then f uuid se ()
     else forbidden ()
  | None -> forbidden ()

let () = Html5.register ~service:election_setup
  (generic_setup_page T.election_setup)

let () = Html5.register ~service:election_setup_trustees
  (generic_setup_page T.election_setup_trustees)

let () = Html5.register ~service:election_setup_credential_authority
  (generic_setup_page T.election_setup_credential_authority)

let election_setup_mutex = Lwt_mutex.create ()

let handle_setup f uuid x =
  match_lwt Web_state.get_site_user () with
  | Some u ->
     let uuid_s = Uuidm.to_string uuid in
     Lwt_mutex.with_lock election_setup_mutex (fun () ->
       lwt se = Ocsipersist.find election_stable uuid_s in
       if se.se_owner = u then (
         try_lwt
           lwt cont = f se x u uuid in
           Ocsipersist.add election_stable uuid_s se >>
           cont ()
         with e ->
           let service = preapply election_setup uuid in
           T.generic_page ~title:"Error" ~service (Printexc.to_string e) () >>= Html5.send
       ) else forbidden ()
     )
  | None -> forbidden ()

let redir_preapply s u () = Redirection.send (preapply s u)

let () =
  Any.register
    ~service:election_setup_description
    (handle_setup
       (fun se (name, description) _ uuid ->
         se.se_questions <- {se.se_questions with
           t_name = name;
           t_description = description;
         };
         return (redir_preapply election_setup uuid)))

let generate_password title url id =
  let email, login = split_identity id in
  lwt salt = generate_token () in
  lwt password = generate_token () in
  let hashed = sha256_hex (salt ^ password) in
  let body = Mail_templates.password title login password url in
  let subject = "Your password for election " ^ title in
  send_email email subject body >>
  return (salt, hashed)

let handle_password se uuid ~force voters =
  let title = se.se_questions.t_name in
  let url = Eliom_uri.make_string_uri ~absolute:true ~service:election_home
    (uuid, ()) |> rewrite_prefix
  in
  Lwt_list.iter_s (fun id ->
    match id.sv_password with
    | Some _ when not force -> return_unit
    | None | Some _ ->
       lwt x = generate_password title url id.sv_id in
       return (id.sv_password <- Some x)
  ) voters >>
  return (fun () ->
    let service = preapply election_setup uuid in
    T.generic_page ~title:"Success" ~service
      "Passwords have been generated and mailed!" () >>= Html5.send)

let () =
  Any.register
    ~service:election_setup_auth_genpwd
    (handle_setup
       (fun se () _ uuid ->
         handle_password se uuid ~force:false se.se_voters))

let () =
  Any.register
    ~service:election_regenpwd
    (fun (uuid, ()) () ->
      T.regenpwd uuid () >>= Html5.send)

let () =
  Any.register
    ~service:election_regenpwd_post
    (fun (uuid, ()) user ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      lwt metadata = Web_persist.get_election_metadata uuid_s in
      let module W = (val w) in
      lwt site_user = Web_state.get_site_user () in
      match site_user with
      | Some u when metadata.e_owner = Some u ->
         let table = "password_" ^ underscorize uuid_s in
         let table = Ocsipersist.open_table table in
         let title = W.election.e_params.e_name in
         let url = Eliom_uri.make_string_uri
           ~absolute:true ~service:election_home
           (uuid, ()) |> rewrite_prefix
         in
         let service = preapply election_admin (uuid, ()) in
         begin try_lwt
           lwt _ = Ocsipersist.find table user in
           lwt x = generate_password title url user in
           Ocsipersist.add table user x >>
           dump_passwords (!spool_dir / uuid_s) table >>
           T.generic_page ~title:"Success" ~service
             ("A new password has been mailed to " ^ user ^ ".") ()
           >>= Html5.send
         with Not_found ->
           T.generic_page ~title:"Error" ~service
             (user ^ " is not a registered user for this election.") ()
           >>= Html5.send
         end
      | _ -> forbidden ()
    )

let () =
  Html5.register
    ~service:election_setup_questions
    (fun uuid () ->
     match_lwt Web_state.get_site_user () with
     | Some u ->
        let uuid_s = Uuidm.to_string uuid in
        lwt se = Ocsipersist.find election_stable uuid_s in
        if se.se_owner = u
        then T.election_setup_questions uuid se ()
        else forbidden ()
     | None -> forbidden ()
    )

let () =
  Any.register
    ~service:election_setup_questions_post
    (handle_setup
       (fun se x _ uuid ->
        se.se_questions <- template_of_string x;
         return (redir_preapply election_setup_questions uuid)))

let () =
  Html5.register
    ~service:election_setup_voters
    (fun uuid () ->
      match_lwt Web_state.get_site_user () with
      | Some u ->
         let uuid_s = Uuidm.to_string uuid in
         lwt se = Ocsipersist.find election_stable uuid_s in
         if se.se_owner = u
         then T.election_setup_voters uuid se ()
         else forbidden ()
      | None -> forbidden ()
    )

(* see http://www.regular-expressions.info/email.html *)
let identity_rex = Pcre.regexp
  ~flags:[`CASELESS]
  "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,7}(,[A-Z0-9._%+-]+)?$"

let is_identity x =
  try ignore (Pcre.pcre_exec ~rex:identity_rex x); true
  with Not_found -> false

let email_rex = Pcre.regexp
  ~flags:[`CASELESS]
  "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,7}$"

let is_email x =
  try ignore (Pcre.pcre_exec ~rex:email_rex x); true
  with Not_found -> false

module SSet = Set.Make (PString)

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
  Any.register
    ~service:election_setup_voters_add
    (handle_setup
       (fun se x _ uuid ->
         if se.se_public_creds_received then forbidden () else (
         let xs = Pcre.split x in
         let () =
           try
             let bad = List.find (fun x -> not (is_identity x)) xs in
             Printf.ksprintf failwith "%S is not a valid identity" bad
           with Not_found -> ()
         in
         se.se_voters <- merge_voters se.se_voters xs (fun _ -> None);
         return (redir_preapply election_setup_voters uuid))))

let () =
  Any.register
    ~service:election_setup_voters_remove
    (handle_setup
       (fun se voter _ uuid ->
         if se.se_public_creds_received then forbidden () else (
         se.se_voters <- List.filter (fun v ->
           v.sv_id <> voter
         ) se.se_voters;
         return (redir_preapply election_setup_voters uuid))))

let () =
  Any.register ~service:election_setup_voters_passwd
    (handle_setup
       (fun se voter _ uuid ->
         let voter = List.filter (fun v -> v.sv_id = voter) se.se_voters in
         handle_password se uuid ~force:true voter))

let () =
  Any.register
    ~service:election_setup_trustee_add
    (fun uuid st_id ->
     if is_email st_id then
     match_lwt Web_state.get_site_user () with
     | Some u ->
        let uuid_s = Uuidm.to_string uuid in
        Lwt_mutex.with_lock election_setup_mutex (fun () ->
          lwt se = Ocsipersist.find election_stable uuid_s in
          if se.se_owner = u
          then (
            lwt st_token = generate_token () in
            let trustee = {st_id; st_token; st_public_key = ""} in
            se.se_public_keys <- se.se_public_keys @ [trustee];
            Ocsipersist.add election_stable uuid_s se >>
            Ocsipersist.add election_pktokens st_token uuid_s
          ) else forbidden ()
        ) >>
        Redirection.send (preapply election_setup_trustees uuid)
     | None -> forbidden ()
     else
       let msg = st_id ^ " is not a valid e-mail address!" in
       let service = preapply election_setup_trustees uuid in
       T.generic_page ~title:"Error" ~service msg () >>= Html5.send
    )

let () =
  Redirection.register
    ~service:election_setup_trustee_del
    (fun uuid index ->
     match_lwt Web_state.get_site_user () with
     | Some u ->
        let uuid_s = Uuidm.to_string uuid in
        Lwt_mutex.with_lock election_setup_mutex (fun () ->
          lwt se = Ocsipersist.find election_stable uuid_s in
          if se.se_owner = u
          then (
            let trustees, old =
              se.se_public_keys |>
              List.mapi (fun i x -> i, x) |>
              List.partition (fun (i, _) -> i <> index) |>
              (fun (x, y) -> List.map snd x, List.map snd y)
            in
            se.se_public_keys <- trustees;
            Ocsipersist.add election_stable uuid_s se >>
            Lwt_list.iter_s (fun {st_token; _} ->
              Ocsipersist.remove election_pktokens st_token
            ) old
          ) else forbidden ()
        ) >>
        return (preapply election_setup_trustees uuid)
     | None -> forbidden ()
    )

let () =
  Html5.register
    ~service:election_setup_credentials
    (fun token () ->
     lwt uuid = Ocsipersist.find election_credtokens token in
     lwt se = Ocsipersist.find election_stable uuid in
     T.election_setup_credentials token uuid se ()
    )

let () =
  File.register
    ~service:election_setup_credentials_download
    ~content_type:"text/plain"
    (fun token () ->
     lwt uuid = Ocsipersist.find election_credtokens token in
     return (!spool_dir / uuid ^ ".public_creds.txt")
    )

let wrap_handler f =
  try_lwt f ()
  with
  | e -> T.generic_page ~title:"Error" ~service:home (Printexc.to_string e) () >>= Html5.send

let handle_credentials_post token creds =
  lwt uuid = Ocsipersist.find election_credtokens token in
  lwt se = Ocsipersist.find election_stable uuid in
  if se.se_public_creds_received then forbidden () else
  let module G = (val Group.of_string se.se_group : GROUP) in
  let fname = !spool_dir / uuid ^ ".public_creds.txt" in
  Lwt_mutex.with_lock
    election_setup_mutex
    (fun () ->
     Lwt_io.with_file
       ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
       ~perm:0o600 ~mode:Lwt_io.Output fname
       (fun oc -> Lwt_io.write_chars oc creds)
    ) >>
  lwt () =
    let i = ref 1 in
    Lwt_stream.iter
      (fun x ->
       try
         let x = G.of_string x in
         if not (G.check x) then raise Exit;
         incr i
       with _ ->
         Printf.ksprintf failwith "invalid credential at line %d" !i)
      (Lwt_io.lines_of_file fname)
  in
  let () = se.se_metadata <- {se.se_metadata with e_cred_authority = None} in
  let () = se.se_public_creds_received <- true in
  Ocsipersist.add election_stable uuid se >>
  T.generic_page ~title:"Success" ~service:home
    "Credentials have been received and checked!" () >>= Html5.send

let () =
  Any.register
    ~service:election_setup_credentials_post
    (fun token creds ->
     let s = Lwt_stream.of_string creds in
     wrap_handler (fun () -> handle_credentials_post token s))

let () =
  Any.register
    ~service:election_setup_credentials_post_file
    (fun token creds ->
     let s = Lwt_io.chars_of_file creds.Ocsigen_extensions.tmp_filename in
     wrap_handler (fun () -> handle_credentials_post token s))

module CG = Credential.MakeGenerate (LwtRandom)

let () =
  Any.register
    ~service:election_setup_credentials_server
    (handle_setup (fun se () _ uuid ->
      if se.se_public_creds_received then forbidden () else
      let () = se.se_metadata <- {se.se_metadata with
        e_cred_authority = Some "server"
      } in
      let uuid_s = Uuidm.to_string uuid in
      let title = se.se_questions.t_name in
      let url = Eliom_uri.make_string_uri
        ~absolute:true ~service:election_home
        (uuid, ()) |> rewrite_prefix
      in
      let module S = Set.Make (PString) in
      let module G = (val Group.of_string se.se_group : GROUP) in
      let module CD = Credential.MakeDerive (G) in
      lwt creds =
        Lwt_list.fold_left_s (fun accu v ->
          let email, login = split_identity v.sv_id in
          lwt cred = CG.generate () in
          let pub_cred =
            let x = CD.derive uuid cred in
            let y = G.(g **~ x) in
            G.to_string y
          in
          let body = Mail_templates.credential title login cred url in
          let subject = "Your credential for election " ^ title in
          lwt () = send_email email subject body in
          return @@ S.add pub_cred accu
        ) S.empty se.se_voters
      in
      let creds = S.elements creds in
      let fname = !spool_dir / uuid_s ^ ".public_creds.txt" in
      lwt () =
          Lwt_io.with_file
            ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
            ~perm:0o600 ~mode:Lwt_io.Output fname
            (fun oc ->
              Lwt_list.iter_s (Lwt_io.write_line oc) creds)
      in
      se.se_public_creds_received <- true;
      return (fun () ->
        let service = preapply election_setup uuid in
        T.generic_page ~title:"Success" ~service
          "Credentials have been generated and mailed!" () >>= Html5.send)))

let () =
  Html5.register
    ~service:election_setup_trustee
    (fun token () ->
     lwt uuid = Ocsipersist.find election_pktokens token in
     lwt se = Ocsipersist.find election_stable uuid in
     T.election_setup_trustee token se ()
    )

let () =
  Any.register
    ~service:election_setup_trustee_post
    (fun token public_key ->
     wrap_handler
       (fun () ->
        lwt uuid = Ocsipersist.find election_pktokens token in
        Lwt_mutex.with_lock
          election_setup_mutex
          (fun () ->
           lwt se = Ocsipersist.find election_stable uuid in
           let t = List.find (fun x -> token = x.st_token) se.se_public_keys in
           let module G = (val Group.of_string se.se_group : GROUP) in
           let pk = trustee_public_key_of_string G.read public_key in
           let module KG = Election.MakeSimpleDistKeyGen (G) (LwtRandom) in
           if not (KG.check pk) then failwith "invalid public key";
           (* we keep pk as a string because of G.t *)
           t.st_public_key <- public_key;
           Ocsipersist.add election_stable uuid se
          ) >> T.generic_page ~title:"Success" ~service:home
            "Your key has been received and checked!"
            () >>= Html5.send
       )
    )

let () =
  Any.register
    ~service:election_setup_create
    (fun uuid () ->
     match_lwt Web_state.get_site_user () with
     | None -> forbidden ()
     | Some u ->
        begin try_lwt
          let uuid_s = Uuidm.to_string uuid in
          Lwt_mutex.with_lock election_setup_mutex (fun () ->
            lwt se = Ocsipersist.find election_stable uuid_s in
            if se.se_owner <> u then forbidden () else
            finalize_election uuid se >>
            Redirection.send (preapply election_admin (uuid, ()))
          )
        with e ->
          T.new_election_failure (`Exception e) () >>= Html5.send
        end
    )

let () =
  Html5.register
    ~service:election_setup_import
    (fun uuid () ->
      lwt site_user = Web_state.get_site_user () in
      match site_user with
      | None -> forbidden ()
      | Some u ->
         lwt se = Ocsipersist.find election_stable (Uuidm.to_string uuid) in
         lwt elections = get_finalized_elections_by_owner u in
         T.election_setup_import uuid se elections ())

let () =
  Any.register
    ~service:election_setup_import_post
    (handle_setup
       (fun se from _ uuid ->
         let from_s = Uuidm.to_string from in
         lwt voters = Web_persist.get_voters from_s in
         lwt passwords = Web_persist.get_passwords from_s in
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
            if se.se_public_creds_received then forbidden () else (
              se.se_voters <- merge_voters se.se_voters voters get_password;
              return (redir_preapply election_setup_voters uuid))
         | None ->
            return (fun () -> T.generic_page ~title:"Error"
              ~service:(preapply election_setup_voters uuid)
              (Printf.sprintf
                 "Could not retrieve voter list from election %s"
                 from_s)
              () >>= Html5.send)))


let () =
  Any.register
    ~service:election_home
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      try_lwt
        lwt w = find_election uuid_s in
        let module W = (val w) in
        Eliom_reference.unset Web_state.ballot >>
        let cont () =
          Redirection.send
            (Eliom_service.preapply
               election_home (W.election.e_params.e_uuid, ()))
        in
        Eliom_reference.set Web_state.cont [cont] >>
        match_lwt Eliom_reference.get Web_state.cast_confirmed with
        | Some result ->
           Eliom_reference.unset Web_state.cast_confirmed >>
           T.cast_confirmed (module W) ~result () >>= Html5.send
        | None ->
           lwt state = Web_persist.get_election_state uuid_s in
           T.election_home (module W) state () >>= Html5.send
      with Not_found ->
        T.generic_page ~title:"Sorry, this election is not yet open"
          ~service:(preapply election_home (uuid, ()))
          "This election does not exist yet. Please come back later." ()
          >>= Html5.send)

let () =
  Any.register
    ~service:election_admin
    (fun (uuid, ()) () ->
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt metadata = Web_persist.get_election_metadata uuid_s in
     lwt site_user = Web_state.get_site_user () in
     let module W = (val w) in
     match site_user with
     | Some u when metadata.e_owner = Some u ->
        lwt state = Web_persist.get_election_state uuid_s in
        T.election_admin w metadata state () >>= Html5.send
     | _ ->
        let cont () =
          Redirection.send (Eliom_service.preapply election_admin (uuid, ()))
        in
        Eliom_reference.set Web_state.cont [cont] >>
        Redirection.send (Eliom_service.preapply site_login None)
    )

let election_set_state state (uuid, ()) () =
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt metadata = Web_persist.get_election_metadata uuid_s in
     let module W = (val w) in
     lwt () =
       match_lwt Web_state.get_site_user () with
       | Some u when metadata.e_owner = Some u -> return ()
       | _ -> forbidden ()
     in
     lwt () =
       match_lwt Web_persist.get_election_state uuid_s with
       | `Open | `Closed -> return ()
       | _ -> forbidden ()
     in
     let state = if state then `Open else `Closed in
     Web_persist.set_election_state uuid_s state >>
     Redirection.send (preapply election_admin (uuid, ()))

let () = Any.register ~service:election_open (election_set_state true)
let () = Any.register ~service:election_close (election_set_state false)

let () = Any.register ~service:election_archive (fun (uuid, ()) () ->
  let uuid_s = Uuidm.to_string uuid in
  lwt w = find_election uuid_s in
  lwt metadata = Web_persist.get_election_metadata uuid_s in
  lwt site_user = Web_state.get_site_user () in
  let module W = (val w) in
  match site_user with
  | Some u when metadata.e_owner = Some u ->
     archive_election uuid_s >>
     Redirection.send (Eliom_service.preapply election_admin (uuid, ()))
  | _ -> forbidden ()
)

let () =
  Any.register
    ~service:election_update_credential
    (fun (uuid, ()) () ->
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt metadata = Web_persist.get_election_metadata uuid_s in
     lwt site_user = Web_state.get_site_user () in
     let module W = (val w) in
     match site_user with
     | Some u ->
        if metadata.e_owner = Some u then (
          T.update_credential (module W) () >>= Html5.send
        ) else (
          forbidden ()
        )
     | _ -> forbidden ())

let () =
  Any.register
    ~service:election_update_credential_post
    (fun (uuid, ()) (old, new_) ->
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt metadata = Web_persist.get_election_metadata uuid_s in
     let module W = (val w) in
     lwt site_user = Web_state.get_site_user () in
     let module WE = Web_election.Make (W) (LwtRandom) in
     match site_user with
     | Some u ->
       if metadata.e_owner = Some u then (
         try_lwt
           WE.B.update_cred ~old ~new_ >>
           String.send ("OK", "text/plain")
         with Error e ->
           String.send ("Error: " ^ explain_error e, "text/plain")
       ) >>= (fun x -> return @@ cast_unknown_content_kind x)
       else (
         forbidden ()
       )
     | _ -> forbidden ())

let () =
  Any.register
    ~service:election_vote
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      Eliom_reference.unset Web_state.ballot >>
      Redirection.send
        (Eliom_service.preapply
           (Eliom_service.static_dir_with_params
              ~get_params:(Eliom_parameter.string "election_url") ())
           (["static"; "vote.html"],
            "../elections/" ^ uuid_s ^ "/")))

let () =
  Any.register
    ~service:election_cast
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      let cont () =
        Redirection.send
          (Eliom_service.preapply
             election_cast (W.election.e_params.e_uuid, ()))
      in
      Eliom_reference.set Web_state.cont [cont] >>
      match_lwt Eliom_reference.get Web_state.ballot with
      | Some b -> T.cast_confirmation (module W) (sha256_b64 b) () >>= Html5.send
      | None -> T.cast_raw (module W) () >>= Html5.send)

let () =
  Any.register
    ~service:election_cast_post
    (fun (uuid, ()) (ballot_raw, ballot_file) ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      lwt user = Web_state.get_election_user uuid in
      lwt the_ballot = match ballot_raw, ballot_file with
        | Some ballot, None -> return ballot
        | None, Some fi ->
           let fname = fi.Ocsigen_extensions.tmp_filename in
           Lwt_stream.to_string (Lwt_io.chars_of_file fname)
        | _, _ -> fail_http 400
      in
      let cont () =
        Redirection.send
          (Eliom_service.preapply
             Web_services.election_cast (W.election.e_params.e_uuid, ()))
      in
      Eliom_reference.set Web_state.cont [cont] >>
      Eliom_reference.set Web_state.ballot (Some the_ballot) >>
      match user with
      | None ->
         Redirection.send
           (Eliom_service.preapply
              Web_services.election_login
              ((W.election.e_params.e_uuid, ()), None))
      | Some _ -> cont ())

let () =
  Any.register
    ~service:election_cast_confirm
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      let module WE = Web_election.Make (W) (LwtRandom) in
      match_lwt Eliom_reference.get Web_state.ballot with
      | Some the_ballot ->
         begin
           Eliom_reference.unset Web_state.ballot >>
           match_lwt Web_state.get_election_user uuid with
           | Some u ->
              let record = u, now () in
              lwt result =
                try_lwt
                  lwt hash = WE.B.cast the_ballot record in
                  return (`Valid hash)
                with Error e -> return (`Error e)
              in
              Eliom_reference.set Web_state.cast_confirmed (Some result) >>
              Redirection.send
                (Eliom_service.preapply
                   election_home (W.election.e_params.e_uuid, ()))
           | None -> forbidden ()
         end
      | None -> fail_http 404)

let () =
  Any.register
    ~service:election_pretty_ballots
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      lwt ballots = Web_persist.get_ballot_hashes uuid_s in
      lwt result = Web_persist.get_election_result uuid_s in
      T.pretty_ballots w ballots result () >>= Html5.send)

let () =
  Any.register
    ~service:election_pretty_ballot
    (fun ((uuid, ()), hash) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt ballot = Web_persist.get_ballot_by_hash ~uuid:uuid_s ~hash in
      match ballot with
      | None -> fail_http 404
      | Some b ->
         String.send (b, "application/json") >>=
           (fun x -> return @@ cast_unknown_content_kind x))

let () =
  let rex = Pcre.regexp "\".*\" \".*:(.*)\"" in
  Any.register
    ~service:election_missing_voters
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      lwt metadata = Web_persist.get_election_metadata uuid_s in
      let module W = (val w) in
      lwt () =
        match_lwt Web_state.get_site_user () with
        | Some u when metadata.e_owner = Some u -> return ()
        | _ -> forbidden ()
      in
      let voters = Lwt_io.lines_of_file
        (!spool_dir / uuid_s / string_of_election_file ESVoters)
      in
      let module S = Set.Make (PString) in
      lwt voters = Lwt_stream.fold (fun v accu ->
        let _, login = split_identity v in
        S.add login accu
      ) voters S.empty in
      let records = Lwt_io.lines_of_file
        (!spool_dir / uuid_s / string_of_election_file ESRecords)
      in
      lwt voters = Lwt_stream.fold (fun r accu ->
        let s = Pcre.exec ~rex r in
        let v = Pcre.get_substring s 1 in
        S.remove v accu
      ) records voters in
      let buf = Buffer.create 128 in
      S.iter (fun v ->
        Buffer.add_string buf v;
        Buffer.add_char buf '\n'
      ) voters;
      String.send (Buffer.contents buf, "text/plain"))

let () =
  let rex = Pcre.regexp "\"(.*)\\..*\" \".*:(.*)\"" in
  Any.register ~service:election_pretty_records
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      lwt metadata = Web_persist.get_election_metadata uuid_s in
      let module W = (val w) in
      lwt () =
        match_lwt Web_state.get_site_user () with
        | Some u when metadata.e_owner = Some u -> return_unit
        | _ -> forbidden ()
      in
      let records = Lwt_io.lines_of_file
        (!spool_dir / uuid_s / string_of_election_file ESRecords)
      in
      lwt records = Lwt_stream.fold (fun r accu ->
        let s = Pcre.exec ~rex r in
        let date = Pcre.get_substring s 1 in
        let voter = Pcre.get_substring s 2 in
        (date, voter) :: accu
      ) records [] in
      T.pretty_records w (List.rev records) () >>= Html5.send
    )

let () =
  Any.register
    ~service:election_tally_trustees
    (fun (uuid, ((), trustee_id)) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      lwt () =
        match_lwt Web_persist.get_election_state uuid_s with
        | `EncryptedTally _ -> return ()
        | _ -> fail_http 404
      in
      lwt pds = Web_persist.get_partial_decryptions uuid_s in
      if List.mem_assoc trustee_id pds then (
        T.generic_page ~title:"Error"
          ~service:(preapply election_home (uuid, ()))
          "Your partial decryption has already been received and checked!"
          () >>= Html5.send
      ) else (
        T.tally_trustees (module W) trustee_id () >>= Html5.send
      ))

let () =
  Any.register
    ~service:election_tally_trustees_post
    (fun (uuid, ((), trustee_id)) partial_decryption ->
      let uuid_s = Uuidm.to_string uuid in
      lwt () =
        match_lwt Web_persist.get_election_state uuid_s with
        | `EncryptedTally _ -> return ()
        | _ -> forbidden ()
      in
      lwt pds = Web_persist.get_partial_decryptions uuid_s in
      lwt () =
        if List.mem_assoc trustee_id pds then forbidden () else return ()
      in
      lwt () =
        if trustee_id > 0 then return () else fail_http 404
      in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      let module E = Election.MakeElection (W.G) (LwtRandom) in
      let pks = !spool_dir / uuid_s / string_of_election_file ESKeys in
      let pks = Lwt_io.lines_of_file pks in
      lwt () = Lwt_stream.njunk (trustee_id-1) pks in
      lwt pk = Lwt_stream.peek pks in
      lwt () = Lwt_stream.junk_while (fun _ -> true) pks in
      lwt pk =
        match pk with
        | None -> fail_http 404
        | Some x -> return x
      in
      let pk = trustee_public_key_of_string W.G.read pk in
      let pk = pk.trustee_public_key in
      let pd = partial_decryption_of_string W.G.read partial_decryption in
      let et = !spool_dir / uuid_s / string_of_election_file ESETally in
      lwt et = Lwt_io.chars_of_file et |> Lwt_stream.to_string in
      let et = encrypted_tally_of_string W.G.read et in
      if E.check_factor et pk pd then (
        let pds = (trustee_id, partial_decryption) :: pds in
        lwt () = Web_persist.set_partial_decryptions uuid_s pds in
        let service = preapply election_home (uuid, ()) in
        T.generic_page ~title:"Success" ~service
          "Your partial decryption has been received and checked!" () >>=
        Html5.send
      ) else (
        let service = preapply election_tally_trustees (uuid, ((), trustee_id)) in
        T.generic_page ~title:"Error" ~service
          "The partial decryption didn't pass validation!" () >>=
        Html5.send
      ))

let handle_election_tally_release (uuid, ()) () =
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      lwt metadata = Web_persist.get_election_metadata uuid_s in
      let module W = (val w) in
      let module E = Election.MakeElection (W.G) (LwtRandom) in
      lwt () =
        match_lwt Web_state.get_site_user () with
        | Some u when metadata.e_owner = Some u -> return ()
        | _ -> forbidden ()
      in
      lwt npks, ntallied =
        match_lwt Web_persist.get_election_state uuid_s with
        | `EncryptedTally (npks, ntallied, _) -> return (npks, ntallied)
        | _ -> forbidden ()
      in
      lwt pds = Web_persist.get_partial_decryptions uuid_s in
      lwt pds =
        try
          return @@ Array.init npks (fun i ->
            List.assoc (i+1) pds |> partial_decryption_of_string W.G.read
          )
        with Not_found -> fail_http 404
      in
      lwt et =
        !spool_dir / uuid_s / string_of_election_file ESETally |>
        Lwt_io.chars_of_file |> Lwt_stream.to_string >>=
        wrap1 (encrypted_tally_of_string W.G.read)
      in
      let result = E.combine_factors ntallied et pds in
      lwt () =
        let open Lwt_io in
        with_file
          ~mode:Output (!spool_dir / uuid_s / string_of_election_file ESResult)
          (fun oc -> Lwt_io.write_line oc (string_of_result W.G.write result))
      in
      lwt () = Web_persist.set_election_state uuid_s (`Tallied result.result) in
      Eliom_service.preapply
        election_home (W.election.e_params.e_uuid, ()) |>
      Redirection.send

let () =
  Any.register
    ~service:election_tally_release
    handle_election_tally_release

let content_type_of_file = function
  | ESRaw | ESKeys | ESBallots | ESETally | ESResult -> "application/json"
  | ESCreds | ESRecords | ESVoters -> "text/plain"

let handle_pseudo_file uuid_s w f site_user =
  let module W = (val w : ELECTION_DATA) in
  let confidential =
    match f with
    | ESRaw | ESKeys | ESBallots | ESETally | ESResult | ESCreds -> false
    | ESRecords | ESVoters -> true
  in
  lwt () =
    if confidential then (
      lwt metadata = Web_persist.get_election_metadata uuid_s in
      match site_user with
      | Some u when metadata.e_owner = Some u -> return ()
      | _ -> forbidden ()
    ) else return ()
  in
  let content_type = content_type_of_file f in
  File.send ~content_type (!spool_dir / uuid_s / string_of_election_file f)

let () =
  Any.register
    ~service:election_dir
    (fun (uuid, f) () ->
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt site_user = Web_state.get_site_user () in
     let module W = (val w) in
     handle_pseudo_file uuid_s w f site_user)

let () =
  Any.register
    ~service:election_compute_encrypted_tally
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      lwt metadata = Web_persist.get_election_metadata uuid_s in
      let module W = (val w) in
      let module WE = Web_election.Make (W) (LwtRandom) in
      lwt () =
        match_lwt Web_state.get_site_user () with
        | Some u when metadata.e_owner = Some u -> return ()
        | _ -> forbidden ()
      in
      lwt () =
        match_lwt Web_persist.get_election_state uuid_s with
        | `Closed -> return ()
        | _ -> forbidden ()
      in
      lwt nb, hash, tally = WE.B.compute_encrypted_tally () in
      let pks = !spool_dir / uuid_s / string_of_election_file ESKeys in
      let pks = Lwt_io.lines_of_file pks in
      let npks = ref 0 in
      lwt () = Lwt_stream.junk_while (fun _ -> incr npks; true) pks in
      Web_persist.set_election_state uuid_s (`EncryptedTally (!npks, nb, hash)) >>
      (* compute partial decryption and release tally
         if the (single) key is known *)
      let skfile = !spool_dir / uuid_s / "private_key.json" in
      if !npks = 1 && Sys.file_exists skfile then (
        lwt sk = Lwt_io.lines_of_file skfile |> Lwt_stream.to_list in
        let sk = match sk with
          | [sk] -> number_of_string sk
          | _ -> failwith "several private keys are available"
        in
        let tally = encrypted_tally_of_string WE.G.read tally in
        lwt pd = WE.E.compute_factor tally sk in
        let pd = string_of_partial_decryption WE.G.write pd in
        Web_persist.set_partial_decryptions uuid_s [1, pd] >>
        handle_election_tally_release (uuid, ()) ()
      ) else Redirection.send (preapply election_admin (uuid, ())))
