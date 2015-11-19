(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
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
open Web_serializable_j
open Web_common
open Web_signatures
open Web_services

let source_file = ref "belenios.tar.gz"
let spool_dir = ref "."

let get_single_line x =
  match_lwt Lwt_stream.get x with
  | None -> return None
  | Some _ as line ->
    lwt b = Lwt_stream.is_empty x in
    if b then (
      return line
    ) else (
      Lwt_stream.junk_while (fun _ -> true) x >>
      return None
    )

let ( / ) = Filename.concat

let delete_shallow_directory dir =
  lwt () =
    Lwt_unix.files_of_directory dir |>
    Lwt_stream.filter (fun x -> x <> "." && x <> "..") |>
    Lwt_stream.iter_s (fun x -> Lwt_unix.unlink (dir/x))
  in
  Lwt_unix.rmdir dir

module PString = String

open Eliom_service
open Eliom_registration

module LwtRandom = MakeLwtRandom (struct let rng = make_rng () end)

(* Persistent table, used to initialize the server. *)
let election_ptable = Ocsipersist.open_table "site_elections"

(* Table with elections in setup mode. *)
let election_stable = Ocsipersist.open_table "site_setup"

(* Table with tokens given to trustees. *)
let election_pktokens = Ocsipersist.open_table "site_pktokens"

(* Table with tokens given to credential authorities. *)
let election_credtokens = Ocsipersist.open_table "site_credtokens"

module T = Web_templates

let web_election_data (raw_election, web_params) =
  let params = Group.election_params_of_string raw_election in
  let module P = (val params : ELECTION_PARAMS) in
  let module D = struct
    module G = P.G
    let election = {
      e_params = P.params;
      e_pks = None;
      e_fingerprint = P.fingerprint;
    }
    include (val web_params : WEB_PARAMS)
  end in
  (module D : WEB_ELECTION_DATA)

let find_election uuid =
  lwt x = Ocsipersist.find election_ptable uuid in
  return (web_election_data x)

(* Mutex to avoid simultaneous registrations of the same election *)
let registration_mutex = Lwt_mutex.create ()

let import_election f =
  Lwt_mutex.lock registration_mutex >>
  try_lwt
    lwt raw_election =
      Lwt_io.lines_of_file f.f_election |>
      get_single_line >>=
      (function
      | Some e -> return e
      | None -> Printf.ksprintf
        failwith "election.json must contain a single line"
      )
    in
    let params = Group.election_params_of_string raw_election in
    let module P = (val params : ELECTION_PARAMS) in
    let uuid = Uuidm.to_string P.params.e_uuid in
    lwt exists =
      try_lwt
        lwt _ = Ocsipersist.find election_ptable uuid in
        return true
      with Not_found -> return false
    in
    if exists then (
      Lwt_mutex.unlock registration_mutex;
      return None
    ) else (
      let dir = !spool_dir/uuid in
      lwt metadata =
        Lwt_io.chars_of_file f.f_metadata |>
        Lwt_stream.to_string >>=
        wrap1 metadata_of_string
      in
      let module X = struct
        let metadata = metadata
        let dir = dir
      end in
      let web_params = (module X : WEB_PARAMS) in
      let module G = P.G in
      let module KG = Election.MakeSimpleDistKeyGen (G) (LwtRandom) in
      let public_keys = Lwt_io.lines_of_file f.f_public_keys in
      let voters = Lwt_io.lines_of_file f.f_voters in
      lwt () =
        match_lwt Lwt_stream.peek voters with
        | Some _ -> return_unit
        | None -> Lwt.fail (Failure "No voters")
      in
      lwt () =
        match metadata.e_auth_config with
        | Some [x] ->
           if x.auth_system = "password" then
             let table = "password_" ^ underscorize uuid in
             let table = Ocsipersist.open_table table in
             lwt n = Ocsipersist.length table in
             if n = 0 then Lwt.fail (Failure "No passwords")
             else return_unit
           else return_unit
        | _ -> return_unit
      in
      lwt pks = Lwt_stream.(
        clone public_keys |>
        map (trustee_public_key_of_string G.read) |>
        to_list >>= wrap1 Array.of_list
      ) in
      if not (Array.forall KG.check pks) then
        failwith "Public keys are invalid.";
      if not G.(P.params.e_public_key =~ KG.combine pks) then
        failwith "Public keys mismatch with election public key.";
      let public_creds = Lwt_io.lines_of_file f.f_public_creds in
      lwt () = Lwt_stream.(
        clone public_creds |>
        iter_s (fun x ->
          if not G.(check @@ of_string x) then (
            Lwt.fail @@ Failure "Public credentials are invalid."
          ) else return ()
        )
      ) in
      let module R = struct
        let register () =
          try_lwt
            Lwt_unix.mkdir dir 0o700 >>
            Lwt_io.(with_file Output (dir/"election.json") (fun oc ->
              write_line oc raw_election
            )) >>
            Lwt_io.(with_file Output (dir/"public_keys.jsons") (fun oc ->
              write_lines oc public_keys
            )) >>
            Lwt_io.(with_file Output (dir/"voters.txt") (fun oc ->
              write_lines oc voters
            )) >>
            let election = web_election_data (raw_election, web_params) in
            let module W = Web_election.Make ((val election)) (LwtRandom) in
            lwt () =
              match W.D.metadata.e_auth_config with
              | None -> return ()
              | Some xs ->
                 let auth_config =
                   List.map (fun {auth_system; auth_instance; auth_config} ->
                     auth_instance, (auth_system, List.map snd auth_config)
                   ) xs
                 in
                 Web_persist.set_auth_config uuid auth_config
            in
            let () =
              Ocsigen_messages.debug (fun () ->
                Printf.sprintf "Injecting credentials for %s" uuid
              )
            in
            public_creds |>
            Lwt_stream.iter_s W.B.inject_cred >>
            W.B.update_files () >>
            Ocsipersist.add election_ptable uuid (raw_election, web_params) >>
            let () = Lwt_mutex.unlock registration_mutex in
            return (module W : WEB_ELECTION)
          with e ->
            lwt () =
              try_lwt delete_shallow_directory dir
              with e ->
                Printf.ksprintf
                  (fun s ->
                   return (Ocsigen_messages.unexpected_exception e s))
                  "error while deleting %s after failure of %s"
                  dir uuid
            in
            Lwt_mutex.unlock registration_mutex;
            Lwt.fail e
      end in
      (* until here, no side-effects on the running server *)
      return @@ Some (module R : REGISTRABLE_ELECTION)
    )
  with e ->
    Lwt_mutex.unlock registration_mutex;
    Lwt.fail e

let () = Any.register ~service:home
  (fun () () ->
    Eliom_reference.unset Web_auth_state.cont >>
    T.home () >>= Html5.send
  )

let () = Html5.register ~service:admin
  (fun () () ->
    let cont () = Redirection.send admin in
    Eliom_reference.set Web_auth_state.cont [cont] >>
    lwt site_user = Web_auth_state.get_site_user () in
    lwt elections =
      match site_user with
      | None -> return None
      | Some u ->
         lwt elections, tallied =
           Ocsipersist.fold_step (fun uuid_s (_, web_params) accu ->
             let module W = (val web_params : WEB_PARAMS) in
             if W.metadata.e_owner = Some u then (
               lwt w = find_election uuid_s in
               lwt state = Web_persist.get_election_state uuid_s in
               lwt date = Web_persist.get_election_date uuid_s in
               let elections, tallied = accu in
               match state with
               | `Tallied _ -> return (elections, (date, w) :: tallied)
               | _ -> return ((date, w) :: elections, tallied)
             ) else (
               return accu
             )
           ) election_ptable ([], [])
         and setup_elections =
           Ocsipersist.fold_step (fun k v accu ->
             if v.se_owner = u
             then return ((uuid_of_string k, v.se_questions.t_name) :: accu)
             else return accu
           ) election_stable []
         in
         let sort l =
           List.sort (fun (x, _) (y, _) -> datetime_compare x y) l |>
           List.map (fun (_, x) -> x)
         in
         let elections = sort elections and tallied = sort tallied in
         return @@ Some (elections, tallied, setup_elections)
    in
    T.admin ~elections ()
  )

let () = File.register
  ~service:source_code
  ~content_type:"application/x-gzip"
  (fun () () -> return !source_file)

let do_get_randomness =
  let prng = Lazy.lazy_from_fun (Lwt_preemptive.detach (fun () ->
    pseudo_rng (random_string secure_rng 16)
  )) in
  let mutex = Lwt_mutex.create () in
  fun () ->
    Lwt_mutex.with_lock mutex (fun () ->
      lwt prng = Lazy.force prng in
      return (random_string prng 32)
    )

let () = String.register
  ~service:get_randomness
  (fun () () ->
    lwt r = do_get_randomness () in
    b64_encode_compact r |>
    (fun x -> string_of_randomness { randomness=x }) |>
    (fun x -> return (x, "application/json"))
  )

let generate_uuid = Uuidm.v4_gen (Random.State.make_self_init ())

let () = Redirection.register ~service:election_setup_new
  (fun () () ->
   match_lwt Web_auth_state.get_site_user () with
   | Some u ->
      let uuid = generate_uuid () in
      let uuid_s = Uuidm.to_string uuid in
      lwt token = generate_token () in
      let se_metadata = {
        e_owner = Some u;
        e_auth_config = Some [{auth_system = "password"; auth_instance = "password"; auth_config = []}];
        e_cred_authority = None;
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
        se_owner = u;
        se_group = "{\"g\":\"14887492224963187634282421537186040801304008017743492304481737382571933937568724473847106029915040150784031882206090286938661464458896494215273989547889201144857352611058572236578734319505128042602372864570426550855201448111746579871811249114781674309062693442442368697449970648232621880001709535143047913661432883287150003429802392229361583608686643243349727791976247247948618930423866180410558458272606627111270040091203073580238905303994472202930783207472394578498507764703191288249547659899997131166130259700604433891232298182348403175947450284433411265966789131024573629546048637848902243503970966798589660808533\",\"p\":\"16328632084933010002384055033805457329601614771185955389739167309086214800406465799038583634953752941675645562182498120750264980492381375579367675648771293800310370964745767014243638518442553823973482995267304044326777047662957480269391322789378384619428596446446984694306187644767462460965622580087564339212631775817895958409016676398975671266179637898557687317076177218843233150695157881061257053019133078545928983562221396313169622475509818442661047018436264806901023966236718367204710755935899013750306107738002364137917426595737403871114187750804346564731250609196846638183903982387884578266136503697493474682071\",\"q\":\"61329566248342901292543872769978950870633559608669337131139375508370458778917\"}";
        se_voters = [];
        se_questions;
        se_public_keys = [];
        se_metadata;
        se_public_creds = token;
      } in
      lwt () = Ocsipersist.add election_stable uuid_s se in
      lwt () = Ocsipersist.add election_credtokens token uuid_s in
      return (preapply election_setup uuid)
  | None -> forbidden ()
  )

let generic_setup_page f uuid () =
  match_lwt Web_auth_state.get_site_user () with
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
  match_lwt Web_auth_state.get_site_user () with
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
           T.generic_page ~title:"Error" (Printexc.to_string e) () >>= Html5.send
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

let () =
  Any.register
    ~service:election_setup_group
    (handle_setup
       (fun se x _ uuid ->
        let _group = Group.of_string x in
        (* we keep it as a string since it contains a type *)
        se.se_group <- x;
        return (redir_preapply election_setup uuid)))

let () =
  Any.register
    ~service:election_setup_metadata
    (handle_setup
       (fun se x u uuid ->
        let metadata = metadata_of_string x in
        if metadata.e_owner <> Some u then failwith "wrong owner";
        se.se_metadata <- metadata;
        return (redir_preapply election_setup uuid)))

let () =
  Any.register
    ~service:election_setup_auth
    (handle_setup
       (fun se auth_system _ uuid ->
         (match auth_system with
         | Some (("dummy" | "password") as auth_system) ->
            se.se_metadata <- {
              se.se_metadata with
                e_auth_config = Some [{
                  auth_system;
                  auth_instance = auth_system;
                  auth_config = []
                }]
            }
         | Some "cas" ->
            se.se_metadata <- {
              se.se_metadata with
                e_auth_config = Some [{
                  auth_system = "cas";
                  auth_instance = "cas";
                  auth_config = ["server", ""];
                }]
            }
         | Some x -> failwith ("unknown authentication system: "^x)
         | None -> failwith "no authentication system was given");
         return (redir_preapply election_setup uuid)))

let () =
  Any.register
    ~service:election_setup_auth_cas
    (handle_setup
       (fun se server _ uuid ->
         se.se_metadata <- {
           se.se_metadata with
             e_auth_config = Some [{
               auth_system = "cas";
               auth_instance = "cas";
               auth_config = ["server", server];
             }]
         };
         return (redir_preapply election_setup uuid)))

let template_password = format_of_string
  "You are listed as a voter for the election

  %s

You will find below your login and password.  To cast a vote, you will
also need a credential, sent in a separate email.  Be careful,
passwords and credentials look similar but play different roles.  You
will be asked to enter your credential before entering the voting
booth.  Login and passwords are required once your ballot is ready to
be cast.

Username: %s
Password: %s
Page of the election: %s

Note that you are allowed to vote several times.  Only the last vote
counts.

-- 
Belenios"

let generate_password table title url v =
  lwt salt = generate_token () in
  lwt password = generate_token () in
  let hashed = sha256_hex (salt ^ password) in
  lwt () = Ocsipersist.add table v (salt, hashed) in
  let body = Printf.sprintf template_password title v password url in
  let subject = "Your password for election " ^ title in
  send_email "noreply@belenios.org" v subject body

let () =
  Any.register
    ~service:election_setup_auth_genpwd
    (handle_setup
       (fun se () _ uuid ->
         let table =
           "password_" ^
           let u = Uuidm.to_string uuid in
           underscorize u
         in
         let title = se.se_questions.t_name in
         let url = Eliom_uri.make_string_uri
           ~absolute:true ~service:election_home
           (uuid, ()) |> rewrite_prefix
         in
         let table = Ocsipersist.open_table table in
         Lwt_list.iter_s (generate_password table title url) se.se_voters >>
         return (fun () ->
           T.generic_page ~title:"Success"
             "Passwords have been generated and mailed!" () >>= Html5.send)))

let () =
  Any.register
    ~service:election_regenpwd
    (fun (uuid, ()) user ->
      T.regenpwd uuid () >>= Html5.send)

let () =
  Any.register
    ~service:election_regenpwd_post
    (fun (uuid, ()) user ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      lwt site_user = Web_auth_state.get_site_user () in
      match site_user with
      | Some u when W.metadata.e_owner = Some u ->
         let table = "password_" ^ underscorize uuid_s in
         let table = Ocsipersist.open_table table in
         let title = W.election.e_params.e_name in
         let url = Eliom_uri.make_string_uri
           ~absolute:true ~service:election_home
           (uuid, ()) |> rewrite_prefix
         in
         begin try_lwt
           lwt _ = Ocsipersist.find table user in
           generate_password table title url user >>
           T.generic_page ~title:"Success"
             ("A new password has been mailed to " ^ user ^ ".") ()
           >>= Html5.send
         with Not_found ->
           T.generic_page ~title:"Error"
             (user ^ " is not a registered user for this election.") ()
           >>= Html5.send
         end
      | _ -> forbidden ()
    )

let () =
  Html5.register
    ~service:election_setup_questions
    (fun uuid () ->
     match_lwt Web_auth_state.get_site_user () with
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
      match_lwt Web_auth_state.get_site_user () with
      | Some u ->
         let uuid_s = Uuidm.to_string uuid in
         lwt se = Ocsipersist.find election_stable uuid_s in
         if se.se_owner = u
         then T.election_setup_voters uuid se ()
         else forbidden ()
      | None -> forbidden ()
    )

(* see http://www.regular-expressions.info/email.html *)
let email_rex = Pcre.regexp
  ~flags:[`CASELESS]
  "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,7}$"

let is_email x =
  try ignore (Pcre.pcre_exec ~rex:email_rex x); true
  with Not_found -> false

let () =
  Any.register
    ~service:election_setup_voters_post
    (handle_setup
       (fun se x _ uuid ->
         let xs = Pcre.split x in
         let () =
           try
             let bad = List.find (fun x -> not (is_email x)) xs in
             Printf.ksprintf failwith "%S is not a valid address" bad
           with Not_found -> ()
         in
         se.se_voters <- xs;
         return (redir_preapply election_setup uuid)))

let () =
  Redirection.register
    ~service:election_setup_trustee_add
    (fun uuid () ->
     match_lwt Web_auth_state.get_site_user () with
     | Some u ->
        let uuid_s = Uuidm.to_string uuid in
        Lwt_mutex.with_lock election_setup_mutex (fun () ->
          lwt se = Ocsipersist.find election_stable uuid_s in
          if se.se_owner = u
          then (
            lwt token = generate_token () in
            se.se_public_keys <- (token, ref "") :: se.se_public_keys;
            Ocsipersist.add election_stable uuid_s se >>
            Ocsipersist.add election_pktokens token uuid_s
          ) else forbidden ()
        ) >>
        return (preapply election_setup_trustees uuid)
     | None -> forbidden ()
    )

let () =
  Redirection.register
    ~service:election_setup_trustee_del
    (fun uuid () ->
     match_lwt Web_auth_state.get_site_user () with
     | Some u ->
        let uuid_s = Uuidm.to_string uuid in
        Lwt_mutex.with_lock election_setup_mutex (fun () ->
          lwt se = Ocsipersist.find election_stable uuid_s in
          if se.se_owner = u
          then (
            match se.se_public_keys with
            | (token, _) :: xs ->
               se.se_public_keys <- xs;
               Ocsipersist.add election_stable uuid_s se >>
               Ocsipersist.remove election_pktokens token
            | _ -> return ()
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
  | e -> T.generic_page ~title:"Error" (Printexc.to_string e) () >>= Html5.send

let handle_credentials_post token creds =
  lwt uuid = Ocsipersist.find election_credtokens token in
  lwt se = Ocsipersist.find election_stable uuid in
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
  T.generic_page ~title:"Success"
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

module Credgen = struct
  module String = PString

  (* FIXME: duplicate of Tool_credgen *)

  let get_random_char =
    let prng = Lazy.lazy_from_fun (Lwt_preemptive.detach (fun () ->
      pseudo_rng (random_string secure_rng 16)
    )) in
    let mutex = Lwt_mutex.create () in
    fun () ->
      Lwt_mutex.with_lock mutex (fun () ->
        lwt prng = Lazy.force prng in
        let s = random_string prng 1 in
        return @@ s.[0]
      )

  (* Beware: the following must be changed in accordance with the booth! *)
  let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  let token_length = 14
  let n58 = Z.of_int 58
  let n53 = Z.of_int 53

  let generate_raw_token () =
    let res = String.create token_length in
    let rec loop i accu =
      if i < token_length then (
        lwt digit = get_random_char () in
        let digit = int_of_char digit mod 58 in
        res.[i] <- digits.[digit];
        loop (i+1) Z.(n58 * accu + of_int digit)
      ) else return (res, accu)
    in loop 0 Z.zero

  let add_checksum (raw, value) =
    let checksum = 53 - Z.(to_int (value mod n53)) in
    return (raw ^ String.make 1 digits.[checksum])

  let generate () =
    generate_raw_token () >>= add_checksum

end

let template_credential = format_of_string
  "You are listed as a voter for the election

  %s

You will find below your login and credential.  To cast a vote, you will
also need a password, sent in a separate email.  Be careful,
passwords and credentials look similar but play different roles.  You
will be asked to enter your credential before entering the voting
booth.  Login and passwords are required once your ballot is ready to
be cast.

Username: %s
Credential: %s
Page of the election: %s

Note that you are allowed to vote several times.  Only the last vote
counts.

-- 
Belenios"

let () =
  Any.register
    ~service:election_setup_credentials_server
    (handle_setup (fun se () _ uuid ->
      let () = se.se_metadata <- {se.se_metadata with
        e_cred_authority = Some "server"
      } in
      let uuid_s = Uuidm.to_string uuid in
      let title = se.se_questions.t_name in
      let url = Eliom_uri.make_string_uri
        ~absolute:true ~service:election_home
        (uuid, ()) |> rewrite_prefix
      in
      lwt se = Ocsipersist.find election_stable uuid_s in
      let module S = Set.Make (PString) in
      let module G = (val Group.of_string se.se_group : GROUP) in
      lwt creds =
        Lwt_list.fold_left_s (fun accu id ->
          lwt cred = Credgen.generate () in
          let priv_cred = derive_cred uuid cred in
          let pub_cred =
            let x = Z.(of_string_base 16 priv_cred mod G.q) in
            let y = G.(g **~ x) in
            G.to_string y
          in
          let body = Printf.sprintf template_credential title id cred url in
          let subject = "Your credential for election " ^ title in
          lwt () = send_email "noreply@belenios.org" id subject body in
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
      return (fun () ->
        T.generic_page ~title:"Success"
          "Credentials have been generated and mailed!" () >>= Html5.send)))

let () =
  Html5.register
    ~service:election_setup_trustee
    (fun token () ->
     lwt uuid = Ocsipersist.find election_pktokens token in
     lwt se = Ocsipersist.find election_stable uuid in
     T.election_setup_trustee token uuid se ()
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
           let pkref = List.assoc token se.se_public_keys in
           let module G = (val Group.of_string se.se_group : GROUP) in
           let pk = trustee_public_key_of_string G.read public_key in
           let module KG = Election.MakeSimpleDistKeyGen (G) (LwtRandom) in
           if not (KG.check pk) then failwith "invalid public key";
           (* we keep pk as a string because of G.t *)
           pkref := public_key;
           Ocsipersist.add election_stable uuid se
          ) >> T.generic_page ~title:"Success"
            "Your key has been received and checked!"
            () >>= Html5.send
       )
    )

let () =
  Any.register
    ~service:election_setup_create
    (fun uuid () ->
     match_lwt Web_auth_state.get_site_user () with
     | None -> forbidden ()
     | Some u ->
        begin try_lwt
          let uuid_s = Uuidm.to_string uuid in
          Lwt_mutex.with_lock election_setup_mutex (fun () ->
            lwt se = Ocsipersist.find election_stable uuid_s in
            if se.se_owner <> u then forbidden () else
            let group = Group.of_string se.se_group in
            let module G = (val group : GROUP) in
            let module KG = Election.MakeSimpleDistKeyGen (G) (LwtRandom) in
            (* construct election data in memory *)
            lwt public_keys, private_key =
              match se.se_public_keys with
              | [] ->
                 lwt private_key, public_key = KG.generate_and_prove () in
                 return ([public_key], Some private_key)
              | _ :: _ ->
                 return (List.rev_map
                   (fun (_, r) ->
                     if !r = "" then failwith "some public keys are missing";
                     trustee_public_key_of_string G.read !r
                   ) se.se_public_keys, None)
            in
            let y = KG.combine (Array.of_list public_keys) in
            let template = se.se_questions in
            let params = {
              e_description = template.t_description;
              e_name = template.t_name;
              e_public_key = {wpk_group = G.group; wpk_y = y};
              e_questions = template.t_questions;
              e_uuid = uuid;
              e_short_name = template.t_short_name;
            } in
            let files = {
              f_election = !spool_dir / uuid_s ^ ".election.json";
              f_metadata = !spool_dir / uuid_s ^ ".metadata.json";
              f_public_keys = !spool_dir / uuid_s ^ ".public_keys.jsons";
              f_public_creds = !spool_dir / uuid_s ^ ".public_creds.txt";
              f_voters = !spool_dir / uuid_s ^ ".voters.txt";
            } in
            lwt _ =
              try_lwt Lwt_unix.stat files.f_public_creds
              with _ -> failwith "public credentials are missing"
            in
            (* write election files to disk *)
            let create_file fname what xs =
              Lwt_io.with_file
                ~flags:(Unix.([O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC]))
                ~perm:0o600 ~mode:Lwt_io.Output fname
                (fun oc ->
                  Lwt_list.iter_s
                    (fun v ->
                      Lwt_io.write oc (what v) >>
                      Lwt_io.write oc "\n") xs)
            in
            create_file files.f_election (string_of_params (write_wrapped_pubkey G.write_group G.write)) [params] >>
            create_file files.f_metadata string_of_metadata [se.se_metadata] >>
            create_file files.f_voters (fun x -> x) se.se_voters >>
            create_file files.f_public_keys (string_of_trustee_public_key G.write) public_keys >>
            (* actually create the election *)
            begin match_lwt import_election files with
            | None ->
               T.new_election_failure `Exists () >>= Html5.send
            | Some w ->
               let module W = (val w : REGISTRABLE_ELECTION) in
               lwt w = W.register () in
               let module W = (val w : WEB_ELECTION) in
               (* create file with private key, if any *)
               lwt () =
                 match private_key with
                 | None -> return ()
                 | Some x ->
                    let fname = W.D.dir / "private_key.json" in
                    create_file fname string_of_number [x]
               in
               (* clean up temporary files *)
               Lwt_unix.unlink files.f_election >>
               Lwt_unix.unlink files.f_metadata >>
               Lwt_unix.unlink files.f_public_keys >>
               Lwt_unix.unlink files.f_public_creds >>
               Lwt_unix.unlink files.f_voters >>
               (* clean up tokens *)
               Ocsipersist.remove election_credtokens se.se_public_creds >>
               Lwt_list.iter_s
                 (fun (token, _) ->
                  Ocsipersist.remove election_pktokens token)
                 se.se_public_keys >>
               Ocsipersist.remove election_stable uuid_s >>
               Web_persist.set_election_date uuid_s (now ()) >>
               Redirection.send
                 (preapply election_admin (W.D.election.e_params.e_uuid, ()))
            end
          )
        with e ->
          T.new_election_failure (`Exception e) () >>= Html5.send
        end
    )

let () =
  Any.register
    ~service:election_home
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      try_lwt
        lwt w = find_election uuid_s in
        let module W = (val w) in
        Eliom_reference.unset Web_services.ballot >>
        let cont () =
          Redirection.send
            (Eliom_service.preapply
               election_home (W.election.e_params.e_uuid, ()))
        in
        Eliom_reference.set Web_auth_state.cont [cont] >>
        match_lwt Eliom_reference.get Web_services.cast_confirmed with
        | Some result ->
           Eliom_reference.unset Web_services.cast_confirmed >>
           T.cast_confirmed (module W) ~result () >>= Html5.send
        | None ->
           lwt state = Web_persist.get_election_state uuid_s in
           T.election_home (module W) state () >>= Html5.send
      with Not_found ->
        T.generic_page ~title:"Sorry, this election is not yet open"
          "This election does not exist yet. Please come back later." ()
          >>= Html5.send)

let () =
  Any.register
    ~service:election_admin
    (fun (uuid, ()) () ->
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt site_user = Web_auth_state.get_site_user () in
     let module W = (val w) in
     let uuid = Uuidm.to_string W.election.e_params.e_uuid in
     match site_user with
     | Some u when W.metadata.e_owner = Some u ->
        lwt state = Web_persist.get_election_state uuid in
        T.election_admin (module W) state () >>= Html5.send
       | _ -> forbidden ()
    )

let election_set_state state (uuid, ()) () =
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     let module W = (val w) in
     lwt () =
       match_lwt Web_auth_state.get_site_user () with
       | Some u when W.metadata.e_owner = Some u -> return ()
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

let () =
  Any.register
    ~service:election_update_credential
    (fun (uuid, ()) () ->
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt site_user = Web_auth_state.get_site_user () in
     let module W = (val w) in
     match site_user with
     | Some u ->
        if W.metadata.e_owner = Some u then (
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
     lwt site_user = Web_auth_state.get_site_user () in
     let module W = Web_election.Make ((val w)) (LwtRandom) in
     let module B = W.B in
     let module W = W.D in
     match site_user with
     | Some u ->
       if W.metadata.e_owner = Some u then (
         try_lwt
           B.update_cred ~old ~new_ >>
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
      Eliom_reference.unset Web_services.ballot >>
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
      Eliom_reference.set Web_auth_state.cont [cont] >>
      match_lwt Eliom_reference.get Web_services.ballot with
      | Some b -> T.cast_confirmation (module W) (sha256_b64 b) () >>= Html5.send
      | None -> T.cast_raw (module W) () >>= Html5.send)

let () =
  Any.register
    ~service:election_cast_post
    (fun (uuid, ()) (ballot_raw, ballot_file) ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = (val w) in
      lwt user = Web_auth_state.get_election_user uuid in
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
      Eliom_reference.set Web_auth_state.cont [cont] >>
      Eliom_reference.set Web_services.ballot (Some the_ballot) >>
      match user with
      | None ->
         Redirection.send
           (Eliom_service.preapply
              Web_services.election_login
              ((W.election.e_params.e_uuid, ()), None))
      | Some u -> cont ())

let () =
  Any.register
    ~service:election_cast_confirm
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = Web_election.Make ((val w)) (LwtRandom) in
      let module B = W.B in
      let module W = W.D in
      match_lwt Eliom_reference.get Web_services.ballot with
      | Some the_ballot ->
         begin
           Eliom_reference.unset Web_services.ballot >>
           match_lwt Web_auth_state.get_election_user uuid with
           | Some u ->
              let record = string_of_user u, now () in
              lwt result =
                try_lwt
                  lwt hash = B.cast the_ballot record in
                  return (`Valid hash)
                with Error e -> return (`Error e)
              in
              Eliom_reference.set Web_services.cast_confirmed (Some result) >>
              Redirection.send
                (Eliom_service.preapply
                   election_home (W.election.e_params.e_uuid, ()))
           | None -> forbidden ()
         end
      | None -> fail_http 404)

let () =
  Any.register
    ~service:election_pretty_ballots
    (fun ((uuid, ()), start) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = Web_election.Make ((val w)) (LwtRandom) in
      let module B = W.B in
      let module W = W.D in
      lwt res, _ =
        B.Ballots.fold
          (fun h _ (accu, i) ->
            if i >= start && i < start+1000 then
              return (h :: accu, i+1)
            else return (accu, i+1)
          ) ([], 1)
      in T.pretty_ballots (module W) res () >>= Html5.send)

let () =
  Any.register
    ~service:election_pretty_ballot
    (fun ((uuid, ()), hash) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = Web_election.Make ((val w)) (LwtRandom) in
      lwt ballot =
        W.B.Ballots.fold
          (fun h b accu ->
            if h = hash then return (Some b) else return accu
          ) None
      in
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
      let module W = (val w) in
      lwt () =
        match_lwt Web_auth_state.get_site_user () with
        | Some u when W.metadata.e_owner = Some u -> return ()
        | _ -> forbidden ()
      in
      let voters = Lwt_io.lines_of_file
        (W.dir / string_of_election_file ESVoters)
      in
      let module S = Set.Make (PString) in
      lwt voters = Lwt_stream.fold (fun v accu ->
        S.add v accu
      ) voters S.empty in
      let records = Lwt_io.lines_of_file
        (W.dir / string_of_election_file ESRecords)
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
      let module W = Web_election.Make ((val w)) (LwtRandom) in
      let module E = W.E in
      let module W = W.D in
      let pks = W.dir / string_of_election_file ESKeys in
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
      let et = W.dir / string_of_election_file ESETally in
      lwt et = Lwt_io.chars_of_file et |> Lwt_stream.to_string in
      let et = encrypted_tally_of_string W.G.read et in
      if E.check_factor et pk pd then (
        let pds = (trustee_id, partial_decryption) :: pds in
        lwt () = Web_persist.set_partial_decryptions uuid_s pds in
        T.generic_page ~title:"Success"
          "Your partial decryption has been received and checked!" () >>=
        Html5.send
      ) else (
        T.generic_page ~title:"Error"
          "The partial decryption didn't pass validation!" () >>=
        Html5.send
      ))

let handle_election_tally_release (uuid, ()) () =
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = Web_election.Make ((val w)) (LwtRandom) in
      let module E = W.E in
      let module W = W.D in
      lwt () =
        match_lwt Web_auth_state.get_site_user () with
        | Some u when W.metadata.e_owner = Some u -> return ()
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
        W.dir / string_of_election_file ESETally |>
        Lwt_io.chars_of_file |> Lwt_stream.to_string >>=
        wrap1 (encrypted_tally_of_string W.G.read)
      in
      let result = E.combine_factors ntallied et pds in
      lwt () =
        let open Lwt_io in
        with_file
          ~mode:Output (W.dir / string_of_election_file ESResult)
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

let handle_pseudo_file w u f site_user =
  let module W = (val w : WEB_ELECTION_DATA) in
  let confidential =
    match f with
    | ESRaw | ESKeys | ESBallots | ESETally | ESResult | ESCreds -> false
    | ESRecords | ESVoters -> true
  in
  lwt () =
    if confidential then (
      match site_user with
      | Some u when W.metadata.e_owner = Some u -> return ()
      | _ -> forbidden ()
    ) else return ()
  in
  let content_type = content_type_of_file f in
  File.send ~content_type (W.dir / string_of_election_file f)

let () =
  Any.register
    ~service:election_dir
    (fun (uuid, f) () ->
     let uuid_s = Uuidm.to_string uuid in
     lwt w = find_election uuid_s in
     lwt site_user = Web_auth_state.get_site_user () in
     let module W = (val w) in
     handle_pseudo_file w () f site_user)

let () =
  Any.register
    ~service:election_compute_encrypted_tally
    (fun (uuid, ()) () ->
      let uuid_s = Uuidm.to_string uuid in
      lwt w = find_election uuid_s in
      let module W = Web_election.Make ((val w)) (LwtRandom) in
      let module E = W.E in
      let module B = W.B in
      let module W = W.D in
      lwt () =
        match_lwt Web_auth_state.get_site_user () with
        | Some u when W.metadata.e_owner = Some u -> return ()
        | _ -> forbidden ()
      in
      lwt () =
        match_lwt Web_persist.get_election_state uuid_s with
        | `Closed -> return ()
        | _ -> forbidden ()
      in
      lwt nb, hash, tally = B.compute_encrypted_tally () in
      let pks = W.dir / string_of_election_file ESKeys in
      let pks = Lwt_io.lines_of_file pks in
      let npks = ref 0 in
      lwt () = Lwt_stream.junk_while (fun _ -> incr npks; true) pks in
      Web_persist.set_election_state uuid_s (`EncryptedTally (!npks, nb, hash)) >>
      (* compute partial decryption and release tally
         if the (single) key is known *)
      let skfile = W.dir / "private_key.json" in
      if !npks = 1 && Sys.file_exists skfile then (
        lwt sk = Lwt_io.lines_of_file skfile |> Lwt_stream.to_list in
        let sk = match sk with
          | [sk] -> number_of_string sk
          | _ -> failwith "several private keys are available"
        in
        let tally = encrypted_tally_of_string W.G.read tally in
        lwt pd = E.compute_factor tally sk in
        let pd = string_of_partial_decryption W.G.write pd in
        Web_persist.set_partial_decryptions uuid_s [1, pd] >>
        handle_election_tally_release (uuid, ()) ()
      ) else Redirection.send (preapply election_admin (uuid, ())))
