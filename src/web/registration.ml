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

open Signatures
open Util
open Serializable_t
open Lwt
open Web_common

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

module G = Election.DefaultGroup
module M = MakeLwtRandom(struct let rng = make_rng () end)
module E = Election.MakeElection(G)(M)

module EMap = Map.Make(Uuidm)

let ( / ) = Filename.concat

let file_exists x =
  try_lwt
    Lwt_unix.(access x [R_OK]) >>
    return true
  with _ ->
    return false

let populate accu f s = Lwt_stream.fold_s f s accu

let secure_logfile = ref None
let data_dir = ref None
let source_file = ref None
let enable_dummy = ref false
let password_db_fname = ref None
let enable_cas = ref false
let cas_server = ref "https://cas.example.org"
let main_election = ref None
let rewrite_src = ref None
let rewrite_dst = ref None

let () = CalendarLib.Time_Zone.(change Local)

let () =
  let open Ocsigen_extensions.Configuration in
  Eliom_config.parse_config [
    element
      ~name:"log"
      ~obligatory:true
      ~attributes:[
        attribute ~name:"file" ~obligatory:true (fun s -> secure_logfile := Some s);
      ] ();
    element
      ~name:"source"
      ~obligatory:true
      ~attributes:[
        attribute ~name:"file" ~obligatory:true (fun s -> source_file := Some s);
      ] ();
    element
      ~name:"data"
      ~obligatory:true
      ~attributes:[
        attribute ~name:"dir" ~obligatory:true (fun s -> data_dir := Some s);
      ] ();
    element
      ~name:"rewrite-prefix"
      ~obligatory:false
      ~attributes:[
        attribute ~name:"src" ~obligatory:true (fun s -> rewrite_src := Some s);
        attribute ~name:"dst" ~obligatory:true (fun s -> rewrite_dst := Some s);
      ] ();
    element
      ~name:"enable-dummy"
      ~obligatory:false
      ~init:(fun () -> enable_dummy := true)
      ();
    element
      ~name:"enable-password"
      ~obligatory:false
      ~attributes:[
        attribute ~name:"db" ~obligatory:true (fun s -> password_db_fname := Some s);
      ] ();
    element
      ~name:"enable-cas"
      ~obligatory:false
      ~init:(fun () -> enable_cas := true)
      ~attributes:[
        attribute ~name:"server" ~obligatory:true (fun s -> cas_server := s);
      ] ();
    element
      ~name:"main-election"
      ~obligatory:false
      ~attributes:[
        attribute ~name:"uuid" ~obligatory:true (fun s -> main_election := Some s);
      ] ();
  ];;

let password_db = match !password_db_fname with
  | None -> None
  | Some fname -> Some (
    List.fold_left (fun accu line ->
      match line with
      | username :: salt :: password :: _ ->
        SMap.add username (salt, password) accu
      | _ -> failwith "error in password db file"
    ) SMap.empty (Csv.load fname)
  )

let rewrite_prefix =
  match !rewrite_src, !rewrite_dst with
  | Some src, Some dst ->
    let nsrc = String.length src in
    (fun x ->
      let n = String.length x in
      if n >= nsrc && String.sub x 0 nsrc = src then
        dst ^ String.sub x nsrc (n-nsrc)
      else x
    )
  | _, _ -> (fun x -> x)

lwt () =
  match !secure_logfile with
    | Some x -> open_security_log x
    | None -> return ()

let main_election = match !main_election with
  | None -> None
  | Some u ->
    match Uuidm.of_string u with
    | Some u -> Some u
    | None -> failwith "Incorrect UUID in configuration <main-election> tag"

lwt election_table =
  match !data_dir with
    | Some dir ->
      Ocsigen_messages.debug (fun () ->
        "Using data from " ^ dir ^ "..."
      );
      Lwt_unix.files_of_directory dir |>
      populate EMap.empty (fun subdir accu ->
        let path = dir/subdir in
        lwt b = file_exists (path/"result.json") in
        if b then (
          (* result is available *)
          (* TODO: if the election is featured, show it on the home page *)
          return accu
        ) else (
          let params_fname = path/"election.json" in
          let public_keys_fname = path/"public_keys.jsons" in
          lwt b = file_exists params_fname in
          if b then (
            Ocsigen_messages.debug (fun () ->
              "-- registering " ^ subdir
            );
            lwt raw_election =
              Lwt_io.lines_of_file params_fname |>
              Lwt_stream.to_list |>
              (fun x -> match_lwt x with
              | [e] -> return e
              | _ -> failwith "election.json is invalid")
            in
            lwt metadata =
              let fname = path/"metadata.json" in
              lwt b = file_exists fname in
              if b then (
                Lwt_io.chars_of_file fname |>
                Lwt_stream.to_string >>=
                wrap1 Serializable_j.metadata_of_string >>=
                (fun x -> return (Some x))
              ) else return None
            in
            let public_creds_fname = path/"public_creds.txt" in
            lwt public_creds =
              Lwt_io.lines_of_file public_creds_fname |>
              populate SSet.empty (fun c accu ->
                return (SSet.add c accu)
              )
            in
            let can_vote = match metadata with
              | None -> Web_election.Any
              | Some m -> match m.e_voters_list with
                | None -> Web_election.Any
                | Some voters ->
                  let set = List.fold_left (fun accu u ->
                    SSet.add u accu
                  ) SSet.empty voters in
                  Web_election.Restricted (fun u ->
                    return (SSet.mem (Auth_common.string_of_user u) set)
                  )
            in
            let election_web = Web_election.({
              params_fname;
              public_keys_fname;
              featured_p = true;
              can_read = Any;
              can_vote;
            }) in
            let open Web_election in
            let web_election = make_web_election
              raw_election metadata election_web
            in
            let module X = (val web_election.modules : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t) in
            X.B.inject_creds public_creds >>
            let uuid = web_election.election.e_params.e_uuid in
            return (EMap.add uuid web_election accu)
          ) else return accu
        )
       )
    | None -> return EMap.empty

let get_election_by_uuid x =
  try_lwt
    EMap.find x election_table |> return
  with Not_found ->
    raise_lwt Eliom_common.Eliom_404

let get_featured_elections () =
  let open Web_election in
  EMap.fold (fun uuid e res ->
    if e.election_web.featured_p then
      e.election.e_params :: res
    else res
  ) election_table [] |> return

let check_acl acl election user =
  let open Web_election in
  match acl election user with
    | Any -> return true
    | Restricted p ->
      match user with
        | Some user -> p user.Auth_common.user_user
        | None -> return false

let if_eligible acl f uuid x =
  lwt election = get_election_by_uuid uuid in
  lwt user = Eliom_reference.get Auth_common.user in
  lwt b = check_acl acl election.Web_election.election_web user in
  if b then f uuid election user x else forbidden ()

module S = struct
  open Eliom_service
  open Eliom_parameter
  open Services

  let home = service
    ~path:[]
    ~get_params:unit
    ()

  let source_code = service
    ~path:["belenios.tar.gz"]
    ~get_params:unit
    ()

  let election_index = service
    ~path:["election"; ""]
    ~get_params:uuid
    ()

  let election_vote = service
    ~path:["election"; "vote"]
    ~get_params:uuid
    ()

  let election_cast = service
    ~path:["election"; "cast"]
    ~get_params:uuid
    ()

  let create_confirm () =
    Eliom_service.post_coservice
      ~csrf_safe:true
      ~csrf_scope:Eliom_common.default_session_scope
      ~fallback:election_cast
      ~post_params:Eliom_parameter.unit
      ()

  let election_cast_post = post_service
    ~fallback:election_cast
    ~post_params:(opt (string "encrypted_vote") ** opt (file "encrypted_vote_file"))
    ()

  let election_file = Eliom_parameter.user_type
    election_file_of_string
    string_of_election_file
    "file"

  let election_dir = service
    ~path:["elections"]
    ~get_params:(suffix (uuid ** election_file))
    ()

  let election_booth = static_dir_with_params
    ~get_params:(string "election_url")
    ()

  let election_update_credential = post_service
    ~fallback:election_update_credential_form
    ~post_params:(string "old_credential" ** string "new_credential")
    ()

  let make_booth uuid =
    let service = Eliom_service.preapply election_dir (uuid, ESRaw) in
    Eliom_service.preapply election_booth (
      ["booth"; "vote.html"],
      Eliom_uri.make_string_uri ~service ()
    )

  let election_file e f = Eliom_service.preapply election_dir (e.e_uuid, f)

  let to_service = function
    | Home -> home
    | Cast u -> Eliom_service.preapply election_cast u
    | Election u -> Eliom_service.preapply election_index u

  open Lwt

  let get () =
    Eliom_reference.get saved_service >>= wrap1 to_service

  let set s =
    Eliom_reference.set saved_service s

  include Auth_common.Make (struct end)
end

module T = Templates.Make (S)

module C = struct
  let enable_cas = !enable_cas
  let cas_server = !cas_server
  let password_db = password_db
  let enable_dummy = !enable_dummy
  let rewrite_prefix = rewrite_prefix
end

module A = Auth_common.Register (S)

let () =
  if C.enable_dummy then let module X = Auth_dummy.Register (S) (T) in ()

let () =
  match C.password_db with
  | Some _ -> let module X = Auth_password.Register (C) (S) (T) in ()
  | None -> ()

let () =
  if C.enable_cas then let module X = Auth_cas.Register (C) (S) in ()

let () =
  match main_election with
  | None -> Eliom_registration.Html5.register ~service:S.home
    (fun () () ->
      Eliom_reference.unset Services.ballot >>
      Eliom_reference.unset Services.saved_service >>
      lwt featured = get_featured_elections () in
      T.index ~featured
    )
  | Some uuid -> Eliom_registration.Redirection.register ~service:S.home
    (fun () () ->
      Eliom_reference.unset Services.ballot >>
      Eliom_reference.unset Services.saved_service >>
      return (Eliom_service.preapply S.election_index uuid)
    )

let can_read x u = x.Web_election.can_read
let can_vote x u = x.Web_election.can_vote

let () = Eliom_registration.File.register
  ~service:S.source_code
  ~content_type:"application/x-gzip"
  (fun () () -> match !source_file with
  | None -> fail_http 404
  | Some f ->
    match_lwt Eliom_reference.get Auth_common.user with
    | Some u ->
      security_log (fun () ->
        Auth_common.(string_of_user u.user_user) ^ " downloaded source code"
      ) >>
      return f
    | None ->
      security_log (fun () ->
        "someone anonymously downloaded source code"
      ) >>
      return f
  )

let f_raw uuid election user () =
  return Web_election.(election.election_web.params_fname)

let f_keys uuid election user () =
  return Web_election.(election.election_web.public_keys_fname)

let f_creds uuid election user () =
  let open Web_election in
  let module X = (val election.modules : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t) in
  lwt creds = X.B.extract_creds () in
  let s = SSet.fold (fun x accu ->
    (fun () -> return (Ocsigen_stream.of_string (x^"\n"))) :: accu
  ) creds [] in
  return (List.rev s, "text/plain")

let f_ballots uuid election user () =
  let open Web_election in
  let module X = (val election.modules : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t) in
  (* TODO: streaming *)
  lwt ballots = X.B.Ballots.fold (fun _ x xs ->
    return ((x^"\n")::xs)
  ) [] in
  let s = List.map (fun b () ->
    return (Ocsigen_stream.of_string b)
  ) ballots in
  return (s, "application/json")

let f_records uuid election user () =
  match user with
  | Some u when u.Auth_common.user_admin ->
    let open Web_election in
    let module X = (val election.modules : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t) in
    (* TODO: streaming *)
    lwt ballots = X.B.Records.fold (fun u (d, _) xs ->
      let x = Printf.sprintf "%s %S\n"
        (Serializable_builtin_j.string_of_datetime d) u
      in return (x::xs)
    ) [] in
    let s = List.map (fun b () ->
      return (Ocsigen_stream.of_string b)
    ) ballots in
    return (s, "text/plain")
  | _ -> forbidden ()

let handle_pseudo_file u f =
  let open Eliom_registration in
  let open Services in
  let file f =
    if_eligible can_read f u () >>=
    File.send ~content_type:"application/json"
  and stream f =
    if_eligible can_read f u () >>=
    Streamlist.send >>=
    (fun x -> return (cast_unknown_content_kind x))
  in
  match f with
  | ESRaw -> file f_raw
  | ESKeys -> file f_keys
  | ESCreds -> stream f_creds
  | ESBallots -> stream f_ballots
  | ESRecords -> stream f_records

let () =
  Eliom_registration.Any.register
    ~service:S.election_dir
    (fun (uuid, f) () -> handle_pseudo_file uuid f)

let get_randomness =
  let prng = Lazy.lazy_from_fun (Lwt_preemptive.detach (fun () ->
    Cryptokit.Random.(pseudo_rng (string secure_rng 16))
  )) in
  let mutex = Lwt_mutex.create () in
  fun () ->
    Lwt_mutex.with_lock mutex (fun () ->
      lwt prng = Lazy.force prng in
      return Cryptokit.Random.(string prng 32)
    )

let () = Eliom_registration.String.register
  ~service:Services.get_randomness
  (fun () () ->
    lwt r = get_randomness () in
    Cryptokit.(transform_string (Base64.encode_compact ()) r) |>
    (fun x -> Serializable_j.string_of_randomness { randomness=x }) |>
    (fun x -> return (x, "application/json"))
  )

let () = Eliom_registration.Html5.register
  ~service:S.election_index
  (if_eligible can_read
     (fun uuid election user () ->
       Eliom_reference.unset Services.ballot >>
       Eliom_reference.set Services.saved_service (Services.Election uuid) >>
       T.election_view ~election ~user
     )
  )

let () = Eliom_registration.Redirection.register
  ~service:S.election_vote
  (if_eligible can_read
     (fun uuid election user () ->
       Eliom_reference.unset Services.ballot >>
       Eliom_reference.set Services.saved_service (Services.Election uuid) >>
       return (S.make_booth uuid)
     )
  )

let do_cast election uuid () =
  match_lwt Eliom_reference.get Services.ballot with
    | Some ballot ->
      begin
        Eliom_reference.unset Services.ballot >>
        let open Web_election in
        let module X = (val election.modules : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t) in
        match_lwt Eliom_reference.get Auth_common.user with
          | Some user as u ->
            lwt b = check_acl can_vote election.election_web u in
            if b then (
              let record =
                Auth_common.string_of_user user.Auth_common.user_user,
                (CalendarLib.Fcalendar.Precise.now (), None)
              in
              lwt result =
                try_lwt
                  lwt hash = X.B.cast ballot record in
                  return (`Valid hash)
                with Error e -> return (`Error e)
              in
              Eliom_reference.unset Services.ballot >>
              T.do_cast_ballot ~election ~result
            ) else forbidden ()
          | None -> forbidden ()
      end
    | None -> fail_http 404

let ballot_received uuid election user =
  let open Web_election in
  let module X = (val election.modules : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t) in
  Eliom_reference.set Services.saved_service (Services.Cast uuid) >>
  let confirm () =
    let service = S.create_confirm () in
    let () = Eliom_registration.Html5.register
      ~service
      ~scope:Eliom_common.default_session_scope
      (do_cast election)
    in service
  in
  lwt can_vote = check_acl can_vote election.election_web user in
  T.ballot_received ~election ~confirm ~user ~can_vote


let () = Eliom_registration.Html5.register
  ~service:S.election_cast
  (if_eligible can_read
     (fun uuid election user () ->
       match_lwt Eliom_reference.get Services.ballot with
         | Some _ -> ballot_received uuid election user
         | None -> T.election_cast_raw ~election
     )
  )

let () = Eliom_registration.Redirection.register
  ~service:S.election_cast_post
  (if_eligible can_read
     (fun uuid election user (ballot_raw, ballot_file) ->
       lwt ballot = match ballot_raw, ballot_file with
         | Some ballot, None -> return ballot
         | None, Some fi ->
           let fname = fi.Ocsigen_extensions.tmp_filename in
           Lwt_stream.to_string (Lwt_io.chars_of_file fname)
         | _, _ -> fail_http 400
       in
       Eliom_reference.set Services.saved_service (Services.Cast uuid) >>
       Eliom_reference.set Services.ballot (Some ballot) >>
       match user with
         | None -> return (Eliom_service.preapply S.login None)
         | Some u -> S.get ()
     )
  )

let () = Eliom_registration.Html5.register
  ~service:Services.election_update_credential_form
  (fun uuid () ->
    lwt user = Eliom_reference.get Auth_common.user in
    match user with
    | Some u when u.Auth_common.user_admin ->
      lwt election = get_election_by_uuid uuid in
      T.election_update_credential ~election
    | _ -> forbidden ()
  )

let () = Eliom_registration.String.register
  ~service:S.election_update_credential
  (fun uuid (old, new_) ->
    lwt user = Eliom_reference.get Auth_common.user in
    match user with
    | Some u when u.Auth_common.user_admin ->
      lwt election = get_election_by_uuid uuid in
      let open Web_election in
      let module X = (val election.modules : WEB_BALLOT_BOX_BUNDLE with type elt = Z.t) in
      begin try_lwt
        X.B.update_cred ~old ~new_ >>
        return ("OK", "text/plain")
      with Error e ->
        return ("Error: " ^ explain_error e, "text/plain")
      end
    | _ -> forbidden ()
  )
