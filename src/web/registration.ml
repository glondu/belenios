open Util
open Serializable_t
open Lwt

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

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
      ~name:"data"
      ~obligatory:true
      ~attributes:[
        attribute ~name:"dir" ~obligatory:true (fun s -> data_dir := Some s);
      ] ();
  ];;

lwt () =
  match !secure_logfile with
    | Some x -> Web_common.open_security_log x
    | None -> return ()

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
          let fn_election = path/"election.json" in
          let fn_public_keys = path/"public_keys.jsons" in
          lwt b = file_exists fn_election in
          if b then (
            Ocsigen_messages.debug (fun () ->
              "-- registering " ^ subdir
            );
            lwt raw =
              Lwt_io.chars_of_file fn_election |>
              Lwt_stream.to_string
            in
            let election = Serializable_j.election_of_string
              Serializable_j.read_ff_pubkey raw
            in
            let fingerprint = sha256_b64 raw in
            lwt metadata =
              let fn = path/"metadata.json" in
              lwt b = file_exists fn in
              if b then (
                Lwt_io.chars_of_file fn |>
                Lwt_stream.to_string >>=
                wrap1 Serializable_j.metadata_of_string >>=
                (fun x -> return (Some x))
              ) else return None
            in
            let fn_public_creds = path/"public_creds.txt" in
            lwt public_creds =
              Lwt_io.lines_of_file fn_public_creds |>
              populate Web_common.SSet.empty (fun c accu ->
                return (Web_common.SSet.add c accu)
              )
            in
            let can_vote = match metadata with
              | None -> Web_common.Any
              | Some m -> match m.e_voters_list with
                | None -> Web_common.Any
                | Some voters ->
                  let set = List.fold_left (fun accu u ->
                    Web_common.SSet.add u accu
                  ) Web_common.SSet.empty voters in
                  Web_common.Restricted (fun u ->
                    return (Web_common.SSet.mem (Web_common.string_of_user u) set)
                  )
            in
            let election_data = Web_common.({
              fn_election;
              fingerprint;
              election;
              fn_public_keys;
              public_creds;
              featured_p = true;
              can_read = Any;
              can_vote;
            }) in
            let {g; p; q; y} = election.e_public_key in
            let module G = (val
              Election.finite_field ~p ~q ~g :
                Signatures.GROUP with type t = Z.t
            ) in
            let module P = struct
              module G = G
              let public_keys = lazy (assert false)
              let params = { election with e_public_key = y }
              let fingerprint = fingerprint
              let metadata = metadata
            end in
            let module X : Web_common.WEB_ELECTION = struct
              module G = G
              module M = Web_common.MakeLwtRandom(G)
              module P = P
              module E = Election.MakeElection(P)(M)
              module B = Web_common.MakeBallotBox(P)(E)
              let data = election_data
            end in
            X.B.inject_creds public_creds >>
            let uuid = election.e_uuid in
            return (EMap.add uuid (module X : Web_common.WEB_ELECTION) accu)
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
  EMap.fold (fun uuid e res ->
    let module X = (val e : Web_common.WEB_ELECTION) in
    let e = X.data in
    if e.Web_common.featured_p then e::res else res
  ) election_table [] |> return

let fail_http status =
  raise_lwt (
    Ocsigen_extensions.Ocsigen_http_error
      (Ocsigen_cookies.empty_cookieset, status)
  )

let forbidden () = fail_http 403

let check_acl acl election user =
  let open Web_common in
  match acl election with
    | Any -> return true
    | Restricted p ->
      match user with
        | Some user -> p user
        | None -> return false

let if_eligible acl f uuid x =
  lwt election = get_election_by_uuid uuid in
  let module X = (val election : Web_common.WEB_ELECTION) in
  lwt user = Eliom_reference.get Services.user in
  lwt b = check_acl acl X.data user in
  if b then f uuid election user x else forbidden ()

let () = Eliom_registration.Html5.register
  ~service:Services.home
  (fun () () ->
    Eliom_reference.unset Services.ballot >>
    Eliom_reference.unset Services.saved_service >>
    lwt featured = get_featured_elections () in
    Templates.index ~featured)

let () = Eliom_registration.Html5.register
  ~service:Services.login_dummy
  (fun () () ->
    let service = Services.create_dummy_login () in
    let () = Eliom_registration.Redirection.register
      ~service
      ~scope:Eliom_common.default_session_scope
      (fun () user_name ->
        let open Web_common in
        let user_type = Dummy in
        Eliom_reference.set Services.user (Some {user_name; user_type}) >>
        Web_common.security_log (fun () ->
          user_name ^ " successfully logged in using dummy"
        ) >>
        Services.get ())
    in
    Templates.dummy_login ~service)

let next_lf str i =
  try Some (String.index_from str i '\n')
  with Not_found -> None

let () = Eliom_registration.Redirection.register
  ~service:Services.login_cas
  (fun ticket () -> match ticket with
    | Some x ->
      let me =
        let service = Eliom_service.preapply Services.login_cas None in
        Eliom_uri.make_string_uri ~absolute:true ~service ()
      in
      let validation =
        let service = Eliom_service.preapply Services.cas_validate (me, x) in
        Eliom_uri.make_string_uri ~absolute:true ~service ()
      in
      lwt reply = Ocsigen_http_client.get_url validation in
      (match reply.Ocsigen_http_frame.frame_content with
        | Some stream ->
          lwt info = Ocsigen_stream.(string_of_stream 1000 (get stream)) in
          Ocsigen_stream.finalize stream `Success >>
          (match next_lf info 0 with
            | Some i ->
              (match String.sub info 0 i with
                | "yes" ->
                  (match next_lf info (i+1) with
                    | Some j ->
                      let open Web_common in
                      let user_name = String.sub info (i+1) (j-i-1) in
                      let user_type = CAS in
                      Web_common.security_log (fun () ->
                        user_name ^ " successfully logged in using CAS"
                      ) >>
                      Eliom_reference.set Services.user
                        (Some {user_name; user_type}) >>
                      Services.get ()
                    | None -> fail_http 502
                  )
                | "no" -> fail_http 401
                | _ -> fail_http 502
              )
            | None -> fail_http 502
          )
        | None -> fail_http 502
      )
    | None ->
      let service = Eliom_service.preapply Services.login_cas None in
      let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
      return (Eliom_service.preapply Services.cas_login uri)
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.logout
  (fun () () ->
    lwt user = Eliom_reference.get Services.user in
    (* should ballot be unset here or not? *)
    Eliom_reference.unset Services.user >>
    match user with
      | Some user ->
        if user.Web_common.user_type = Web_common.CAS then (
          lwt service = Services.get () in
          let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
          Web_common.(security_log (fun () ->
            string_of_user user ^ " logged out, redirecting to CAS"
          )) >>
          return (Eliom_service.preapply Services.cas_logout uri)
        ) else (
          Web_common.(security_log (fun () ->
            string_of_user user ^ " logged out"
          )) >> Services.get ()
        )
      | _ -> Services.get ()
  )

let can_read x = x.Web_common.can_read
let can_vote x = x.Web_common.can_vote

let () = Eliom_registration.File.register
  ~service:Services.election_raw
  ~content_type:"application/json"
  (if_eligible can_read
     (fun uuid election user () ->
       let module X = (val election : Web_common.WEB_ELECTION) in
       return X.data.Web_common.fn_election
     )
  )

let () = Eliom_registration.File.register
  ~service:Services.election_public_keys
  ~content_type:"application/json"
  (if_eligible can_read
      (fun uuid election user () ->
        let module X = (val election : Web_common.WEB_ELECTION) in
        return X.data.Web_common.fn_public_keys
      )
   )

let () = Eliom_registration.Streamlist.register
  ~service:Services.election_public_creds
  (if_eligible can_read
      (fun uuid election user () ->
        let module X = (val election : Web_common.WEB_ELECTION) in
        lwt creds = X.B.extract_creds () in
        let s = Web_common.SSet.fold (fun x accu ->
          (fun () -> return (Ocsigen_stream.of_string (x^"\n"))) :: accu
        ) creds [] in
        return (List.rev s, "text/plain")
       )
   )

let () = Eliom_registration.Streamlist.register
  ~service:Services.election_ballots
  (if_eligible can_read
     (fun uuid election user () ->
       let module X = (val election : Web_common.WEB_ELECTION) in
       (* TODO: streaming *)
       lwt ballots = X.B.fold_ballots (fun x xs ->
         return ((x^"\n")::xs)
       ) [] in
       let s = List.map (fun b () ->
         return (Ocsigen_stream.of_string b)
       ) ballots in
       return (s, "application/json")
     )
  )

let () = Eliom_registration.Streamlist.register
  ~service:Services.election_records
  (if_eligible can_read
     (fun uuid election user () ->
       if Web_common.is_admin user then (
         let module X = (val election : Web_common.WEB_ELECTION) in
         (* TODO: streaming *)
         lwt ballots = X.B.fold_records (fun (u, d) xs ->
           let x = Printf.sprintf "%s %S\n"
             (Serializable_builtin_j.string_of_datetime d) u
           in return (x::xs)
         ) [] in
         let s = List.map (fun b () ->
           return (Ocsigen_stream.of_string b)
         ) ballots in
         return (s, "text/plain")
       ) else forbidden ()
     )
  )

let get_randomness =
  let prng = Lazy.lazy_from_fun (Lwt_preemptive.detach (fun () ->
    Cryptokit.Random.(pseudo_rng (string secure_rng 16))
  )) in
  let mutex = Lwt_mutex.create () in
  fun () ->
    Lwt_mutex.lock mutex >>
    lwt prng = Lazy.force prng in
    let r = Cryptokit.Random.(string prng 32) in
    Lwt_mutex.unlock mutex;
    return r

let () = Eliom_registration.String.register
  ~service:Services.get_randomness
  (fun () () ->
    lwt r = get_randomness () in
    Cryptokit.(transform_string (Base64.encode_compact ()) r) |>
    (fun x -> Serializable_j.string_of_randomness { randomness=x }) |>
    (fun x -> return (x, "application/json"))
  )

let () = Eliom_registration.Html5.register
  ~service:Services.election_index
  (if_eligible can_read
     (fun uuid election user () ->
       Eliom_reference.unset Services.ballot >>
       Eliom_reference.set Services.saved_service (Services.Election uuid) >>
       Templates.election_view ~election ~user
     )
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.election_vote
  (if_eligible can_read
     (fun uuid election user () ->
       Eliom_reference.unset Services.ballot >>
       Eliom_reference.set Services.saved_service (Services.Election uuid) >>
       return (Services.make_booth uuid)
     )
  )

let do_cast election uuid () =
  match_lwt Eliom_reference.get Services.ballot with
    | Some ballot ->
      begin
        Eliom_reference.unset Services.ballot >>
        let open Web_common in
        let module X = (val election : WEB_ELECTION) in
        match_lwt Eliom_reference.get Services.user with
          | Some user as u ->
            lwt b = check_acl can_vote X.data u in
            if b then (
              let record =
                Web_common.string_of_user user,
                (CalendarLib.Fcalendar.Precise.now (), None)
              in
              lwt result =
                try_lwt
                  X.B.cast ballot record >>
                  return (`Valid (sha256_b64 ballot))
                with Error e -> return (`Error e)
              in
              Eliom_reference.unset Services.ballot >>
              Templates.do_cast_ballot ~election:X.data ~result
            ) else forbidden ()
          | None -> forbidden ()
      end
    | None -> fail_http 404

let ballot_received uuid election user =
  let module X = (val election : Web_common.WEB_ELECTION) in
  Eliom_reference.set Services.saved_service (Services.Cast uuid) >>
  let confirm () =
    let service = Services.create_confirm () in
    let () = Eliom_registration.Html5.register
      ~service
      ~scope:Eliom_common.default_session_scope
      (do_cast election)
    in service
  in
  lwt can_vote = check_acl can_vote X.data user in
  Templates.ballot_received ~election:X.data ~confirm ~user ~can_vote


let () = Eliom_registration.Html5.register
  ~service:Services.election_cast
  (if_eligible can_read
     (fun uuid election user () ->
       match_lwt Eliom_reference.get Services.ballot with
         | Some _ -> ballot_received uuid election user
         | None -> Templates.election_cast_raw ~election
     )
  )

let () = Eliom_registration.Html5.register
  ~service:Services.election_cast_post
  (if_eligible can_read
     (fun uuid election user ballot ->
       Eliom_reference.set Services.ballot (Some ballot) >>
       ballot_received uuid election user
     )
  )
