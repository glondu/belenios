open Util
open Serializable_t
open Lwt

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

let elections_table = Ocsipersist.open_table "elections"
let imported_table = Ocsipersist.open_table "imported"

let () =
  let dir = ref None in
  let open Ocsigen_extensions.Configuration in
  Eliom_config.parse_config [
    element
      ~name:"import"
      ~obligatory:false
      ~attributes:[
        attribute ~name:"dir" ~obligatory:true (fun s -> dir := Some s);
      ]
      ()
  ];
  match !dir with
    | Some dir ->
      Ocsigen_messages.debug
        (fun () -> "Importing elections from " ^ dir ^ "...");
      Common.load_elections_and_votes dir |>
      Lwt_stream.iter_s (fun (e, ballots) ->
        let uuid = Uuidm.to_string e.Common.election.e_uuid in
        lwt b =
          try_lwt Ocsipersist.find imported_table uuid
          with Not_found -> return false
        in
        if not b then (
          Ocsigen_messages.debug (fun () ->
            Printf.sprintf "-- importing %s (%s)"
              uuid e.Common.election.e_short_name
          );
          lwt () = Ocsipersist.add elections_table uuid e in
          let uuid_underscored = String.map (function '-' -> '_' | c -> c) uuid in
          let table = Ocsipersist.open_table ("ballots_" ^ uuid_underscored) in
          Lwt_stream.iter_s (fun (r, v) ->
            Ocsipersist.add table (Common.hashB r) v
          ) ballots >>
          Ocsipersist.add imported_table uuid true
        ) else (
          Ocsigen_messages.debug (fun () ->
            Printf.sprintf "-- skipping %s (%s)" uuid
              e.Common.election.e_short_name
          );
          return ()
        )
      ) |>
      Lwt_main.run
    | None -> ()

let get_election_by_uuid x =
  try_lwt
    Ocsipersist.find elections_table (Uuidm.to_string x)
  with Not_found ->
    raise_lwt Eliom_common.Eliom_404

let get_featured_elections () =
  (* FIXME: doesn't scale when there are a lot of unfeatured elections *)
  Ocsipersist.fold_step (fun uuid e res ->
    let res = if e.Common.featured_p then e::res else res in
    return res
  ) elections_table []

let forbidden () = raise_lwt (
  Ocsigen_extensions.Ocsigen_http_error (
    Ocsigen_cookies.empty_cookieset, 403
  )
)

let if_eligible acl f uuid x =
  lwt election = get_election_by_uuid uuid in
  lwt user = Eliom_reference.get Services.user in
  lwt () =
    let open Common in
    match acl election with
      | Any -> return ()
      | Restricted p ->
        match user with
          | Some user ->
            lwt ok = p user in
            if ok then return () else forbidden ()
        | None -> forbidden ()
  in f uuid election user x

let () = Eliom_registration.Html5.register
  ~service:Services.home
  (fun () () ->
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
        let open Common in
        let user_type = Dummy in
        Eliom_reference.set Services.user (Some {user_name; user_type}) >>
        Services.get ())
    in
    Templates.dummy_login ~service)

let next_lf str i =
  try Some (String.index_from str i '\n')
  with Not_found -> None

let fail_http status =
  fail (
    Ocsigen_extensions.Ocsigen_http_error
      (Ocsigen_cookies.empty_cookieset, status)
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.login_cas
  (fun ticket () -> match ticket with
    | Some x ->
      let service = Eliom_service.preapply Services.login_cas None in
      let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
      let service = Eliom_service.preapply Services.cas_validate (uri, x) in
      let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
      lwt reply = Ocsigen_http_client.get_url uri in
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
                      let open Common in
                      let user_name = String.sub info (i+1) (j-i-1) in
                      let user_type = CAS in
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
    Eliom_reference.unset Services.user >>
    match user with
      | Some user when user.Common.user_type = Common.CAS ->
        lwt service = Services.get () in
        let uri = Eliom_uri.make_string_uri ~absolute:true ~service () in
        return (Eliom_service.preapply Services.cas_logout uri)
      | _ -> Services.get ()
  )

let can_read x = x.Common.can_read
let can_vote x = x.Common.can_vote

let () = Eliom_registration.String.register
  ~service:Services.election_raw
  (if_eligible can_read
     (fun uuid election user () ->
       return (election.Common.raw, "application/json")
     )
  )

let () = Eliom_registration.String.register
  ~service:Services.election_ballots
  (if_eligible can_read
     (fun uuid election user () ->
       let uuid_underscored = String.map (function '-' -> '_' | c -> c) (Uuidm.to_string uuid) in
       let table = Ocsipersist.open_table ("ballots_" ^ uuid_underscored) in
       lwt ballots = Ocsipersist.fold_step (fun hash v res ->
         let s = Serializable_j.string_of_ballot Serializable_builtin_j.write_number v ^ "\n" in
         return (s :: res)
       ) table [] in
       let result = String.concat "" ballots in
       return (result, "application/json")
     )
  )

let () = Eliom_registration.String.register
  ~service:Services.get_randomness
  (fun () () ->
    (* FIXME: DoS/entropy exhaustion vulnerability *)
    Lwt_preemptive.detach (fun () -> Cryptokit.Random.(string secure_rng 32)) () >>=
    wrap1 Cryptokit.(transform_string (Base64.encode_compact ())) >>=
    (fun x -> return (Serializable_j.string_of_randomness { randomness=x })) >>=
    (fun x -> return (x, "application/json"))
  )

let () = Eliom_registration.Html5.register
  ~service:Services.election_view
  (if_eligible can_read
     (fun uuid election user () ->
       Templates.election_view ~election ~user
     )
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.election_vote
  (if_eligible can_vote
     (fun uuid election user () ->
       return (Services.make_booth uuid)
     )
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.election_cast
  (if_eligible can_vote
     (fun uuid election user () ->
       return (
         Services.(preapply_uuid election_view election)
       )
     )
  )

let () = Eliom_registration.Html5.register
  ~service:Services.election_cast_post
  (if_eligible can_vote
     (fun uuid election user raw_ballot ->
       let result =
         try
           let ballot = Serializable_j.ballot_of_string Serializable_builtin_j.read_number raw_ballot in
           let {g; p; q; y} = election.Common.election.e_public_key in
           let module P = struct
             module G = (val Election.finite_field ~p ~q ~g : Signatures.GROUP with type t = Z.t)
             let public_keys = Array.map (fun x ->
               x.trustee_public_key
             ) election.Common.public_keys
             let params = { election.Common.election with e_public_key = y }
             let fingerprint = election.Common.fingerprint
           end in
           let module M = Election.MakeSimpleMonad(P.G) in
           let module E = Election.MakeElection(P)(M) in
           if
             Uuidm.equal uuid ballot.election_uuid &&
             E.check_ballot ballot
           then `Valid (Common.hashB raw_ballot)
           else `Invalid
         with e -> `Malformed e
       in
       Templates.cast_ballot ~election ~result
     )
  )
