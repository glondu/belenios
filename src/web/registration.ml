open Util
open Serializable_compat_t
open Lwt

(* The following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 32768

let elections_table = Ocsipersist.open_table "elections"

let () =
  let dir = ref None in
  let open Ocsigen_extensions.Configuration in
  Eliom_config.parse_config [
    element
      ~name:"load"
      ~obligatory:false
      ~attributes:[
        attribute ~name:"dir" ~obligatory:true (fun s -> dir := Some s);
      ]
      ()
  ];
  match !dir with
    | Some dir ->
      Ocsigen_messages.debug
        (fun () -> "Loading elections from " ^ dir ^ "...");
      Common.load_elections_and_votes dir |>
      Lwt_stream.iter_s (fun (e, ballots) ->
        let uuid = Uuidm.to_string e.Common.election.e_uuid in
        Ocsigen_messages.debug
          (fun () -> Printf.sprintf "-- loading %s (%s)" uuid e.Common.election.e_short_name);
        lwt () = Ocsipersist.add elections_table uuid e in
        let uuid_underscored = String.map (function '-' -> '_' | c -> c) uuid in
        let table = Ocsipersist.open_table ("ballots_" ^ uuid_underscored) in
        lwt () = Lwt_stream.iter_s (fun (r, v) ->
          Ocsipersist.add table (Common.hashB r) v
        ) ballots in
        return ()
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

let if_eligible f uuid x =
  lwt election = get_election_by_uuid uuid in
  lwt user = Eliom_reference.get Services.user in
  lwt () =
    if election.Common.private_p then (
      match user with
        | Some user ->
          lwt eligible = Services.is_eligible uuid user in
          if not eligible then forbidden () else return ()
        | None -> forbidden ()
    ) else return ()
  in f uuid election user x

let () = Eliom_registration.Html5.register
  ~service:Services.home
  (fun () () ->
    lwt featured = get_featured_elections () in
    Templates.index ~featured)

let () = Eliom_registration.Html5.register
  ~service:Services.login
  (fun () () ->
    (* FIXME *)
    let service = Services.perform_login () in
    let () = Eliom_registration.Redirection.register
      ~service
      ~scope:Eliom_common.default_session_scope
      (fun () user_name ->
        let user_type = "dummy" in
        Eliom_reference.set Services.user
          Common.(Some {user_name; user_type}) >>
        return Services.home)
    in
    Templates.dummy_login ~service)

let () = Eliom_registration.Redirection.register
  ~service:Services.logout
  (fun () () ->
    Eliom_state.discard ~scope:Eliom_common.default_session_scope () >>
    return Services.home)

let () = Eliom_registration.String.register
  ~service:Services.election_raw
  (if_eligible
     (fun uuid election user () ->
       return (election.Common.raw, "application/json")
     )
  )

let () = Eliom_registration.String.register
  ~service:Services.election_ballots
  (if_eligible
     (fun uuid election user () ->
       let uuid_underscored = String.map (function '-' -> '_' | c -> c) (Uuidm.to_string uuid) in
       let table = Ocsipersist.open_table ("ballots_" ^ uuid_underscored) in
       lwt ballots = Ocsipersist.fold_step (fun hash v res ->
         let s = Serializable_compat_j.string_of_ballot Serializable_builtin_j.write_number v ^ "\n" in
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
    (fun x -> return (Serializable_compat_j.string_of_randomness { randomness=x })) >>=
    (fun x -> return (x, "application/json"))
  )

let () = Eliom_registration.Html5.register
  ~service:Services.election_view
  (if_eligible
     (fun uuid election user () ->
       Templates.election_view ~election ~user
     )
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.election_vote
  (if_eligible
     (fun uuid election user () ->
       return (Services.make_booth uuid)
     )
  )

let () = Eliom_registration.Redirection.register
  ~service:Services.election_cast
  (if_eligible
     (fun uuid election user () ->
       return (
         Services.(preapply_uuid election_view election)
       )
     )
  )

let () = Eliom_registration.Html5.register
  ~service:Services.election_cast_post
  (if_eligible
     (fun uuid election user raw_ballot ->
       let result =
         try
           let ballot = Serializable_compat_j.ballot_of_string Serializable_builtin_j.read_number raw_ballot in
           let {g; p; q; y} = election.Common.election.e_public_key in
           let module P = struct
             module G = (val Election.finite_field ~p ~q ~g : Signatures.GROUP with type t = Z.t)
             let public_keys = Array.map (fun x ->
               x.trustee_public_key.y
             ) election.Common.public_keys
             let params = Serializable_compat.election election.Common.election
             let fingerprint = assert false
           end in
           let module M = Election.MakeSimpleMonad(P.G) in
           let module E = Election.MakeElection(P)(M) in
           if
             Uuidm.equal uuid ballot.election_uuid &&
             E.check_ballot (Serializable_compat.ballot ballot)
           then `Valid (Common.hashB raw_ballot)
           else `Invalid
         with e -> `Malformed
       in
       Templates.cast_ballot ~election ~result
     )
  )
