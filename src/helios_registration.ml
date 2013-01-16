open StdExtra
open Helios_datatypes_t
open Lwt

(* The following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 32768

let elections_table = Ocsipersist.open_table "elections"

let format_election e =
  let open Helios_services in
  let open Helios_templates in
  let election = e.Common.election in
  let election_trustees =
    e.Common.public_data.public_keys |>
    Array.map (fun k -> k.trustee_public_key.y |> Z.to_string |> hashB) |>
    Array.to_list
  in
  let election_state = match e.Common.public_data.election_result with
    | Some r ->
      Array.mapi (fun i q ->
        let q' = election.e_questions.(i) in
        let question = q'.q_question in
        let answers = Array.mapi (fun j a ->
          let answer = q'.q_answers.(j) in
          let count = a in
          (answer, count)
        ) q |> Array.to_list
        in
        let (winners, _) = List.fold_left
          (fun (ws, v) ((_, c) as w) ->
            if c > v then ([w], c)
            else if c = v then (w::ws, v)
            else (ws, v)
          ) ([], 0) answers
        in
        let answers = List.map
          (fun ((answer, count) as x) ->
            let winner = List.memq x winners in
            { answer; count; winner }
          ) answers
        in
        { question; answers }
      ) (r.result : int array array) |> (fun x -> `Finished (Array.to_list x))
    | None -> `Started
  in
  { election; xelection=e; election_trustees; election_state }

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
      Lwt_stream.iter_s (fun (e, votes) ->
        let uuid = Uuidm.to_string e.Common.election.e_uuid in
        Ocsigen_messages.debug
          (fun () -> Printf.sprintf "-- loading %s (%s)" uuid e.Common.election.e_short_name);
        lwt () = Ocsipersist.add elections_table uuid (format_election e) in
        let uuid_underscored = String.map (function '-' -> '_' | c -> c) uuid in
        let table = Ocsipersist.open_table ("votes_" ^ uuid_underscored) in
        Lwt_stream.iter_s (fun v ->
          Ocsipersist.add table (Common.hash_vote v) v
        ) votes
      ) |>
      Lwt_main.run
    | None -> ()

let get_election_by_uuid x =
  Ocsipersist.find elections_table (Uuidm.to_string x)

let get_featured_elections () =
  (* FIXME: doesn't scale when there are a lot of unfeatured elections *)
  Ocsipersist.fold_step (fun uuid e res ->
    let res = if e.Helios_templates.xelection.Common.public_data.featured_p then e::res else res in
    return res
  ) elections_table []

let () = Eliom_registration.Html5.register
  ~service:Helios_services.home
  (fun () () ->
    lwt featured = get_featured_elections () in
    Helios_templates.index ~featured)

let () = Eliom_registration.Html5.register
  ~service:Helios_services.elections_administered
  (fun () () ->
    Helios_templates.not_implemented "Administrate elections")

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_new
  (fun () () ->
    Helios_templates.not_implemented "Create election")

let () = Eliom_registration.Html5.register
  ~service:Helios_services.login
  (fun () () ->
    (* FIXME *)
    let service = Helios_services.perform_login () in
    let () = Eliom_registration.Redirection.register
      ~service
      ~scope:Eliom_common.default_session_scope
      (fun () (user_name, admin_p) ->
        let user_type = "dummy" in
        Eliom_reference.set Helios_services.user
          (Some (admin_p, {user_name; user_type})) >>
        return Helios_services.home)
    in
    Helios_templates.dummy_login ~service)

let () = Eliom_registration.Redirection.register
  ~service:Helios_services.logout
  (fun () () ->
    Eliom_state.discard ~scope:Eliom_common.default_session_scope () >>
    return Helios_services.home)

let () = Eliom_registration.String.register
  ~service:Helios_services.election_raw
  (fun uuid () ->
    try_lwt
      lwt election = get_election_by_uuid uuid in
      return (election.Helios_templates.xelection.Common.raw, "application/json")
    with Not_found ->
      raise_lwt Eliom_common.Eliom_404)

let () = Eliom_registration.String.register
  ~service:Helios_services.get_randomness
  (fun () () ->
    (* FIXME: DoS/entropy exhaustion vulnerability *)
    Lwt_preemptive.detach (fun () -> Cryptokit.Random.(string secure_rng 32)) () >>=
    wrap1 Cryptokit.(transform_string (Base64.encode_compact ())) >>=
    (fun x -> return (Helios_datatypes_j.string_of_randomness { randomness=x })) >>=
    (fun x -> return (x, "application/json"))
  )

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_view
  (fun uuid () ->
    try_lwt
      lwt election = get_election_by_uuid uuid in
      Helios_templates.election_view ~election
    with Not_found ->
      raise_lwt Eliom_common.Eliom_404)

let () = Eliom_registration.Redirection.register
  ~service:Helios_services.election_vote
  (fun uuid () ->
    try_lwt
      return (Helios_services.make_booth uuid)
    with Not_found ->
      raise_lwt Eliom_common.Eliom_404)

let () = Eliom_registration.Redirection.register
  ~service:Helios_services.election_cast
  (fun uuid () ->
      return (Eliom_service.preapply
                Helios_services.election_view
                uuid))

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_cast_post
  (fun uuid evote ->
    lwt election = get_election_by_uuid uuid in
    let result =
      try
        let vote = Helios_datatypes_j.vote_of_string Core_datatypes_j.read_number evote in
        let {g; p; q; y} = election.Helios_templates.election.e_public_key in
        let module G = (val ElGamal.make_ff_msubgroup p q g : ElGamal.GROUP with type t = Z.t) in
        let module Crypto = ElGamal.Make (G) in
        if
          Uuidm.equal uuid vote.election_uuid &&
          (* ehash = vote.election_hash && *)
          Crypto.verify_vote election.Helios_templates.election election.Helios_templates.xelection.Common.fingerprint vote
        then `Valid (Common.hash_vote vote)
        else `Invalid
      with e -> `Malformed
    in
    Helios_templates.vote_cast ~election ~result
  )
