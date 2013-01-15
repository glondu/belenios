open StdExtra
open Helios_datatypes_t
open Lwt

let elections_table = Ocsipersist.open_table "elections"

let format_election e =
  let open Helios_services in
  let open Helios_templates in
  let election = e.Common.election in
  let election_admin = {
    user_name = "admin";
    user_type = "dummy";
  } in
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
  { election; xelection=e; election_admin; election_trustees; election_state }

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
  Ocsipersist.fold_step (fun uuid e res -> return (e :: res)) elections_table []

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
          (Some (admin_p, Helios_services.({user_name; user_type}))) >>
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

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_questions
  (fun uuid () ->
    Helios_templates.not_implemented "Questions")

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_voters
  (fun uuid () ->
    Helios_templates.not_implemented "Voters")

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_trustees
  (fun uuid () ->
    Helios_templates.not_implemented "Trustees")
