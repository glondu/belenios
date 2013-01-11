open StdExtra
open Helios_datatypes_t
open Lwt

let election_index, election_library =
  let index = ref None in
  let library = ref None in
  let open Ocsigen_extensions.Configuration in
  Eliom_config.parse_config [
    element
      ~name:"elections"
      ~obligatory:true
      ~attributes:[
        attribute ~name:"index" ~obligatory:true (fun s -> index := Some s);
        attribute ~name:"library" ~obligatory:true (fun s -> library := Some s);
      ]
      ()
  ];
  match !index, !library with
    | Some i, Some l -> i, l
    | _ -> raise (Ocsigen_extensions.Error_in_config_file
                       "could not find index or library in configuration file")

let raw_elections =
  Ocsigen_messages.debug
    (fun () -> "Loading elections from " ^ election_index ^ "...");
  Lwt_io.lines_of_file election_index |>
  Lwt_stream.filter (fun s -> s <> "") |>
  Lwt_stream.to_list |> Lwt_main.run

let load_election_data raw =
  let election =
    Helios_datatypes_j.election_of_string Core_datatypes_j.read_number raw
  in
  let fingerprint = hashB raw in
  let dir = Filename.concat election_library
    ("{" ^ Uuidm.to_string election.e_uuid ^ "}")
  in
  let data x = Filename.concat dir x in
  let public_data = load_from_file
    (Helios_datatypes_j.read_election_public_data Core_datatypes_j.read_number)
    (data "public.json")
  in
  let votes =
    non_empty_lines_of_file (data "votes.json") |>
    Lwt_main.run |>
    List.map (Helios_datatypes_j.vote_of_string Core_datatypes_j.read_number) |>
    List.rev
  in
  Helios_services.({ raw; fingerprint; election; votes; public_data })

let elections = List.map load_election_data raw_elections

let get_raw_election_by_uuid x =
  let open Helios_services in
  List.find (fun e -> Uuidm.equal e.election.e_uuid x) elections

let test_uuid =
  match Uuidm.of_string "94c1a03e-1c48-11e2-8866-3cd92b7981b8" with
    | Some u -> u
    | None -> assert false

let format_election e =
  let open Helios_services in
  let open Helios_templates in
  let election = e.Helios_services.election in
  let election_admin = {
    user_name = "admin";
    user_type = "dummy";
  } in
  let election_trustees = [] in
  let election_state = match e.public_data.election_result with
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

let elections = List.map format_election elections

let get_featured_elections () =
  return elections

let get_election_by_name x =
  let open Helios_templates in
  wrap2 List.find (fun e -> e.election.e_short_name = x) elections

let get_election_by_uuid x =
  let open Helios_templates in
  wrap2 List.find (fun e -> Uuidm.equal e.election.e_uuid x) elections

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

let () = Eliom_registration.Redirection.register
  ~service:Helios_services.election_shortcut
  (fun name () ->
    try_lwt
      lwt e = get_election_by_name name in
      return (Eliom_service.preapply
                Helios_services.election_view
                e.Helios_templates.election.e_uuid)
    with Not_found ->
      raise_lwt Eliom_common.Eliom_404)

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
      return (election.Helios_templates.xelection.Helios_services.raw, "application/json")
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
      lwt election = get_election_by_uuid uuid in
      let service = Eliom_service.preapply Helios_services.election_raw uuid in
      return (Eliom_service.preapply Helios_services.election_booth
                (["booth"; "vote.html"],
                 Eliom_uri.make_string_uri ~absolute_path:true ~service ()))
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
