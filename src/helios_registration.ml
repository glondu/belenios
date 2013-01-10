open StdExtra
open Helios_datatypes_t
open Lwt

let election_file =
  let res = ref None in
  let open Ocsigen_extensions.Configuration in
  Eliom_config.parse_config [
    element
      ~name:"elections"
      ~obligatory:true
      ~attributes:[
        attribute ~name:"file" ~obligatory:true (fun s -> res := Some s)
      ]
      ()
  ];
  match !res with
    | Some s -> s
    | None -> raise (Ocsigen_extensions.Error_in_config_file
                       "could not find elections in configuration file")

let raw_elections =
  Ocsigen_messages.debug
    (fun () -> "Loading elections from " ^ election_file ^ "...");
  Lwt_io.lines_of_file election_file |>
  Lwt_stream.filter (fun s -> s <> "") |>
  Lwt_stream.to_list |> Lwt_main.run |>
  List.map (Helios_datatypes_j.election_of_string Core_datatypes_j.read_number)

let get_raw_election_by_uuid x =
  List.find (fun e -> Uuidm.equal e.e_uuid x) raw_elections

let test_uuid =
  match Uuidm.of_string "94c1a03e-1c48-11e2-8866-3cd92b7981b8" with
    | Some u -> u
    | None -> assert false

let elections =
  let open Helios_services in
  let open Helios_templates in
  [
    {
      election = get_raw_election_by_uuid test_uuid;
      election_admin = {
        user_name = "admin";
        user_type = "dummy";
      };
      election_trustees = [];
      election_state = `Finished [
        {
          answers = [
            {
              answer = "emacs";
              count = 2;
              winner = true;
            };
            {
              answer = "vim";
              count = 1;
              winner = false;
            };
            {
              answer = "gedit";
              count = 0;
              winner = false;
            };
            {
              answer = "something else";
              count = 1;
              winner = false;
            };
          ];
          question = "What is your favourite editor?";
        };
      ];
    }
  ]

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

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_view
  (fun uuid () ->
    try_lwt
      lwt election = get_election_by_uuid uuid in
      Helios_templates.election_view ~election
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
