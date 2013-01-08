open Lwt

let elections =
  let open Helios_services in
  let open Helios_templates in
  [
    {
      election_uuid = "94c1a03e-1c48-11e2-8866-3cd92b7981b8";
      election_short_name = "editor";
      election_name = "Best editor";
      election_description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
      election_admin = {
        user_name = "admin";
        user_type = "dummy";
      };
      election_questions = [];
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
  wrap2 List.find (fun e -> e.election_short_name = x) elections

let get_election_by_uuid x =
  let open Helios_templates in
  wrap2 List.find (fun e -> e.election_uuid = x) elections

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
                e.Helios_templates.election_uuid)
    with Not_found ->
      raise_lwt Eliom_common.Eliom_404)

let () = Eliom_registration.Html5.register
  ~service:Helios_services.login
  (fun () () ->
    (* FIXME *)
    let service = Helios_services.perform_login () in
    let () = Eliom_registration.Redirection.register
      ~service
      ~scope:Eliom_common.session
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
    Eliom_state.discard ~scope:Eliom_common.session () >>
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
