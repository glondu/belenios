open Lwt

let get_featured_elections () =
  let open Helios_services in
  let open Helios_templates in
  return [
    {
      election_short_name = "editor";
      election_name = "Best editor";
      election_description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
      election_admin = {
        user_name = "admin";
        user_type = "dummy";
      };
    }
  ]

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
  ~service:Helios_services.election_shortcut
  (fun _ () ->
    Helios_templates.not_implemented "Election shortcut")

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
