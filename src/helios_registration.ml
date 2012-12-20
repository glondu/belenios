open Lwt

let user = Eliom_reference.eref
  ~scope:Eliom_common.session
  None

let auth_systems = [
  "dummy";
]

let get_featured_elections () =
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
    lwt user = Eliom_reference.get user in
    let mystuff = match user with
      | Some (admin_p, u) -> `User (u, (if admin_p then Some [] else None), [])
      | None -> `Auth_systems auth_systems
    in
    lwt featured = get_featured_elections () in
    return (Helios_templates.index ~mystuff ~featured))

let () = Eliom_registration.Html5.register
  ~service:Helios_services.elections_administered
  (fun () () ->
    return (Helios_templates.not_implemented "Administrate elections"))

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_new
  (fun () () ->
    return (Helios_templates.not_implemented "Create election"))

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_shortcut
  (fun _ () ->
    return (Helios_templates.not_implemented "Election shortcut"))

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
        Eliom_reference.set user
          (Some (admin_p,
                 Helios_templates.({user_name; user_type}))) >>
        return Helios_services.home)
    in
    return (Helios_templates.dummy_login ~service))

let () = Eliom_registration.Redirection.register
  ~service:Helios_services.logout
  (fun () () ->
    Eliom_state.discard ~scope:Eliom_common.session () >>
    return Helios_services.home)
