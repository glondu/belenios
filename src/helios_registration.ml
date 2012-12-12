open Lwt

let user =
  Eliom_reference.eref
    ~scope:Eliom_common.session_group
    None

let auth_systems = [
]

let get_featured_elections () =
  let open Helios_templates in
  return [
  ]

let () = Eliom_registration.Html5.register
  ~service:Helios_services.home
  (fun () () ->
    lwt user = Eliom_reference.get user in
    let mystuff = match user with
      | Some u -> `User (u, None, [])
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
  (fun _ () ->
    return (Helios_templates.not_implemented "Login"))
