open Lwt

let user =
  Eliom_reference.eref
    ~scope:Eliom_common.session_group
    None

let featured_elections =
  Eliom_reference.eref
    ~scope:Eliom_common.global
    ~persistent:"featured_elections"
    []

let () = Eliom_registration.Html5.register
  ~service:Helios_services.home
  (fun () () ->
    lwt user = Eliom_reference.get user in
    lwt featured = Eliom_reference.get featured_elections in
    return (Helios_templates.index ~user ~featured))

let () = Eliom_registration.Html5.register
  ~service:Helios_services.elections_administered
  (fun () () ->
    return (Helios_templates.not_implemented "Administrate elections"))

let () = Eliom_registration.Html5.register
  ~service:Helios_services.election_new
  (fun () () ->
    return (Helios_templates.not_implemented "Create election"))
