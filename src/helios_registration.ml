let () =
  Eliom_registration.Html5.register
    ~service:Helios_services.home
    (fun () () -> Lwt.return (Helios_templates.index
                                ~user:(`Login [])
                                ~featured:[]))

let () =
  Eliom_registration.Html5.register
    ~service:Helios_services.elections_administered
    (fun () () -> Lwt.return (Helios_templates.index
                                ~user:(`Login [])
                                ~featured:[]))

let () =
  Eliom_registration.Html5.register
    ~service:Helios_services.election_new
    (fun () () -> Lwt.return (Helios_templates.index
                                ~user:(`Login [])
                                ~featured:[]))
