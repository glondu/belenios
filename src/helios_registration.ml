let () =
  Eliom_registration.Html5.register ~service:Helios_services.home_s
    (fun () () -> Lwt.return (Helios_templates.base
                            ~title:"Helios Election Server"
                            ~header:[]
                            ~content:[]))
