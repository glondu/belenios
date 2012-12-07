open Lwt
open Eliom_content.Html5.D
open Eliom_service
open Eliom_parameter
open Eliom_registration.Html5

let main_service =
  register_service ~path:["helios"] ~get_params:unit
    (fun () () -> return (html (head (title (pcdata "Page title")) [])
                               (body [h1 [pcdata "Helios"]])))
