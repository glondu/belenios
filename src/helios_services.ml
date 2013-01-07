open Eliom_service
open Eliom_parameter

let project_home = external_service
  ~prefix:"http://heliosvoting.org"
  ~path:[]
  ~get_params:unit
  ()

let home = service
  ~path:[]
  ~get_params:unit
  ()

let elections_administered = service
  ~path:["elections"; "administered"]
  ~get_params:unit
  ()

let election_new = service
  ~path:["elections"; "new"]
  ~get_params:unit
  ()

let election_shortcut = service
  ~path:["e"]
  ~get_params:(suffix (string "name"))
  ()

let login = service
  ~path:["login"]
  ~get_params:unit
  ()

let logout = service
  ~path:["logout"]
  ~get_params:unit
  ()

let perform_login () =
  Eliom_service.post_coservice
    ~csrf_safe:true
    ~csrf_scope:Eliom_common.session
    ~fallback:login
    ~post_params:Eliom_parameter.(string "username" ** bool "admin_p")
    ()

let auth_systems = [
  "dummy";
]

type user = {
  user_name : string;
  user_type : string;
}

let user = Eliom_reference.eref
  ~scope:Eliom_common.session
  (None : (bool * user) option)
