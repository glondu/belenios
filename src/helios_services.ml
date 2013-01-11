open Helios_datatypes_t
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
    ~csrf_scope:Eliom_common.default_session_scope
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
  ~scope:Eliom_common.default_session_scope
  (None : (bool * user) option)

let uuid = Eliom_parameter.user_type
  (fun x -> match Uuidm.of_string x with
    | Some x -> x
    | None -> invalid_arg "uuid")
  Uuidm.to_string
  "uuid"

let election_raw = service
  ~path:["elections"; ""]
  ~get_params:uuid
  ()

let election_view = service
  ~path:["elections"; "view"]
  ~get_params:uuid
  ()

let election_booth = static_dir_with_params
  ~get_params:(string "election_url")
  ()

let election_vote = service
  ~path:["elections"; "vote"]
  ~get_params:uuid
  ()

let election_questions = service
  ~path:["elections"; "questions"]
  ~get_params:uuid
  ()

let election_voters = service
  ~path:["elections"; "voters"]
  ~get_params:uuid
  ()

let election_trustees = service
  ~path:["elections"; "trustees"]
  ~get_params:uuid
  ()

(* FIXME: type declarations should be elsewhere *)

type election_data = {
  raw : string;
  fingerprint : string;
  election : Z.t election;
  mutable votes : Z.t vote list;
  public_data : Z.t election_public_data;
}
