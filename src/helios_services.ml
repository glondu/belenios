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
  ~path:["election"; "administered"]
  ~get_params:unit
  ()

let election_new = service
  ~path:["election"; "new"]
  ~get_params:unit
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
  ~path:["election"; ""]
  ~get_params:uuid
  ()

let election_view = service
  ~path:["election"; "view"]
  ~get_params:uuid
  ()

let election_booth = static_dir_with_params
  ~get_params:(string "election_url")
  ()

let make_booth uuid =
  let service = Eliom_service.preapply election_raw uuid in
  Eliom_service.preapply election_booth (
    ["booth"; "vote.html"],
    Eliom_uri.make_string_uri ~absolute_path:true ~service ()
  )

let election_vote = service
  ~path:["election"; "vote"]
  ~get_params:uuid
  ()

let election_cast = service
  ~path:["election"; "cast"]
  ~get_params:uuid
  ()

let election_cast_post = post_service
  ~fallback:election_cast
  ~post_params:(
    string "election_uuid" **
    string "election_hash" **
    string "encrypted_vote"
  ) ()

let election_questions = service
  ~path:["election"; "questions"]
  ~get_params:uuid
  ()

let election_voters = service
  ~path:["election"; "voters"]
  ~get_params:uuid
  ()

let election_trustees = service
  ~path:["election"; "trustees"]
  ~get_params:uuid
  ()
