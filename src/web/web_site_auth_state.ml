open Lwt
open Web_serializable_t

(* Forward references filled in by Web_site_auth, needed by Web_templates *)
let get_user : (unit -> user option Lwt.t) ref = ref (fun () -> return None)
let get_auth_systems : (unit -> string list) ref = ref (fun () -> [])
