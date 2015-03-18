open Lwt

let election_states = Ocsipersist.open_table "election_states"

let get_election_state x =
  try_lwt Ocsipersist.find election_states x
  with Not_found -> return `Open

let set_election_state x s =
  Ocsipersist.add election_states x s
