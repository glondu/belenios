open Lwt
open Common

let election_states = Ocsipersist.open_table "election_states"

let get_election_state x =
  try_lwt Ocsipersist.find election_states x
  with Not_found -> return `Open

let set_election_state x s =
  Ocsipersist.add election_states x s

let store = Ocsipersist.open_store "site"

lwt main_election =
  Ocsipersist.make_persistent store "main_election" None

lwt featured =
  Ocsipersist.make_persistent store "featured_elections" []

let add_featured_election x =
  lwt the_featured = Ocsipersist.get featured in
  if List.mem x the_featured then (
    return ()
  ) else (
    Ocsipersist.set featured (x :: the_featured)
  )

let rec list_remove x = function
  | [] -> []
  | y :: ys -> if x = y then ys else y :: (list_remove x ys)

let remove_featured_election x =
  lwt the_featured = Ocsipersist.get featured in
  Ocsipersist.set featured (list_remove x the_featured)

let is_featured_election x =
  lwt the_featured = Ocsipersist.get featured in
  return (List.mem x the_featured)

let get_featured_elections () =
  Ocsipersist.get featured

let get_main_election () =
  Ocsipersist.get main_election

let set_main_election x =
  Ocsipersist.set main_election (Some x)

let unset_main_election () =
  Ocsipersist.set main_election None
