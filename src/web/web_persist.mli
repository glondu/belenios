val get_election_state : string -> [ `Open | `Closed ] Lwt.t
val set_election_state : string -> [ `Open | `Closed ] -> unit Lwt.t
