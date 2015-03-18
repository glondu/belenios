val get_election_state : string -> [ `Open | `Closed ] Lwt.t
val set_election_state : string -> [ `Open | `Closed ] -> unit Lwt.t

val get_main_election : unit -> string option Lwt.t
val set_main_election : string -> unit Lwt.t
val unset_main_election : unit -> unit Lwt.t

val add_featured_election : string -> unit Lwt.t
val remove_featured_election : string -> unit Lwt.t
val is_featured_election : string -> bool Lwt.t
val get_featured_elections : unit -> string list Lwt.t
