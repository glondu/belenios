open Helios_datatypes_t

type election_data = {
  raw : string;
  fingerprint : string;
  election : Z.t election;
  public_data : Z.t election_public_data;
}

val load_elections_and_votes :
  string -> (election_data * Z.t vote Lwt_stream.t * voter Lwt_stream.t) Lwt_stream.t

val hash_vote : Z.t vote -> string
val hash_user : user -> string
