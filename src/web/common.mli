open Serializable_compat_t

type election_data = {
  raw : string;
  fingerprint : string;
  election : Z.t election;
  public_data : Z.t election_public_data;
}

val hashB : string -> string

val load_elections_and_votes :
  string -> (election_data * (string * Z.t ballot) Lwt_stream.t) Lwt_stream.t
