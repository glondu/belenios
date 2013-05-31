open Serializable_t

type user = {
  user_name : string;
  user_type : string;
}

type acl =
  | Any
  | Restricted of (user -> bool Lwt.t)

type election_data = {
  raw : string;
  fingerprint : string;
  election : ff_pubkey election;
  public_keys : Z.t trustee_public_key array;
  election_result : Z.t result option;
  author : user;
  featured_p : bool;
  can_read : acl;
  can_vote : acl;
  can_admin : acl;
}

val hashB : string -> string

val load_elections_and_votes :
  string -> (election_data * (string * Z.t ballot) Lwt_stream.t) Lwt_stream.t
