open Serializable_compat_t

type user = {
  user_name : string;
  user_type : string;
}

type 'a result = {
  encrypted_tally : 'a encrypted_tally;
  partial_decryptions : 'a partial_decryption array;
  result : raw_result;
}

type election_data = {
  raw : string;
  fingerprint : string;
  election : Z.t election;
  public_keys : Z.t trustee_public_key array;
  election_result : Z.t result option;
  admin : user;
  private_p : bool;
  featured_p : bool;
  state : election_state;
}

val hashB : string -> string

val load_elections_and_votes :
  string -> (election_data * (string * Z.t ballot) Lwt_stream.t) Lwt_stream.t
