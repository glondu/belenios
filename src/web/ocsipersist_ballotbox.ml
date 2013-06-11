open Util

module MakeBallotBox (G : Signatures.GROUP)
  (U : sig val uuid : Uuidm.t end) = struct

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail

  let suffix = "_" ^ String.map (function
    | '-' -> '_'
    | c -> c
  ) (Uuidm.to_string U.uuid)

  let ballot_table = Ocsipersist.open_table ("ballots" ^ suffix)
  let record_table = Ocsipersist.open_table ("records" ^ suffix)

  let prng = Lwt_preemptive.detach (fun () ->
    Cryptokit.Random.(pseudo_rng (string secure_rng 16))
  ) ()

  let random q =
    let size = Z.size q * Sys.word_size / 8 in
    lwt prng = prng in
    let r = Cryptokit.Random.string prng size in
    return Z.(of_bits r mod q)

  type ballot = string
  type record = string * Serializable_builtin_t.datetime

  let cast b (user, date) =
    Ocsipersist.add ballot_table (Common.hashB b) b >>
    Ocsipersist.add record_table user date

  let fold_ballots f x =
    Ocsipersist.fold_step (fun k v x -> f v x) ballot_table x

  let fold_records f x =
    Ocsipersist.fold_step (fun k v x -> f (k, v) x) record_table x

  let turnout = Ocsipersist.length ballot_table
end
