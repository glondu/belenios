open Util
open Serializable_t

module MakeLwtRandom (G : Signatures.GROUP) = struct

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail

  let prng = Lwt_preemptive.detach (fun () ->
    Cryptokit.Random.(pseudo_rng (string secure_rng 16))
  ) ()

  let random q =
    let size = Z.size q * Sys.word_size / 8 in
    lwt prng = prng in
    let r = Cryptokit.Random.string prng size in
    return Z.(of_bits r mod q)

end

exception Serialization of exn
exception ProofCheck

module type LWT_ELECTION = Signatures.ELECTION
  with type elt = Z.t
  and type 'a m = 'a Lwt.t

module MakeBallotBox (E : LWT_ELECTION) = struct

  let suffix = "_" ^ String.map (function
    | '-' -> '_'
    | c -> c
  ) (Uuidm.to_string E.election_params.e_uuid)

  let ballot_table = Ocsipersist.open_table ("ballots" ^ suffix)
  let record_table = Ocsipersist.open_table ("records" ^ suffix)

  type ballot = string
  type record = string * Serializable_builtin_t.datetime

  let cast rawballot (user, date) =
    lwt ballot =
      try Lwt.return (
        Serializable_j.ballot_of_string
          Serializable_builtin_j.read_number rawballot
      ) with e -> Lwt.fail (Serialization e)
    in
    if E.check_ballot ballot then (
      Ocsipersist.add ballot_table (Common.hashB rawballot) rawballot >>
      Ocsipersist.add record_table user date
    ) else (
      Lwt.fail ProofCheck
    )


  let fold_ballots f x =
    Ocsipersist.fold_step (fun k v x -> f v x) ballot_table x

  let fold_records f x =
    Ocsipersist.fold_step (fun k v x -> f (k, v) x) record_table x

  let turnout = Ocsipersist.length ballot_table
end
