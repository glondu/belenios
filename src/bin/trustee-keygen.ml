open Util
open Serializable_t

module RunTrusteeKeygen (X : sig end) = struct

(* Setup group *)

module G = Election.DefaultGroup;;
assert (Election.check_finite_field G.group);;

module M = Election.MakeSimpleMonad(G);;

(* Generate key *)

module KG = Election.MakeSimpleDistKeyGen(G)(M);;
let private_key, public_key = KG.generate_and_prove () ();;
assert (KG.check public_key);;

(* Save to file *)

let id = String.sub
  (sha256_hex (Z.to_string public_key.trustee_public_key))
  0 8 |> String.uppercase
;;

Printf.printf "Keypair %s has been generated\n%!" id;;

let pubkey =
  "public",
  id ^ ".public",
  0o444,
  public_key,
  Serializable_j.write_trustee_public_key Serializable_builtin_j.write_number

let privkey =
  "private",
  id ^ ".private",
  0o400,
  private_key,
  Serializable_builtin_j.write_number

let save (kind, filename, perm, thing, writer) =
  let oc = open_out_gen [Open_wronly; Open_creat] perm filename in
  let ob = Bi_outbuf.create_channel_writer oc in
  writer ob thing;
  Bi_outbuf.flush_channel_writer ob;
  close_out oc;
  Printf.printf "%s key saved to %s\n%!" (String.capitalize kind) filename;
  (* set permissions in the unlikely case where the file already existed *)
  Unix.chmod filename perm;;

save pubkey;;
save privkey;;

end


let () =
  let module X = RunTrusteeKeygen (struct end) in
  ()
