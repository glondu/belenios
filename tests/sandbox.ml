open Helios_datatypes_t

module type TYPES = sig
  type 'a t
  val read : 'a t -> Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
  val write : 'a t -> Bi_outbuf.t -> 'a -> unit
  val election : election t
  val private_key : private_key t
  val vote : vote t
  val encrypted_tally : encrypted_tally t
  val partial_decryption : partial_decryption t
end

module Types : TYPES = struct
  open Helios_datatypes_j
  type 'a t = (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) * (Bi_outbuf.t -> 'a -> unit)
  let read = fst
  let write = snd
  let election = (read_election, write_election)
  let private_key = (read_private_key, write_private_key)
  let vote = (read_vote, write_vote)
  let encrypted_tally = (read_encrypted_tally, write_encrypted_tally)
  let partial_decryption = (read_partial_decryption, write_partial_decryption)
end

let load typ fname =
  let i = open_in fname in
  let buf = Lexing.from_channel i in
  let lex = Yojson.init_lexer ~fname () in
  let result = Types.read typ lex buf in
  close_in i;
  result

let save typ fname x =
  let o = open_out fname in
  let buf = Bi_outbuf.create_channel_writer o in
  Types.write typ buf x;
  Bi_outbuf.flush_channel_writer buf;
  close_out o

let () =
  assert (Sys.command "mkdir -p _build/tests/data" = 0)

let load_and_check typ fname =
  let one_thing = load typ fname in
  save typ (Filename.concat "_build" fname) one_thing;
  let r = Printf.ksprintf Sys.command "bash -c '\
    diff -u <(json_pp < %s) <(json_pp < _build/%s)
    '" fname fname
  in
  assert (r = 0);
  one_thing

let one_election = load_and_check Types.election "tests/data/election.json"
let one_private_key = load_and_check Types.private_key "tests/data/private-key.json"
let vote_1 = load_and_check Types.vote "tests/data/vote-emacs-1.json"
let vote_2 = load_and_check Types.vote "tests/data/vote-emacs-2.json"
let encrypted_tally = load_and_check Types.encrypted_tally "tests/data/encrypted-tally.json"
let one_partial_decryption = load_and_check Types.partial_decryption "tests/data/partial-decryption.json"
