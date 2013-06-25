open Util

(* Setup group *)

let g = Z.of_string "14887492224963187634282421537186040801304008017743492304481737382571933937568724473847106029915040150784031882206090286938661464458896494215273989547889201144857352611058572236578734319505128042602372864570426550855201448111746579871811249114781674309062693442442368697449970648232621880001709535143047913661432883287150003429802392229361583608686643243349727791976247247948618930423866180410558458272606627111270040091203073580238905303994472202930783207472394578498507764703191288249547659899997131166130259700604433891232298182348403175947450284433411265966789131024573629546048637848902243503970966798589660808533";;
let p = Z.of_string "16328632084933010002384055033805457329601614771185955389739167309086214800406465799038583634953752941675645562182498120750264980492381375579367675648771293800310370964745767014243638518442553823973482995267304044326777047662957480269391322789378384619428596446446984694306187644767462460965622580087564339212631775817895958409016676398975671266179637898557687317076177218843233150695157881061257053019133078545928983562221396313169622475509818442661047018436264806901023966236718367204710755935899013750306107738002364137917426595737403871114187750804346564731250609196846638183903982387884578266136503697493474682071";;
let q = Z.of_string "61329566248342901292543872769978950870633559608669337131139375508370458778917";;
assert (Election.check_finite_field ~p ~q ~g);;

module G = (
  val Election.finite_field ~g ~p ~q : Signatures.GROUP with type t = Z.t
);;

(* Some helpers *)

let digits = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
let token_length = 14
let n58 = Z.of_int 58
let n53 = Z.of_int 53

let smjs_template = format_of_string "./stuff/derive_key.js %s %s"

let public_key_of_token uuid x =
  let ic = Printf.ksprintf Unix.open_process_in smjs_template x uuid in
  let hex = input_line ic in
  if Unix.(close_process_in ic <> WEXITED 0) then (
    Printf.eprintf "Error while running nodejs!";
    exit 2;
  );
  let x = Z.(of_string_base 16 hex mod q) in
  let y = G.(g **~ x) in
  Z.to_string y

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

(* Argument parsing *)

let dir = ref (Sys.getcwd ())
let uuid = ref None
let count = ref None
let file = ref None
let derive = ref None

let speclist = Arg.([
  "--dir", String (fun s -> dir := s), "directory where output will be written";
  "--uuid", String (fun s -> uuid := Some s), "UUID of the election";
  "--count", Int (fun i -> count := Some i), "number of credentials to generate";
  "--file", String (fun s -> file := Some s), "file with list of identities";
  "--derive", String (fun s -> derive := Some s), "derive public credential from given private one";
])

let usage_msg =
  Printf.sprintf "Usage: %s [--dir <dir>] --uuid <uuid> {--count <n> | --file <file> | --derive <privcred>}" Sys.argv.(0)

let anon_fun x =
  Printf.eprintf "I do not know what to do with %s!\n" x;
  exit 1

let () = Arg.parse speclist anon_fun usage_msg

let remove_dashes x =
  let n = String.length x in
  let res = Buffer.create n in
  for i = 0 to n-1 do
    let c = x.[i] in
    if c <> '-' then Buffer.add_char res c;
  done;
  Buffer.contents res

let uuid = match !uuid with
  | None ->
    Printf.eprintf "UUID is missing!\n";
    exit 1
  | Some u ->
    match Uuidm.of_string u with
      | Some _ -> remove_dashes u
      | None ->
        Printf.eprintf "UUID is invalid!\n";
        exit 1

let count, ids =
  match !count, !file, !derive with
    | Some i, None, None ->
      if i < 1 then (
        Printf.eprintf "You must generate at least one credential!\n";
        exit 1
      ); i, None
    | None, Some f, None ->
      let ic = open_in f in
      let rec loop accu =
        match (try Some (input_line ic) with End_of_file -> None) with
          | Some "" -> loop accu
          | Some x -> loop (x::accu)
          | None -> List.rev accu
      in
      let res = loop [] in
      close_in ic;
      List.length res, Some res
    | None, None, Some d ->
      print_endline (public_key_of_token uuid d);
      exit 0
    | None, None, None ->
      Printf.eprintf "Nothing to do: use --count, --file or --derive!\n";
      exit 1
    | _, _, _ ->
      Printf.eprintf "Conflicting options!\n";
      exit 1
;;

(* The generation itself, if requested *)

let prng = Cryptokit.Random.(pseudo_rng (string secure_rng 16))
let random_char () = int_of_char (Cryptokit.Random.string prng 1).[0]

let generate_raw_token () =
  let res = String.create token_length in
  let rec loop i accu =
    if i < token_length then (
      let digit = random_char () mod 58 in
      res.[i] <- digits.[digit];
      loop (i+1) Z.(n58 * accu + of_int digit)
    ) else (res, accu)
  in loop 0 Z.zero

let generate_token () =
  let (raw, value) = generate_raw_token () in
  let checksum = 53 - Z.(to_int (value mod n53)) in
  raw ^ String.make 1 digits.[checksum]

let private_credentials =
  let rec loop i accu =
    if i > 0 then loop (i-1) (generate_token () :: accu)
    else accu
  in loop count []

let public_credentials =
  List.map (public_key_of_token uuid) private_credentials

let hashed_credentials = option_map (fun ids ->
  List.map2 (fun id cred ->
    Printf.sprintf "%s %s" (sha256_hex cred) id
  ) ids public_credentials
) ids

(* Save to files *)

let timestamp = Printf.sprintf "%.0f" (Unix.time ())

let pub =
  "public credentials",
  timestamp ^ ".public",
  0o444,
  List.sort compare public_credentials

let priv =
  let kind, creds = match ids with
    | None -> "private credentials", private_credentials
    | Some ids -> "private credentials with ids",
      List.map2 (fun id cred ->
        Printf.sprintf "%s %s" cred id
      ) ids private_credentials
  in
  kind,
  timestamp ^ ".private",
  0o400,
  List.sort compare creds

let hashed = option_map (fun h ->
  "hashed credentials with ids",
  timestamp ^ ".hashed",
  0o400,
  List.sort compare h
) hashed_credentials

let output_endline oc x =
  output_string oc x;
  output_char oc '\n'

let save (kind, filename, perm, thing) =
  let full_filename = Filename.concat !dir filename in
  let oc = open_out_gen [
    Open_wronly; Open_creat; Open_excl
  ] perm full_filename in
  List.iter (output_endline oc) thing;
  close_out oc;
  Printf.printf "%d %s saved to %s\n%!" count kind full_filename;;

save pub;;
save priv;;
ignore (option_map save hashed);;
