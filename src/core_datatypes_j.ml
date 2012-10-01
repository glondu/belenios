open Core_datatypes_t

(** {1 Serializers for type number} *)

let write_number buf n =
  Bi_outbuf.add_char buf '"';
  Bi_outbuf.add_string buf (Z.to_string n);
  Bi_outbuf.add_char buf '"'

let string_of_number ?(len=2048) n =
  let buf = Bi_outbuf.create len in
  write_number buf n;
  Bi_outbuf.contents buf

let number_of_json = function
  | `String s -> Z.of_string s
  | _ -> assert false

let read_number state buf =
  number_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)

let number_of_string s =
  number_of_json (Yojson.Safe.from_string s)

(** {1 Serializers for type uuid} *)

let write_uuid buf n =
  Bi_outbuf.add_char buf '"';
  Bi_outbuf.add_string buf (Uuidm.to_string n);
  Bi_outbuf.add_char buf '"'

let string_of_uuid ?(len=38) n =
  let buf = Bi_outbuf.create len in
  write_uuid buf n;
  Bi_outbuf.contents buf

let uuid_of_json = function
  | `String s ->
    (match Uuidm.of_string s with Some s -> s | _ -> assert false)
  | _ -> assert false

let read_uuid state buf =
  uuid_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)

let uuid_of_string s =
  uuid_of_json (Yojson.Safe.from_string s)

(** {1 Serializers for type datetime} *)

open CalendarLib
let datetime_format = "%Y-%m-%d %H:%M:%S"

let write_datetime buf n =
  Bi_outbuf.add_char buf '"';
  Bi_outbuf.add_string buf (Printer.Precise_Fcalendar.sprint datetime_format n);
  let ts = Printf.sprintf "%.6f" (Fcalendar.Precise.to_unixfloat n) in
  let i = String.index ts '.' in
  Bi_outbuf.add_substring buf ts i (String.length ts - i);
  Bi_outbuf.add_char buf '"'

let string_of_datetime ?(len=28) n =
  let buf = Bi_outbuf.create len in
  write_datetime buf n;
  Bi_outbuf.contents buf

let datetime_of_json = function
  | `String s ->
    let i = String.index s '.' in
    let l = Printer.Precise_Fcalendar.from_fstring datetime_format (String.sub s 0 i) in
    let r = float_of_string ("0" ^ String.sub s i (String.length s-i)) in
    Fcalendar.Precise.add l (Fcalendar.Precise.Period.second r)
  | _ -> assert false

let read_datetime state buf =
  datetime_of_json (Yojson.Safe.from_lexbuf ~stream:true state buf)

let datetime_of_string s =
  datetime_of_json (Yojson.Safe.from_string s)
