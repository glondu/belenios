open Serializable_builtin_t

(** {1 Serializers for type number} *)

val write_number : Bi_outbuf.t -> number -> unit
val string_of_number : ?len:int -> number -> string
val read_number : Yojson.Safe.lexer_state -> Lexing.lexbuf -> number
val number_of_string : string -> number

(** {1 Serializers for type uuid} *)

val write_uuid : Bi_outbuf.t -> uuid -> unit
val string_of_uuid : ?len:int -> uuid -> string
val read_uuid : Yojson.Safe.lexer_state -> Lexing.lexbuf -> uuid
val uuid_of_string : string -> uuid

(** {1 Serializers for type datetime} *)

val write_datetime : Bi_outbuf.t -> datetime -> unit
val string_of_datetime : ?len:int -> datetime -> string
val read_datetime : Yojson.Safe.lexer_state -> Lexing.lexbuf -> datetime
val datetime_of_string : string -> datetime

(** {1 Serializers for type int_or_null} *)

val write_int_or_null : Bi_outbuf.t -> int_or_null -> unit
val string_of_int_or_null : ?len:int -> int_or_null -> string
val read_int_or_null : Yojson.Safe.lexer_state -> Lexing.lexbuf -> int_or_null
val int_or_null_of_string : string -> int_or_null
