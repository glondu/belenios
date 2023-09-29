(* For each source string that has no translation, copy msgid as translation *)
(* Usage: ocaml fill_en_po.ml < en.po | sponge en.po *)

#use "topfind";;
#require "re";;

let rex = Re.Pcre.regexp "^msgid (.*)$"

let rec process_msgid ic oc =
  match input_line ic with
  | exception End_of_file -> ()
  | line ->
     match Re.Pcre.exec ~rex line with
     | s -> process_msgstr (Re.Pcre.get_substring s 1) ic oc
     | exception Not_found ->
        Printf.fprintf oc "%s\n%!" line;
        process_msgid ic oc

and process_msgstr msgid ic oc =
  match input_line ic with
  | exception End_of_file -> failwith "unexpected eof"
  | line ->
     if line = "msgstr \"\"" then
       process_empty msgid ic oc
     else (
       Printf.fprintf oc "msgid %s\n%s\n%!" msgid line;
       process_msgid ic oc
     )

and process_empty msgid ic oc =
  match input_line ic with
  | exception End_of_file ->
     Printf.fprintf oc "msgid %s\nmsgstr %s\n%!" msgid msgid;
  | line ->
     if line = "" then (
       Printf.fprintf oc "msgid %s\nmsgstr %s\n\n%!" msgid msgid;
       process_msgid ic oc
     ) else (
       Printf.fprintf oc "msgid %s\nmsgstr \"\"\n%s\n" msgid line;
       process_msgid ic oc
     )

let () = process_msgid stdin stdout
