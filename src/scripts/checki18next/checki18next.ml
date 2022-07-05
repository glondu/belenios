let files_of_dir dir =
  let dir = Unix.opendir dir in
  let rec loop accu =
    match Unix.readdir dir with
    | f -> loop (f :: accu)
    | exception End_of_file -> Unix.closedir dir; accu
  in
  loop []

let find_special_strings s =
  let n = String.length s in
  let rec lookup_special accu i =
    if i < n then (
      match s.[i] with
      | '{' -> lookup_variable accu (i + 1)
      | '$' -> lookup_call accu (i + 1)
      | _ -> lookup_special accu (i + 1)
    ) else accu
  and lookup_variable accu i =
    let rec loop depth j =
      if j < n then (
        match s.[j] with
        | '{' -> loop (depth + 1) (j + 1)
        | '}' ->
           let depth = depth - 1 in
           if depth = 0 then
             lookup_special (String.sub s (i - 1) (j - i + 2) :: accu) (j + 1)
           else
             loop depth (j + 1)
        | _ -> loop depth (j + 1)
      ) else (String.sub s (i - 1) (j - i + 1) :: accu)
    in
    loop 1 i
  and lookup_call accu i =
    let rec loop depth j =
      if j < n then (
        match s.[j] with
        | '(' -> loop (depth + 1) (j + 1)
        | ')' ->
           let depth = depth - 1 in
           if depth = 0 then
             lookup_special (String.sub s (i - 1) (j - i + 2) :: accu) (j + 1)
           else
             loop depth (j + 1)
        | _ -> loop depth (j + 1)
      ) else (String.sub s (i - 1) (j - i + 1) :: accu)
    in
    loop 0 i
  in
  lookup_special [] 0

let make_reference = function
  | `Assoc o ->
     `Assoc
       (List.filter_map
          (fun (k, s) ->
            match s with
            | `String s ->
               (match find_special_strings s with
                | [] -> None
                | xs -> Some (k, `List (List.map (fun x -> `String x) xs))
               )
            | _ -> assert false
          ) o
       )
  | _ -> assert false

let check_substring ~substring s =
  let regexp = Str.regexp_string substring in
  match Str.search_forward regexp s 0 with
  | _ -> ()
  | exception Not_found -> Printf.ksprintf failwith "%S does not appear in %S" substring s

let check (reference : Yojson.Safe.t) (json : Yojson.Safe.t) =
  match reference, json with
  | `Assoc reference, `Assoc json ->
     List.iter
       (fun (k, specials) ->
         match specials with
         | `List specials ->
            (match List.assoc_opt k json with
             | None -> ()
             | Some (`String to_check) ->
                List.iter
                  (function
                   | `String substring -> check_substring ~substring to_check
                   | _ -> assert false
                  )
                  specials
             | _ -> assert false
            )
         | _ -> assert false
       ) reference
  | _ -> assert false

let dir = ref "."
let make_reference_ref = ref false

let spec =
  let open Arg in
  [
    "--dir", Set_string dir, "dir with translations";
    "--make-reference", Set make_reference_ref, "(re-)generate reference file";
  ]

let () =
  Arg.parse spec (fun _ -> assert false) "Check i18next translation files."

let ( // ) = Filename.concat

let () =
  if !make_reference_ref then (
    Yojson.Safe.from_channel stdin
    |> make_reference
    |> Yojson.Safe.to_channel stdout
  ) else (
    let reference = Yojson.Safe.from_channel stdin in
    let translations =
      files_of_dir !dir
      |> List.filter (fun x -> x <> "." && x <> "..")
      |> List.map (fun x -> Yojson.Safe.from_file (!dir // x))
    in
    List.iter (check reference) translations
)
