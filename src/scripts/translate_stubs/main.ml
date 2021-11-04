open GettextTypes

module StringMap = Map.Make (String)

let build_string_map mo =
  let map, _ =
    GettextMo.fold_mo Ignore
      (fun translation accu ->
        match translation with
        | Singular (str_id, str) ->
           StringMap.add str_id str accu
        | Plural (str_id, _, _) ->
           Printf.ksprintf failwith "unsupported: Plural(%S, _, _)" str_id
      ) StringMap.empty mo
  in
  map

let extract_string = function
  | `String x -> x
  | _ -> failwith "string expected"

let extract_assoc = function
  | `Assoc x -> List.map (fun (x, y) -> x, extract_string y) x
  | _ -> failwith "object expected"

let translate_stubs reference target mo =
  let reference = extract_assoc reference in
  let target = extract_assoc target in
  let new_strings =
     List.filter_map
       (fun (str_id, str) ->
         match List.assoc_opt str_id target with
         | Some _ -> None
         | None ->
            match StringMap.find_opt str mo with
            | None -> None
            | Some str -> Some (str_id, str)
       ) reference
  in
  `Assoc (List.map (fun (x, y) -> x, `String y) (target @ new_strings))

let () =
  let reference = Yojson.Safe.from_file Sys.argv.(1) in
  let mo = build_string_map Sys.argv.(2) in
  let target = Yojson.Safe.from_channel stdin in
  translate_stubs reference target mo
  |> Yojson.Safe.to_channel stdout
