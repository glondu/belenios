open GettextTypes

module StringMap = Map.Make (String)

let build_string_map mo =
  let map, _ =
    GettextMo.fold_mo Ignore
      (fun translation accu ->
        match translation with
        | Singular (str_id, str) ->
           StringMap.add str_id (str, None) accu
        | Plural (str_id, str_plural, lst) ->
           StringMap.add str_id (str_plural, Some lst) accu
      ) StringMap.empty mo
  in
  map

let () =
  build_string_map Sys.argv.(1)
  |> StringMap.bindings
  |> List.map
       (fun (str_id, (str, plural)) ->
         match plural with
         | None -> str_id, `List [`String str]
         | Some lst -> str_id, `List [`String str; `List (List.map (fun x -> `String x) lst)])
  |> (fun x -> `Assoc x)
  |> Yojson.Safe.to_channel stdout
