open Js_of_ocaml

let translations = ref (Js.Unsafe.obj [||] : Js.Unsafe.any)

let init component lang =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let%lwt request = Printf.ksprintf get "static/locales/%s/%s.json" component lang in
  if request.code = 200 then
    translations := Js._JSON##parse (Js.string request.content);
  Lwt.return_unit

module Gettext = struct
  let s_ str_id =
    let str = Js.Unsafe.get !translations (Js.string str_id) in
    Js.Optdef.case str
      (fun () -> str_id)
      (fun x -> Js.to_string (Js.Unsafe.get x 0))

  let f_ str_id =
    let str = s_ (string_of_format str_id) in
    Scanf.format_from_string str str_id
end
