open Lwt.Syntax
open Js_of_ocaml

let translations = ref (Js.Unsafe.obj [||] : Js.Unsafe.any)

let init dir component lang =
  let open Js_of_ocaml_lwt.XmlHttpRequest in
  let* request = Printf.ksprintf get "%s/locales/%s/%s.json" dir component lang in
  if request.code = 200 then
    translations := Js._JSON##parse (Js.string request.content);
  Lwt.return_unit

let auto_init component =
  let lang = Js.to_string (Js.Unsafe.pure_js_expr "belenios_lang") in
  let dir = Js.to_string (Js.Unsafe.pure_js_expr "belenios_dir") in
  init dir component lang

module Gettext = struct
  let lang = "en_devel" (* FIXME *)

  let s_ str_id =
    let str = Js.Unsafe.get !translations (Js.string str_id) in
    Js.Optdef.case str
      (fun () -> str_id)
      (fun x -> Js.to_string (Js.Unsafe.get x 0))

  let f_ str_id =
    let str = s_ (string_of_format str_id) in
    Scanf.format_from_string str str_id
end
