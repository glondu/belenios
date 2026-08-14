open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_tyxml
open Belenios
open Tyxml_js.Html

type 'a t = {
  placeholder : string;
  lookup : string -> 'a Lwt.t;
  before : 'a -> Html_types.div_content Tyxml_js.Html.elt list;
  after : 'a -> Html_types.div_content Tyxml_js.Html.elt list;
}

let set_content container xs =
  container##.innerHTML := Js.string "";
  List.iter (fun x -> Dom.appendChild container (Tyxml_js.To_dom.of_node x)) xs

let make t =
  let before = div [] in
  let after = div [] in
  let set_container =
    let before_dom = Tyxml_js.To_dom.of_div before in
    let after_dom = Tyxml_js.To_dom.of_div after in
    fun prefix ->
      let@ () = Lwt.async in
      let* x = t.lookup prefix in
      set_content before_dom (t.before x);
      set_content after_dom (t.after x);
      Lwt.return_unit
  in
  set_container "";
  let handler (e : Dom_html.event Js.t) =
    let i =
      Js.coerce_opt e##.target Dom_html.CoerceTo.input (fun _ -> assert false)
    in
    set_container (Js.to_string i##.value);
    false
  in
  let lookup =
    input
      ~a:
        [
          a_oninput handler;
          a_placeholder t.placeholder;
          a_class [ "prefix-input" ];
        ]
      ()
  in
  div [ before; div [ lookup ]; after ]
