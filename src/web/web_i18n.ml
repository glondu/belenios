let get_lang = function
  | "fr" -> (module Web_l10n_fr : Web_i18n_sig.LocalizedStrings)
  | "de" -> (module Web_l10n_de : Web_i18n_sig.LocalizedStrings)
  | "ro" -> (module Web_l10n_ro : Web_i18n_sig.LocalizedStrings)
  | "it" -> (module Web_l10n_it : Web_i18n_sig.LocalizedStrings)
  | _ -> (module Web_l10n_en : Web_i18n_sig.LocalizedStrings)

let langs = Hashtbl.create 10

module type LANG = sig
  val lang : string
end

module Belenios_Gettext (L : LANG) (T : GettextTranslate.TRANSLATE_TYPE) : Web_i18n_sig.GETTEXT = struct
  let lang = L.lang
  open GettextCategory
  open GettextTypes
  let t =
    {
      failsafe = Ignore;
      textdomains = MapTextdomain.empty;
      categories = MapCategory.empty;
      language = Some L.lang;
      codeset = "UTF-8";
      path = [];
      default = "belenios";
    }
  let u = T.create t ("po/" ^ L.lang ^ ".mo") (fun x -> x)
  let s_ str = T.translate u false str None
  let f_ str = Scanf.format_from_string (T.translate u true (string_of_format str) None) str
  let sn_ str str_plural n = T.translate u false str (Some (str_plural, n))
  let fn_ str str_plural n = Scanf.format_from_string (T.translate u true (string_of_format str) (Some (string_of_format str_plural, n))) str
end

let default_gettext =
  let module L = struct let lang = "en" end in
  let module G = Belenios_Gettext (L) (GettextTranslate.Dummy) in
  (module G : Web_i18n_sig.GETTEXT)

let register lang =
  let module L = struct let lang = lang end in
  let module G = Belenios_Gettext (L) (GettextTranslate.Map) in
  Hashtbl.add langs lang (module G : Web_i18n_sig.GETTEXT)

let () =
  Lwt_main.run (Lwt_io.lines_of_file "po/LINGUAS" |> Lwt_stream.to_list)
  |> List.iter register

let get_lang_gettext lang =
  try Hashtbl.find langs lang
  with Not_found -> default_gettext
