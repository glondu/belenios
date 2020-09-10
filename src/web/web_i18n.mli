val get_lang_gettext : string -> (module Web_i18n_sig.GETTEXT) Lwt.t
val get_preferred_gettext : unit -> (module Web_i18n_sig.GETTEXT) Lwt.t
