open Web_serializable_t
open Web_signatures

val register : (module LOGIN_TEMPLATES) -> auth_config list -> unit

include AUTH_SERVICES
