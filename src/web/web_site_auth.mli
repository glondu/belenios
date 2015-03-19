open Web_serializable_t
open Web_signatures

val register : (module AUTH_SERVICES) -> (module AUTH_LINKS) -> auth_config list -> unit

include AUTH_SERVICES
