open Web_serializable_t
open Web_signatures

val register : (module AUTH_SERVICES) -> auth_config list -> unit

include AUTH_SERVICES
