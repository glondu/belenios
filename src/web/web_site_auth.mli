open Web_serializable_t
open Web_signatures

val configure : auth_config list -> unit

include AUTH_SERVICES
