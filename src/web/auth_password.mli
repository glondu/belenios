type config = { db : string }
include Web_signatures.AUTH_SYSTEM with type config := config
