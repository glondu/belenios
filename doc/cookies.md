# Cookie handling in belenios-server

Cookies are used only by [Eliom's sessions and server-side
state](https://ocsigen.org/eliom/latest/manual/server-state). Use of
this feature can be audited by looking at uses of modules
`Eliom_reference` and `Eliom_state` in the OCaml code.

Belenios uses only volatile data states (non-persistent Eliom
references). This means that all cookies (regardless of their
client-side validity date) are invalidated on server restart.

Belenios uses Eliom default session timeouts, which is one hour, even
though cookie validity dates are set 10 years in the future. This
means a cookie stops being valid after one hour of inactivity,
regardless of their validity date as seen by the browser.

Server-side state can be either _services_ (code that is called on
reaction to specific HTTP requests) or generic _data_.

Each server-side state is either global, and shared by all clients
(implemented with e.g. in-memory variables or on-disk files), or
specific to a limited _scope_ (implemented with cookies). Each scope
used by Belenios maps to a cookie which contains a nonce used to
identify the client session on the server.

Belenios uses the following data scopes:
- the default one (cookie `eliomdatasession|||ref|`)
- `belenios` (cookie `eliomdatasession||||belenios`)
- `belenios-auth` (cookie `eliomdatasession||||belenios-auth`)
- `belenios-auth-email` (cookie `eliomdatasession||||belenios-auth-email`)
- `belenios-auth-oidc` (cookie `eliomdatasession||||belenios-auth-oidc`)

There is also the default service scope, materialized by cookie
`eliomservicesession|||ref|`, but it is not used in Belenios.

When a scope is discarded (or times out), its corresponding cookie is
immediately invalidated server-side, and client-side in the next HTTP
reply.

Each Eliom reference belongs to one scope, and they are all created in
modules `Web_state` and `Web_auth*`.

The default scope is used only for security-insensitive data related
to anonymous visitors (e.g. voters): the cookie consent and the
language. It is never explicitly discarded (but is subject to
timeout).

The `belenios-auth*` scopes are used only for the authentication
process itself, and are discarded at the end of a successful
authentication. Some protocols use their own scope, which is also
discarded at the end of a successful authentication.

The `belenios` scope groups all other server-side state which might be
security-sensitive, including:
- the logged-in administrator
- the logged-in voter
- the currently-being-authenticated voter's ballot
- temporary data needed by various multi-page workflows such as
  account self-service

It is explicitly discarded on the following events:
- when accessing an election home page
- after a logout
- when account self-service is used:
  - after a successful account creation
  - after a successful change of e-mail or password

# API token handling

API tokens are used for authenticated API access and are handled
outside of Eliom's sessions, but are somehow related to cookies so
they are also documented here.

When an administrator logs in, a token is generated with a validity
period of one day. It is invalidated when they explicitly log out.

API tokens are stored in memory, so their validity is lost on server
restarts.
