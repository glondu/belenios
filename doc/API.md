# Public API of the Belenios web server

Logged in administrators can get an "administrator" API token at
`/api-token`. This token expires when the administrator logs out, or
after 24 hours (or when the server is restarted).

For each draft, a token is generated for the credential authority. It
is embedded in the link sent by the administrator to the credential
authority.

The token (of the administrator or of the credential authority) must
be given in an HTTP header:

    Authorization: Bearer $API_TOKEN

An HTTP error 401 (Unauthorized) is returned when the token is
invalid.

The root of the API is at `/api/`. All endpoints below are relative to
this root.

# Endpoints

Here, we give for each endpoint the available methods and their
types. They refer to types defined in `src/api/serializable.atd`.

## `drafts` (administrator)

* `GET`: unit -> summary_list
* `POST`: draft -> uuid

## `drafts/$UUID` (administrator or credential authority)

* `GET`: unit -> draft
* `PUT`: draft -> unit
* `DELETE`: unit -> unit

## `drafts/$UUID/voters` (administrator or credential authority)

* `GET`: unit -> voter_list
* `PUT`: voter_list -> unit

## `drafts/$UUID/passwords` (administrator)

* `GET`: unit -> voter_list
* `POST`: voter_list -> unit

## `drafts/$UUID/credentials` (administrator or credential authority)

* `GET`: unit -> credentials
* `POST`: credential_list -> unit

## `drafts/$UUID/trustees-mode` (administrator)

* `GET`: unit -> trustees_mode
* `PUT`: trustees_mode -> unit

## `drafts/$UUID/trustees` (administrator)

* `GET`: unit -> trustees
* `POST`: trustee -> unit

## `drafts/$UUID/trustees/$ADDRESS` (administrator)

* `DELETE`: unit -> unit
