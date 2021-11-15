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

`PUT`, `POST` and `DELETE` requests support the `If-Match` header: if
present, it must be the SHA256-Base64Compact of what would be returned
by a `GET` request (if supported) on the same endpoint. This is
typically used to avoid silent conflicts. An HTTP error 412
(Precondition Failed) is returned when there is no match.

# Endpoints

Here, we give for each endpoint the available methods and their
types. They refer to types defined in `src/api/serializable.atd`.

## `configuration` (anybody)

* `GET`: unit -> configuration

## `account` (administrator)

* `GET`: unit -> api_account
* `PUT`: api_account -> unit

## `drafts` (administrator)

* `GET`: unit -> summary_list
* `POST`: draft -> uuid

## `drafts/$UUID`

* `GET`: unit -> draft (administrator or credential authority)
* `PUT`: draft -> unit (administrator)
* `POST`: draft_request -> unit (administrator)
* `DELETE`: unit -> unit (administrator)

## `drafts/$UUID/voters`

* `GET`: unit -> voter_list (administrator or credential authority)
* `PUT`: voter_list -> unit (administrator)

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
* `POST`: trustees_request -> unit

## `drafts/$UUID/trustees/$ADDRESS` (administrator)

* `DELETE`: unit -> unit

## `drafts/$UUID/status` (administrator)

* `GET`: unit -> status

## `elections` (administrator)

* `GET`: unit -> summary_list

## `elections/$UUID`

* `GET`: unit -> election_status (anybody)
* `POST`: admin_request -> unit (administrator)
* `DELETE`: unit -> unit (administrator)

## `elections/$UUID/election` (anybody)

* `GET`: unit -> json

## `elections/$UUID/voters` (administrator)

* `GET`: unit -> voter_list

## `elections/$UUID/records` (administrator)

* `GET`: unit -> records

## `elections/$UUID/shuffles` (administrator)

* `GET`: unit -> shuffles

## `elections/$UUID/shuffles/$ADDRESS` (administrator)

* `POST`: shuffler_request -> unit

## `elections/$UUID/partial-decryptions`

* `GET`: unit -> partial_decryptions (administrator)
