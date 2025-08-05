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
types. They refer to types defined in `src/common/api/serializable.atd`.

## `configuration` (anybody)

### `GET`: unit -> configuration

## `send-message/internal` (anybody)

### `POST`: message_payload -> json

## `send-message/external` (anybody)

### `POST`: message_payload -> json

## `send-message/default` (anybody)

### `POST`: message_payload -> json

## `account` (administrator)

### `GET`: unit -> api_account
### `PUT`: api_account -> unit

## `elections` (administrator)

### `GET`: unit -> summary_list
### `POST`: draft -> uuid

## `elections/$UUID/draft`

### `GET`: unit -> draft (anybody)
### `PUT`: draft -> unit (administrator)
### `POST`: draft_request -> unit (administrator)

## `elections/$UUID/draft/election` (anybody)

### `GET`: unit -> json

## `elections/$UUID/draft/voters`

### `GET`: unit -> voter_list (administrator or credential authority)
### `PUT`: voter_list -> unit (administrator)
### `POST`: voters_request -> unit (administrator)

## `elections/$UUID/draft/passwords` (administrator)

### `GET`: unit -> string_list
### `POST`: string_list -> unit

## `elections/$UUID/draft/credentials/token` (administrator)

### `GET`: unit -> string

## `elections/$UUID/draft/credentials/public`

### `GET`: unit -> public_credentials (anybody)
### `POST`: public_credentials -> unit (credential authority)

## `elections/$UUID/draft/credentials/private` (administrator)

### `GET`: unit -> private_credentials

## `elections/$UUID/draft/trustee` (trustee)

### `GET`: unit -> trustee_status
### `POST`: json -> unit

## `elections/$UUID/draft/trustees`

### `GET`: unit -> draft_trustees (administrator or nobody)
### `POST`: trustees_request -> unit (administrator)

## `elections/$UUID/draft/trustees/$ADDRESS` (administrator)

### `DELETE`: unit -> unit

## `elections/$UUID/draft/status` (administrator)

### `GET`: unit -> draft_status

## `elections/$UUID`

### `GET`: unit -> election_status (anybody)
### `POST`: admin_request -> unit (administrator)
### `DELETE`: unit -> unit (administrator)

## `elections/$UUID/audit-cache` (anybody)

### `GET`: unit -> audit_cache

## `elections/$UUID/sealing-log` (administrator)

### `GET`: unit -> json-stream

## `elections/$UUID/election` (anybody)

### `GET`: unit -> json

## `elections/$UUID/archive` (anybody)

### `GET`: unit -> bel

## `elections/$UUID/trustees` (anybody)

### `GET`: unit -> 'a trustees

## `elections/$UUID/automatic-dates`

### `GET`: unit -> election_auto_dates
### `PUT`: election_auto_dates -> unit (administrator)

## `elections/$UUID/voters` (administrator)

### `GET`: unit -> voter_list

## `elections/$UUID/records` (administrator)

### `GET`: unit -> records

## `elections/$UUID/trustee` (trustee)

### `GET`: unit -> tally_trustee
### `POST`: json -> unit

## `elections/$UUID/nh-ciphertexts` (anybody)

### `GET`: unit -> 'a nh_ciphertexts

## `elections/$UUID/encrypted-tally` (anybody)

### `GET`: unit -> 'a encrypted_tally

## `elections/$UUID/shuffles` (administrator)

### `GET`: unit -> shuffles

## `elections/$UUID/shuffles/$ADDRESS` (administrator)

### `POST`: shuffler_request -> unit

## `elections/$UUID/partial-decryptions`

### `GET`: unit -> partial_decryptions (administrator)

## `elections/$UUID/objects/$HASH` (anybody)

### `GET`: unit -> json

## `elections/$UUID/archive-header` (anybody)

### `GET`: unit -> archive_header

## `elections/$UUID/last-event` (anybody)

### `GET`: unit -> last_event

## `elections/$UUID/roots` (anybody)

### `GET`: unit -> roots

## `elections/$UUID/ballots`

### `GET`: unit -> ballots_with_weights (anybody)

### `POST`: ballot -> unit (voter)

Experimental!

Voters using this endpoint must use a Base64-encoded JSON structure as
API token. The structure depends on the authentication mode (only
dummy and password are supported at the moment).
