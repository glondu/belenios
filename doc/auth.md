# External authentication protocols supported by Belenios

## CAS

```mermaid
sequenceDiagram
    Browser->>+Belenios: login()
    Belenios->>+Browser: auth_url, belenios_url
    Browser->>+Auth: login(belenios_url)
    Auth->>+Browser: ticket, belenios_url
    Browser->>+Belenios: confirm_login(ticket)
    Belenios-->>+Auth: validate(ticket)
    Auth-->>+Belenios: data
    Belenios->>+Browser: ok
```

## OpenID Connect

```mermaid
sequenceDiagram
    Browser->>+Belenios: login()
    Belenios-->>+Auth: get_configuration()
    Auth-->>+Belenios: auth_url
    Belenios->>+Browser: auth_url, belenios_url, client_id, state
    Browser->>+Auth: login(belenios_url, client_id, state)
    Auth->>+Browser: belenios_url, code, state
    Browser->>+Belenios: confirm_login(code, state)
    Belenios-->>+Auth: validate(client_id, client_secret, code)
    Auth-->>+Belenios: data
    Belenios->>+Browser: ok
```
