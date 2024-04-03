2.6
===

 * Change in configuration file: `<uuid>`, `<spool>` and `<accounts>`
   elements are now children of a new `<storage>` element (see example
   in `demo/ocsigenserver.conf.in`)

2.2
===

 * If nspawn-based containers are used, the `belenios-nspawn` script
   should be updated on the host.

2.0
===

 * The spool format has changed in an incompatible way. A migration
   tool, `belenios-migrate`, is provided to convert from the 1.20
   format. It expects a matching `belenios-tool` in the same
   directory, and an `OCAMLFIND_CONF` environment variable set to the
   path of a `findlib.conf` file (that can be copied as is from the
   OPAM directory). To update an existing instance:
   + stop the instance
   + make a backup of the spool directory
   + run `belenios-migrate` with the path to the spool directory
     (make sure permissions are correct)
   + perform the rest of the update as usual

1.20
====

 * If you use the scripts in `doc/nspawn`, you must:
   + update `belenios-nspawn` in your deployment environment
   + in the `/srv/belenios-containers/$NAME` directory:
     - `mv belenios belenios-var`
     - `mkdir -p belenios/etc`
     - `chown -R 1000:1000 belenios`
     - `mv belenios-var belenios/var`
     - `mv belenios/var/ocsigenserver.conf.in belenios/etc`
     - move other relevant files from `belenios/var` to `belenios/etc`

1.17
====

 * To use the `belenios-server` executable, the syntax of
   `ocsigenserver.conf.in` changes. Please review the changes to this
   file since version 1.16.
 * With the new version of the crypto, giving all parameters of a
   group with an external file is no longer supported. Allowed groups
   are hardcoded in the source code and identified by short strings
   such as `BELENIOS-2048` or `RFC-3526-2048`. These strings must now
   be used in the configuration file.
 * The new notion of administrator accounts:
   + adds a new `accounts` directory, configured in
     `ocsigenserver.conf.in`
   + changes the format of the `owner` field of `draft.json`,
     `metadata.json` and `deleted.json`, which is now the account id
     (an integer) for new elections. The old format based on the
     authentication method will continue to be supported for a while,
     but this support may be dropped in the future.

1.15
====

 * All authentication systems available for voters must be explicitly
   listed in the configuration file with the new `<auth-export>`
   directive. Look at `demo/ocsigenserver.conf.in` for examples. In
   particular, password and (generic) CAS authentications are not shown
   by default.

1.11
====

 * The switch to unified trustees changed:
   + the format of the pool: instead of one of `public_keys.jsons` or
     `threshold.json`, only a single `trustees.json` is expected. The
     spool will be automatically converted during the first run of
     this version of the web server. Next versions will only support
     the new scheme.
   + the format of `deleted.json` files: `nb_trustees` and
     `trustees_threshold` fields have been replaced by a new
     `trustees` field reflecting `trustees.json` structure. No
     provisions were made to convert existing files.

1.7
===

 * To upgrade a web server running version 1.6, you need to delete the
   Ocsipersist store (by default the `ocsidb` file referred in the
   configuration file). This will archive all validated elections, and
   delete all draft elections. Additionally, you should clean up the
   data directory (the one referred in the `<spool>` directive in the
   configuration file) by removing all temporary files (run `rm *.*`
   in this directory) and private keys (`rm */private_key*.json*`).

1.1
===

 * To upgrade a web server running version 1.0, you need to delete the
   Ocsipersist store (by default the `ocsidb` file referred in the
   configuration file). This will archive all finalized elections, and
   delete all unfinalized elections (i.e. the elections being
   prepared). Additionally, you should clean up the data directory (the
   one referred in the `<spool>` directive in the configuration file)
   by removing all temporary files (run `rm *.*` in this directory)
   and private keys (`rm */private_key.json`).
