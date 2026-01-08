3.2~dev
=======

 * Use belenios.net domain name for project website and contact
 * Command-line tool:
   + Add "archive pull"
 * Web server:
   + General election management
     - Add support for per-election logo
     - Remove direct download of full election archive (its content is
       accessible via the API and with "belenios-tool archive pull")
     - Make voter address optional
     - Add support for a grace period (requires an external
       authentication server implementing Belenios Connect)
   + Interoperability APIs
     - Add support for external send_message service
     - Add send-message endpoints to API
     - Add support for a new "Belenios Connect" authentication protocol
     - Document authentication protocols
     - Change and document billing protocol
     - Add an external credential authority protocol
   + Server administration
     - Add support for encoding recipient in bouncing address
     - Possibly add Feedback-ID header to outbound e-mails
     - Add possibility to add a description in `<auth-export>`
   + Administrator interface
     - Display number of registered voters in voter list tab
     - Display account-specific voters_limit if present
     - Marked up text must be valid XML and enclosed in a `<markup>`
       element
     - Display authentication method in profile
   + Voter interface
     - Add link to object in downloadable smart ballot tracker
     - Avoid using a new window for voter authentication
     - Fix localization of email authentication process
   + Miscellaneous
     - Add a link to email login e-mails
     - Update e-mail address when logging in with an existing account
 * Monitoring:
   + Randomize User-Agent in monitor_elections.py
 * Software stack:
   + Support ocaml 5.4.0, js_of_ocaml 6.2.0, yojson 3.0.0, atd 3.0.1
   + Base nspawn images on Debian 13 (trixie)
   + Regenerate `libsodium.wasm`

3.1 (2025-05-22)
================

 * Add a contributing policy
 * Software stack:
   + Support OCaml 5.3
   + Use `markup` for HTML parsing
   + Regenerate `libsodium.wasm`
   + Stop vendoring JSBN
   + Reduce usage of SJCL. Now, it is used only for compatibility with
     existing elections and will be removed in a future release.
   + Use @hello-pangea/dnd instead of deprecated react-beautiful-dnd
   + Document Debian Sid as the preferred developing environment
 * Add safety checks in vector encoding into group element for NH
   questions
 * Remove support for salt-based credentials (they have been obsolete
   since version 2.4), and old-style credentials (obsolete since 2.3)
 * Use AES-GCM for PKI operations (in threshold trustee setup)
 * Add a missing check of encrypted tally in `belenios-tool` and
   specification
 * Web server:
   + Notable bugfixes:
     - Allow setting publish date during tallying
     - Fix countdown to automatic opening/closing (GitHub issue #89)
     - Always provide a download attribute when linking to a data URI
     - Fix display bug in MJ booth with large grades and blank vote
     - Check the length of the public credentials list sent by
       registrar
   + Many changes in the web API
   + Add uuid to downloadable file names
   + Use username (when available) in "To" header of sent e-mails
   + Serve CSS files from a dedicated directory
   + Reset authentication cookie after a successful login
   + Replace `<br>` by ` | ` to avoid content layout abuse
   + Add support for `<a>` markup
   + Add an additional notion of sealing for validated elections,
     optionally enabled by a server configuration directive
   + Log sent OTP codes in access log
   + Admin UI:
     - In question editor, check safety
     - Handle better errors in voter list (GitLab issue #15)
     - Allow setting automatic dates for draft elections as well
     - Start elections in closed state
   + Trustee UI:
     - Do not force page reloads during threshold key generation
       protocol
     - Tolerate trailing white spaces in secret key file
   + Election homepage:
     - Add final event hash to audit box
     - Add links to objects in audit box
     - Change display of automatic opening/closing times
     - Show a meaningful page when the election is in Draft state
   + Translations:
     - Add Tamil, Russian

3.0 (2024-10-08)
================

 * Specification:
   + Add support for "lists" questions
   + Add signature to `trustee_public_key` in threshold mode
   + Embed the specification in squashfs images
 * Command-line tool:
   + Use unverified_ballots when computing encrypted tally, voters and
     summary
   + Use encrypted tally from archive when computing decryption and
     result
   + Use cohttp (instead of curl) for downloading election files
 * Web server:
   + Randomize account ids
   + Big refactoring of storage backend
   + Change cookie handling for consent and language preference
   + Election home page:
     - Revamp
     - Show only the public key of threshold trustees
     - Turn it into a SPA
     - Incorporate ballot box browsing and advanced mode
   + Booth:
     - Offer smart ballot tracker for download
     - Turn it into a SPA
     - Change in authentication workflow
   + Avoid credential being saved in browser history
   + New admin UI:
     - Many bugfixes and improvements to bring it in line with the
       classical admin UI
     - Add tests and continuous integration
   + Remove classical admin UI
   + Convert the trustee UIs (key generation, key check, partial
     decryption and shuffle) to a single page application (SPA)
   + Convert the credential authority UIs (credential generation) to a
     SPA
   + API:
     - Bump version to 5
     - Move "postpone" date to automatic dates and name it "publish"
       date
     - Many changes in endpoints to support SPAs
   + Tests and continuous integration:
     - Add automatic tests of the new admin UI
     - Add support for NH question and optional validation in scaling
     - Add automatic test of monitoring
   + Configuration:
     - Drop support for specifying groups by file
     - Remove (unused) security log
     - Add configurable vendor name
     - Replace `<prefix>` and `<rewrite-prefix>` by a single
       `<public-url>` tag
   + Documention:
     - Nspawn: do not drop capabilities needed by logrotate
     - Reverse-proxy: add a note about samesite=strict cookies
 * Add unshare scripts to create squashfs images

2.5.1 (2024-06-28)
==================

 * Strengthen the Pedersen DKG protocol used in threshold mode
 * Rework random generation to make it more easily explainable
 * Web server:
   + Fix administrator and credential authority public names
     (GitHub issue #85)
   + Fix import of passwords for voters with uppercase in login
     (GitLab issue #14)

2.5 (2024-03-08)
================

 * Web server:
   + Add documentation about cookie handling
   + More hardening in reverse-proxy documentation
   + Add a restricted mode
   + Make OpenID Connect and CAS authentications compatible with
     SameSite=Strict cookie policy

2.4 (2024-01-24)
================

 * Drop salts and directly use long enough credentials
 * Command-line tool:
   + Add parallel credential generation
 * Web server:
   + Add section in belenios-tool.html to partial decrypt buggy
     elections
   + Generate credentials on server asynchronously
   + Add a per-admin limit on number of voters
   + Add a domain blacklist for administrator addresses
   + Do not offer credentials for download when they are not ready
   + Send personal links in credential e-mails (GitHub issue #49)
   + Add a scaling test tool (using the HTTP API)
   + Replace breadcrumb with line-vertices progress bar
   + Add billing support
   + New admin UI:
     - Add language preference in account tab

2.3 (2023-09-30)
================

 * Update OCaml stack to 4.14.1 and base containers on Debian 12
 * Add individual salts in credential derivation. This changes the
   format of private credentials. The old format is still supported
   for existing elections, but new ones will use the new format.
 * Web server:
   + Improve consistency of texts (GitHub issue #75)
   + Send private credentials on draft validation

2.2 (2023-08-30)
================

 * Command-line tool:
   + Rename `--ballot` argument of `generate-ballot` to `--choice`
   + Add `verify-ballot` command
   + Add `--skip-ballot-check` mode for `verify` command
 * Web server:
   + Various optimizations for handling elections with around 30k
     voters
   + Remove classical booth
   + New booth:
     - Add support for STV questions (rank without equality)
     - Add basic support for generic questions (give a number to each
       choice)
   + Add possibility to insert an HTML snippet in the page that
     is shown to the voter after a successful vote
   + Add a new admin interface (experimental for now)
   + Automatically use fake credential in booth preview
   + Change location of compute-fingerprint service
   + Limit rate of password authentication attempts
   + Add a complexity check to password creation
   + Uniformize handling of one-time passwords
   + Allow download of archive.zip in Tallied state as well
   + Add demo dockerfile
 * Documentation:
   + Add reverse-proxy
   + Add MTA
 * Translations:
   + Add Japanese

2.1 (2023-02-28)
================

 * When generating credentials, output private credentials in JSON (as
   an object indexed by logins)
 * Add support for Ed25519 (elliptic curve) group and use it by default
 * Web server:
   + Add private key ownership check
   + Remove address input step in email authentication
   + Many changes in API, version bumped to 3
   + Pack JS files of the new booth
   + Pack CSS files
   + Add support for voters in JSON format
   + Interpret basic `<i>` and `<b>` markup (GitHub issue #66)

2.0 (2022-12-06)
================

 * Change the format of public data of running elections
 * Drop crypto v0
 * Optimize some exponentiations
 * In credential generation, generate also a mapping from public
   credentials to usernames, to be sent to the voting server
 * Ensure all public credentials are distinct
 * Web server:
   + Add a spool migration tool
   + Add a direct cast API (experimental)
   + Remove a confirmation step in the voting process
   + Add a SEALING service, GET-able by logged in administrators
   + Unify the style of all server-generated pages
   + Changes in API related to drafts and trustees
   + Add configurable LOGO and favicon.ico services
 * Command-line tool:
   + Add a system integrity (sealing) measurement command

1.20 (2022-10-20)
=================

 * The implementation of Condorcet-Schulze method was incorrect. It
   has been replaced with an adaptation of CIVS's `beatpath2`
   algorithm.
 * Web server:
   + Reduce lifetime of auth-related cookies
   + More trimming of various codes that are subject to copy-paste errors
   + Handle `<br>` in election metadata in more places
   + Ensure partial decryptions are in canonical form
   + Set max to 1 in default homomorphic question
   + Change format of links sent to credential authority and trustees
   + Limit the number of trials in OTPs
 * Command-line tool:
   + Big overhaul: many commands have been renamed and grouped by topic
   + `--group` now takes directly the group description
 * Monitoring:
   + Randomize order of HTTP requests in monitor_elections.py
 * Container:
   + Ensure /tmp is big enough when building container
   + Make the configuration read-only from inside the container

1.19 (2022-05-10)
=================

 * Update OCaml stack to 4.13.1 and base containers on Debian 11
 * Web server:
   + Add a configuration option to deny new elections
   + Avoid & in links sent via mailto (GitHub issue #37)
   + Add support for allowlist in auth config (GitHub issue #47)
   + Add possibility to pass credential in hash (GitHub issue #49)
   + Show pretty names in language selection boxes
   + Make bulk email (credentials and passwords) sending asynchronous
     and resumable
   + Make pages served to credential authority and trustees constant,
     and add them to monitoring
 * Monitoring:
   + More generic handling of translated pages and translation files
   + Check that old ballots are not replayed
   + Various fixes
 * Update instructions
 * Translations:
   + Add Lithuanian, Spanish (Latin America)

1.18 (2021-11-24)
=================

 * Specification:
   + Clarify the fact that some actions are done by the server and not
     the administrator
 * Web server:
   + Fix a deadlock issue with automatic open/close
   + Fix various case sensitivity issues in logins
   + Ensure the private key has been downloaded when submitting the
     public key
   + Check owner of source election when importing voters or trustees
   + New experimental REST API for administrative operations
     - Experimental single-page application for administrators using
       this API
   + Remove support for old-style (pre-1.11) trustees
   + Reduce static information on the admin voter page
   + Add a configuration option to deny revotes
   + Add possibility of multiple owners for an election
   + Issue an error when CAS has not been selected, but a server was
     given
   + Add support for CAS Protocol 2.0
   + Use new booth by default

1.17 (2021-09-07)
=================

 * Properly count and ignore blank votes in Condorcet-Schulze counting
   method
 * New version of cryptographic operations, fixing many (theoretical)
   weaknesses
   + This new version is called v1 and is used for new elections
   + The previous version, retroactively called v0, is kept for
     compatibility but will eventually be removed
 * Web server:
   + New voting interface for Condorcet votes
   + Build a belenios-server executable (with static linking of Eliom
     modules)
   + Add a notion of account for administrators; do no longer rely on
     the authentication method to identify them

1.16 (2021-07-19)
=================

 * Check secret credentials in belenios-tool (GitHub issue #31)
 * Optimize log computation during decryption
 * Majority Judgment: compute explicitly valid and blank votes
 * Web server:
   + Port to (cohttp-based) ocsigenserver 3.0.0
   + New voting interface for classic and Majority Judgment votes
   + Add a way to specify counting method of alternative questions
     (only Majority Judgment is supported at the moment)
   + Add possibility to customize email sender name (GitHub issue #30)
   + Add possibility to customize footer
   + The maximum total weight is now 10^11 (instead of 10^5)
   + `<br>` are interpreted as line breaks in election names,
     descriptions, questions and answers
   + Add possibility to localize configurable snippets
   + Better behaviour when sending a confirmation email fails
   + Many cosmetic changes in administrator's and voter's workflows

1.15 (2021-05-04)
=================

 * Update OCaml stack to 4.11.2
 * Improve support for weighted votes:
   + Update instructions and interface for editing voters
   + Update monitoring scripts
   + Update specification
 * Add documentation and scripts for deploying using systemd-nspawn
 * New format for private credentials
 * Web server:
   + Check consistency of voter list
   + Add a generic service for computing and comparing fingerprints
   + Remove "Archive election" button
   + Change sign up and password change workflows
   + New format for voter passwords
   + Admin home page now shows login form of the first auth system
   + Add possibility to export auth systems to election administrators
   + Add authentication by e-mail
   + Add possibility to remove built-in voter auth systems
   + Do no longer send automatic warning e-mails
   + Show cookie disclaimer only when logged in
   + Add possibility to customize admin home page
   + Rephrase e-mails sent to voters
   + Add a template e-mail to be sent to the credential authority
 * Translations:
   + Add Greek (el), Dutch (nl), Slovak (sk), Finnish (fi), Polish (pl)

1.14 (2021-02-09)
=================

 * Add experimental support for weighted votes: a weight can be
   assigned to a voter with the syntax "address,login,weight" or
   "address,,weight"

1.13 (2020-12-02)
=================

 * Update OCaml stack to 4.11.1
 * Handle mix of Single and Pedersen trustees thoughout codebase and
   specification
 * Add support to apply various counting methods to a set of ballots
   coming out of an non-homomorphic question, in the command-line tool
   and the web server: Condorcet-Schulze, Majority Judgment and Single
   Transferable Vote
 * Web server:
   + In threshold mode, add a mandatory server-owned key
   + Accept only ballots in canonical form
   + Do not allow change of trustee public key once one has been
     received
   + Add Belenios-* headers to all sent e-mails
 * Translations:
   + Switch i18n to gettext and use Weblate
   + Add support for translating the admin interface
   + Add Norwegian Bokmål (nb), Spanish (es), Ukrainian (uk),
     Czech (cs), Occitan (oc)
 * Tests:
   + Add monkey testing
 * Contributed scripts:
   + check_hash.py: use an external reference file with hashes
 * Switch the build system to dune
 * Drop support for old-style UUIDs

1.12 (2020-08-31)
=================

 * Web client:
   + Harden against browser extensions that pollute the global
     namespace
   + Use pristine sources of JavaScript libraries in the build process
 * Web server:
   + Do not allow to set credential authority name in automatic mode
   + Remove useless CalendarLib initialization (it was causing errors
     in some time zones)
 * Minor changes in tests and doc

1.11 (2020-05-25)
=================

 * Update OCaml/Eliom stack to 4.08.1/6.12.0
 * In `opam-bootstrap.sh`, enforce a specific revision of
   opam-repository for reproducibility
 * Switch to unified trustees: handle trustees with a single kind of
   file `trustees.json` instead of previously `public_keys.jsons` or
   `threshold.json`
 * Add monitoring scripts
 * Documentation:
   + Add _Who does what during a Belenios election?_ (in English and
     French)
 * Command-line tool:
   + Add `mktrustees` command to generate `trustees.json` from one
     of the two previous files
   + Add `checksums` command to compute all checksums relevant in
     auditing an election
   + In `credgen`, do no longer generate file with individual public
     credential hashes
   + Add `compute-voters` command to compute the voter list (to be
     executed by the credential authority)
   + Add `sha256-b64` command
 * Web client:
   + Use JavaScript's native BigInt when available
   + Import SJCL sources for DFSG compliance
 * Web server:
   + Fix a vulnerability in the authentication system
   + Add a temporary automatic migration procedure of an election pool
     to unified trustees
   + Add public names to the election administrator and the credential
     authority (they are stored in `election.json`)
   + Add public names to trustees (they are stored in `trustees.json`)
   + Record shuffling order in `result.json`
   + Remove the possibility to replace a credential in a validated
     election
   + Add (much) more auditing data to election home
   + Add the possibility to download private credentials in automatic
     mode
   + Voters must always log in to confirm their vote
   + Export `shuffles.jsons` during the shuffling phase, show applied
     shuffles even before the final result is released
   + For alternative questions, offer a direct link to results instead
     of giving a JS query
   + Allow administrators to log in with their email address
 * Tests:
   + Load testing support:
     - Add Selenium scripts to create and populate an election with
       many voters
   + Allow testing with a prepared database

1.10 (2019-12-09)
=================

 * Add support for non-homomorphic questions (experimental)
 * Check group membership of signature verification key more often
 * Command-line tool:
   + Assume there is no ballots when `ballots.jsons` is missing
 * Web server:
   + Move setting of maxrequestbodysizeinmemory to configuration
   + Rework presentation of links that must be sent to third-parties
   + Bugfixes in the data policy loop:
     - its first iteration was done with the wrong spool dir
     - it died when trying to send warning e-mails
   + Update JSBN
   + Importing non-threshold trustees replaces current trustees

1.9.1 (2019-10-24)
==================

 * Specification:
   + Link to Meadows instead of eprint (easier to read)
   + Be more verbose about checks to do during the election
 * Web server:
   + Avoid error 500 on "accepted ballots" page when no ballots have
     been cast
   + Trim usernames and passwords before checking them
   + Trim and check CAS server addresses
   + Case-insensitive comparison of usernames
   + Set a limit on election names to prevent abuse
   + Protect third-party pages (creds, trustee) from authenticated
     users
   + Avoid error 500 when attempting to authenticate several times in
     a row
 * Use opam 2.0.5 in bootstrap script

1.9 (2019-05-28)
================

 * Fix use of SOURCE_DATE_EPOCH
 * Web server:
   + Fix a bug that seldom caused the server to not perform its
     partial decryption
   + Check that cookies are not blocked on ballot submission
   + Add the possibility to temporarily hide the result from the
     public

1.8 (2019-02-04)
================

 * Add the possibility to override sendmail via an environment variable
 * Use SOURCE_DATE_EPOCH if available
 * Use opam 2.0.0 in bootstrap script
 * Web server:
   + Add some automated tests
   + Add the possibility to create administrator accounts
   + Add booth preview
   + Add automatic open / close dates
   + Unhide support for threshold decryption
   + Fixed a bug that caused some elections to not appear in the
     administrator's listing when the election pool is big
   + Force the server to be a trustee in basic mode
   + Record in trustee public keys whether the server has the private key

1.7.1 (2018-12-05)
==================

 * Do not output spurious empty lines in records file (bugfix: voting
   records and missing voters were not working)
 * More explicit checklist in election validation page
 * Avoid sending password/credential emails when name has not been
   edited
 * Avoid hidden parameters in some services that are meant to be usable
   from non-web clients

1.7 (2018-11-26)
================

 * Add automatic data archival/deletion policy
 * Do not allow election validation if some items have not been edited
 * Trustees can load their private key from a file
 * Do no longer rely on Ocsipersist
 * Port to OCaml 4.06.1 and Eliom 6.3.0
 * Re-seed LwtRandom prng every 30 minutes
 * Add a placeholder for warnings/announcements

1.6 (2018-06-13)
================

 * Add (optional) contact info in emails sent by the server
 * Use base 58 tokens as UUIDs for shorter URLs (optional)
 * Add (optional) return path to mails sent by server
 * Show personal data processing notice to election administrators
 * Fix password regeneration when explicit usernames are used
 * Make the booth independent of the server and usable offline
 * Internationalize error messages

1.5 (2017-12-13)
================

 * Add support for threshold decryption (experimental)
 * Fix bias in random sampling
 * Web server:
   + Add possibility to define the server e-mail address in config
   + Add possibility to explicitly add the server itself as a trustee
   + Add possibility to destroy elections in setup mode
   + Avoid new tabs and use download links
   + Add config option for "contact us" link on admin login page

1.4 (2017-04-05)
================

 * Add a debug mode, which has the possibility to use /dev/urandom as
   source of entropy
 * Check encrypted tally in "belenios-tool verify"
 * Add a sample script to send credentials
 * Web server:
   + Introduce a limit on the number of mails sent at once. This
     effectively limits the number of voters in the general case.
   + Give a link to the future election to the credential authority and
     trustees
   + For each mailto template, add a direct link. This makes life easier
     for situations where complex mailto links are not supported.

1.3 (2017-02-01)
================

 * Add support for blank votes
 * More diagnostics in verify-diff
 * Web server:
   + Do not log out of CAS
   + Automatically log out after a vote
   + Add Italian translation

1.2 (2016-10-05)
================

 * Change the default group parameters to avoid possible
   trapdoors. The new ones are generated using FIPS 186-4.
 * Web server:
   + The administrator can choose the language(s) of mails sent by the
     server
   + The administrator can import trustees from a previous election
   + Question editor: it is now possible to insert and remove
     questions and answers anywhere
   + Add Romanian translation
 * Command-line tool:
   + Add --url option to election subcommands (in particular verify)
   + Add a "verify-diff" command to belenios-tool

1.1 (2016-07-25)
================

 * Web server:
   + Internationalization of voter-facing interfaces
     - add French and German translations
   + Add a confirmation page for election finalization
   + Add cookie disclaimer
   + Add templates for mails to trustees
   + Add the Belenios logo and use www.belenios.org in links
   + Add OpenID Connect authentication for administrators
 * Command-line tool:
   + Issue a proper warning when a result is missing
   + Support result files where decryption factors are not in the same
     order as trustee public keys

1.0 (2016-04-22)
================

 * Many changes in the web server:
   + Add election_missing_voters: it is now possible to see the list of
     people who did not vote (new link in election administration
     page).
   + Hide the login box when it is not relevant: We do no longer show
     login links in the top right hand corner of the page. The voter
     is automatically invited to log in when he is about to cast a
     vote.
   + Do no longer show warning when window.crypto is unavailable (this
     warning appeared on IE8).
   + In admin page, show tallied elections in a new section.
   + In admin page, sort (finalized) elections by finalization time.
   + Add a form to regenerate and mail a password.
   + Generating trustee keys is more resilient to momentary lack of
     entropy.
   + Change default question to make the blank choice explicit.
   + Print number of accepted ballots on the result page.
   + Add the possibility to specify a login attached to an email
     address. E-mail address and logins must be specified in the
     following way: foo@example.com,login. When login is not
     specified, the address is used as login. This feature is useful
     mainly for CAS authentication.
   + Voters (and passwords) can be imported from another (finalized)
     election.
   + Send a confirmation email after a successful vote.
   + Add a new notion of "archived" elections.
   + Pretty page for records.
   + An e-mail address can be attached to trustees.
   + Do not propose dummy authentication for new elections.

0.2 (2014-04-09)
================

 * Major overhaul of the web server:
   + changes in configuration items
   + cleaner isolation between elections
   + add per-site and per-election administration pages
   + elections imported from the configuration file must be explicitly
     listed (no more directory scanning)
   + authentication is more modular
   + changes in CAS authentication method:
     - invoke credential requestor with `renew=true`
     - do not assume CAS paths start with `/cas/`
   + change in the password authentication method:
     - the password file must be uploaded via the web server (no more
       reading on-disk file) before the method is used for the first
       time
   + automatic logout after successful ballot casting
   + online creation of election
 * Remove hardcoded default group

0.1.1 (2014-02-13)
==================

 * New subcommands in belenios-tool: "mkelection" and "election vote"
 * Add a demo (bash) script to simulate a whole election
 * Prettier URLs for election pseudo-files
 * Fix compatibility with reverse-proxies

0.1 (2014-01-13)
================

 * First public release
