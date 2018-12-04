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
