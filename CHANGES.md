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
