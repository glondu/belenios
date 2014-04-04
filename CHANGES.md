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
