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
