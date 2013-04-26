Legacy test data
================

This directory contains data collected by running fake elections with
the reference implementation.

There is one directory per election. The typical content of a
directory is:

 * `public_keys.jsons`: one line per public key share, as it is
   submitted by a trustee.

 * `private_keys.jsons`: one line per private key share, as it is
   saved by a trustee, and pasted during the tally.

 * `election.json`: the election parameters, as they are sent to the
   booth. This is what is returned by the `/helios/elections/$UUID`
   path in the reference implementation.

 * `ballots.jsons`: one line per submitted ballot, as it is sent by
   the booth when the voter confirms.

 * `encrypted_tally.json`: the encrypted tally, as it is sent to the
   trustees at the end of the election.

 * `partial_decryptions.jsons`: one line per partial decryption, as it
   is sent by a trustee.

 * `result.json`: this the result returned by the
   `/helios/election/$UUID/result` path in the reference
   implementation.

Trustee-related `.jsons` files have one line per trustee, in the same
order. Data were collected in situ, using the [TamperData][1] Firefox
extension.

[1]: http://tamperdata.mozdev.org/
