Using Debian vote data to test counting methods
===============================================

This directory provides tools to test Belenios counting functions with
[Debian vote data](https://www.debian.org/vote/).

The `download.sh` script downloads tally sheets of all Debian votes.

The `convert.ml` script (compiled as
`../../_build/tests/debian-votes/convert.byte`) reads on its standard
input a tally sheet and writes on its standard output the list of
ballots in the JSON format expected by Belenios.
