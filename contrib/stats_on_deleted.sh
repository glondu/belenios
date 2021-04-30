#!/bin/bash

set -e

SPOOL="$1"

usage () {
    echo "Usage: $0 SPOOL"
    echo "Computes some statistics on deleted elections in SPOOL."
    exit 1
}

if ! [ -d "$SPOOL" ]; then
    usage
fi

total_elections=0
total_voters=0
total_ballots=0
dates=""

for u in "$SPOOL"/*/deleted.json; do
    echo "Processing $u ..."
    eval $(jq --raw-output .nb_voters,.nb_ballots,.date < $u | {
               read nb_voters
               read nb_ballots
               read date
               echo "let total_voters+=$nb_voters total_ballots+=$nb_ballots 1;"
               echo "date=\"$date\""
           })
    let ++total_elections
    if [ -z "$dates" ]; then
        dates="$date"
    else
        dates="$(printf "%s\n%s" "$dates" "$date")"
    fi
done

dates="$(echo "$dates" | sort)"
first_election="$(echo "$dates" | head -n1)"
last_election="$(echo "$dates" | tail -n1)"

echo "Number of elections: $total_elections"
echo "Number of voters: $total_voters"
echo "Number of ballots: $total_ballots"
echo "First election: $first_election"
echo "Last election: $last_election"
