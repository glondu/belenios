#!/bin/bash

set -e

buffer=$(mktemp)
year=$(date +%Y)

while true; do
    vote=1
    while true; do
        filename=$(printf "vote_%03d_tally.txt" $vote)
        url="https://www.debian.org/vote/$year/$filename"
        echo "Downloading $url ..."
        if curl --silent --fail "$url" > $buffer; then
            mkdir -p $year
            cp $buffer $year/$filename
            ((vote++))
        else
            break
        fi
    done
    if [[ $vote -eq 1 ]]; then
        break
    else
        ((year--))
    fi
done

rm -f $buffer
