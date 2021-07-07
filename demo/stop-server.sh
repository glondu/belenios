#!/bin/sh

set -e

if [ "$1" = "--preload" ]; then
    shift
    . "$1"
    shift
fi

if [ -e .git ]; then
    : ${BELENIOS_RUNDIR:=/tmp/belenios}
fi

echo shutdown 10 > $BELENIOS_RUNDIR/ocsigenserver_command
