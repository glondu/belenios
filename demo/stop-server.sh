#!/bin/sh

set -e

if [ "$1" = "--preload" ]; then
    shift
    . "$1"
    shift
fi

if [ -e _run ]; then
    : ${BELENIOS_RUNDIR:=_run/run}
else
    : ${BELENIOS_RUNDIR:=/run/belenios}
fi

echo shutdown 10 > $BELENIOS_RUNDIR/ocsigenserver_command
