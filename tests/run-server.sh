#!/bin/sh

if [ ! -d _build ]; then
  echo "This script should be run from the root of the source tree!"
  exit 1
fi

RUNDIR=${SELENIOS_RUNDIR:-_run}

mkdir -p \
  $RUNDIR/etc \
  $RUNDIR/log \
  $RUNDIR/lib \
  $RUNDIR/run

sed \
  -e "s@_RUNDIR_@$RUNDIR@g" \
  -e "s@_SRCDIR_@$PWD@g" \
  tests/ocsigenserver.conf.in > $RUNDIR/etc/ocsigenserver.conf

ocsigenserver -c $RUNDIR/etc/ocsigenserver.conf "$@"
