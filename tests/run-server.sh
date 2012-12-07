#!/bin/sh

RUNDIR=${SELENIOS_RUNDIR:-_run}

mkdir -p \
  $RUNDIR/etc \
  $RUNDIR/log \
  $RUNDIR/lib \
  $RUNDIR/run

sed "s@_RUNDIR_@$RUNDIR@g" tests/ocsigenserver.conf.in > $RUNDIR/etc/ocsigenserver.conf

if [ ! -d _build ]; then
  echo "This script should be run from the root of the source tree!"
  exit 1
fi

ocsigenserver -c $RUNDIR/etc/ocsigenserver.conf "$@"
