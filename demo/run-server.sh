#!/bin/sh

if [ ! -d _build ]; then
  echo "This script should be run from the root of the (built) source tree!"
  exit 1
fi

BELENIOS_RUNDIR=${BELENIOS_RUNDIR:-_run}
BELENIOS_TMPDIR=${BELENIOS_TMPDIR:-/tmp/belenios}
OCAML_STDLIBDIR=$(ocamlc -where)

mkdir -p \
  $BELENIOS_RUNDIR/etc \
  $BELENIOS_RUNDIR/log \
  $BELENIOS_RUNDIR/lib \
  $BELENIOS_RUNDIR/upload \
  $BELENIOS_RUNDIR/spool \
  $BELENIOS_TMPDIR/run

sed \
  -e "s@_OCAML_STDLIBDIR_@$OCAML_STDLIBDIR@g" \
  -e "s@_TMPDIR_@$BELENIOS_TMPDIR@g" \
  -e "s@_RUNDIR_@$BELENIOS_RUNDIR@g" \
  -e "s@_SRCDIR_@$PWD@g" \
  demo/ocsigenserver.conf.in > $BELENIOS_RUNDIR/etc/ocsigenserver.conf

ocsigenserver -c $BELENIOS_RUNDIR/etc/ocsigenserver.conf "$@"
