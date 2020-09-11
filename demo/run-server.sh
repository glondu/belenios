#!/bin/sh

set -e

if [ -d _build ]; then
    : ${BELENIOS_CONFIG:=demo/ocsigenserver.conf.in}
    : ${BELENIOS_VARDIR:=_run}
    : ${BELENIOS_RUNDIR:=/tmp/belenios}
    : ${BELENIOS_LIBDIR:=_run/usr/lib}
    : ${BELENIOS_STATICDIR:=_run/usr/share/belenios-server}
    : ${BELENIOS_GROUPDIR:=files/groups}
fi

check_nonempty_var () {
    if eval [ -z "\$$1" ]; then
        echo "$1 must be set!"
        exit 1
    fi
}

check_nonempty_var BELENIOS_CONFIG
check_nonempty_var BELENIOS_VARDIR
check_nonempty_var BELENIOS_RUNDIR
check_nonempty_var BELENIOS_LIBDIR
check_nonempty_var BELENIOS_STATICDIR
check_nonempty_var BELENIOS_GROUPDIR

mkdir -p \
      $BELENIOS_VARDIR/etc \
      $BELENIOS_VARDIR/log \
      $BELENIOS_VARDIR/lib \
      $BELENIOS_VARDIR/upload \
      $BELENIOS_VARDIR/spool \
      $BELENIOS_RUNDIR

touch $BELENIOS_VARDIR/password_db.csv

sed \
    -e "s@_VARDIR_@$BELENIOS_VARDIR@g" \
    -e "s@_RUNDIR_@$BELENIOS_RUNDIR@g" \
    -e "s@_LIBDIR_@$BELENIOS_LIBDIR@g" \
    -e "s@_STATICDIR_@$BELENIOS_STATICDIR@g" \
    -e "s@_GROUPDIR_@$BELENIOS_GROUPDIR@g" \
    $BELENIOS_CONFIG > $BELENIOS_VARDIR/etc/ocsigenserver.conf

OCSIGENSERVER=ocsigenserver

if command -v ocsigenserver.opt > /dev/null; then
  OCSIGENSERVER=ocsigenserver.opt
fi

exec $OCSIGENSERVER -c $BELENIOS_VARDIR/etc/ocsigenserver.conf "$@"
