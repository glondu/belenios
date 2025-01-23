#!/bin/sh

set -e

if [ "$1" = "--preload" ]; then
    shift
    . "$1"
    shift
fi

if [ -e .git ]; then
    : ${BELENIOS_CONFIG:=demo/ocsigenserver.conf.in}
    : ${BELENIOS_VARDIR:=_run}
    : ${BELENIOS_RUNDIR:=/tmp/belenios}
    : ${BELENIOS_BINDIR:=_run/usr/bin}
    : ${BELENIOS_SHAREDIR:=_run/usr/share/belenios-server}
fi

check_nonempty_var () {
    if eval [ -z "\$$1" ]; then
        echo "$1 must be set!"
        exit 1
    fi
}

check_nonempty_var BELENIOS_CONFIG

if [ -f ${BELENIOS_CONFIG}.preload ]; then
   . ${BELENIOS_CONFIG}.preload
fi

check_nonempty_var BELENIOS_VARDIR
check_nonempty_var BELENIOS_RUNDIR
check_nonempty_var BELENIOS_BINDIR
check_nonempty_var BELENIOS_SHAREDIR

mkdir -p \
      $BELENIOS_VARDIR/etc \
      $BELENIOS_VARDIR/log \
      $BELENIOS_VARDIR/lib \
      $BELENIOS_VARDIR/upload \
      $BELENIOS_VARDIR/accounts \
      $BELENIOS_RUNDIR

if ! [ -d $BELENIOS_VARDIR/spool ]; then
    mkdir $BELENIOS_VARDIR/spool
    echo 1 > $BELENIOS_VARDIR/spool/version
fi

touch $BELENIOS_VARDIR/password_db.csv

sed \
    -e "s@_VARDIR_@$BELENIOS_VARDIR@g" \
    -e "s@_RUNDIR_@$BELENIOS_RUNDIR@g" \
    -e "s@_SHAREDIR_@$BELENIOS_SHAREDIR@g" \
    $BELENIOS_CONFIG > $BELENIOS_VARDIR/etc/ocsigenserver.conf

PATH=$BELENIOS_BINDIR:$PATH:/usr/sbin

OCSIGENSERVER=belenios-server

exec $OCSIGENSERVER -c $BELENIOS_VARDIR/etc/ocsigenserver.conf "$@"
