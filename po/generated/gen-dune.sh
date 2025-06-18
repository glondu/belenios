#!/bin/bash

COMPONENT="$1"
LANGS=""

while read LANG; do
    LANGS="$LANGS $LANG"
done

for LANG in $LANGS; do
    echo "(rule
 (target $LANG.mo)
 (deps ../../$COMPONENT/$LANG.po)
 (action (run ocaml-gettext --action compile --compile-output %{target} %{deps})))
"
    echo "(rule
 (target $LANG.json)
 (deps $LANG.mo)
 (action (with-stdout-to %{target} (run ../../../src/scripts/mo2json/mo2json.exe %{deps}))))
"
done
